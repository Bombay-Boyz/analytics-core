-- | Analytics.Core.Plugin
--
-- The plugin contract: the 'Plugin' typeclass, validated submission types,
-- and the 'PluginContext' interface through which plugins interact with the
-- runtime.
--
-- Design invariants:
--
--   * This module defines the /contract/, not the /orchestration/.
--     Lifecycle events ('EvPluginLoaded', etc.) are emitted by the runtime
--     layer (Runtime.hs), not here. This keeps Plugin free of the Runtime
--     import and avoids the Plugin → Runtime → Plugin cycle.
--
--   * 'PluginError' lives in 'Analytics.Core.Event.Types' (not here) because
--     'Event.Types' needs it for 'EvPluginFailed', and Plugin imports
--     'Event.Types'. Defining it here would create the
--     Plugin → Event.Types → Plugin cycle. Plugin re-exports it for
--     callers that only import this module.
--
--   * Namespace ownership is a compile-time-enforced invariant. The
--     'PluginContext' callback for fact submission ('pcSubmitFact') accepts
--     only 'Fact 'NormalFact' values whose type is prefixed with the plugin's
--     own 'PluginId'. 'validatePluginFact' enforces this before the IO call
--     is ever made, returning a structured 'PluginSubmissionError' rather than
--     a runtime exception or a silent discard.
--
--   * Rules submitted via 'pcRegisterRule' must carry the submitting plugin's
--     'PluginId' in their 'rulePluginId' field. 'validatePluginRule' checks
--     this, preventing a plugin from registering rules that fire under
--     another plugin's namespace.
--
--   * The 'Plugin' typeclass has exactly the methods the runtime needs to
--     drive the lifecycle. Nothing more. Implementation details (internal
--     state, caches, config) live in the concrete @p@ type.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Rule
--   Analytics.Core.Event.Types   (PluginError, EventFilter, Event, re-exported)
--   ← this module

module Analytics.Core.Plugin
  ( -- * Plugin typeclass
    Plugin(..)

    -- * Plugin context (runtime → plugin interface)
  , PluginContext(..)

    -- * Validated submission wrappers
  , PluginFact(..)
  , PluginRule(..)

    -- * Submission validation
  , PluginSubmissionError(..)
  , validatePluginFact
  , validatePluginRule

    -- * Plugin registration record
  , PluginRegistration(..)
  , mkPluginRegistration

    -- * Re-exports (callers need not import Event.Types for these)
  , PluginError(..)
  , EventFilter(..)
  , SubscriptionId(..)
  , Event(..)
  ) where

import Relude
import Control.Concurrent.MVar (modifyMVar)

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  , verifyFactNamespace
  )
import Analytics.Core.Rule
  ( Rule(..)
  , RuleValidationError
  , validateRule
  )
import Analytics.Core.Event.Types
  ( PluginError(..)
  , EventFilter(..)
  , SubscriptionId(..)
  , Event(..)
  )

-- ---------------------------------------------------------------------------
-- Plugin typeclass
--
-- Minimal lifecycle contract. The runtime calls these methods in order:
--
--   1. 'pluginId'      — identify the plugin and claim its namespace.
--   2. 'apiVersion'    — version-check against the runtime's expected API.
--   3. 'initialize'    — set up the plugin; receive the 'PluginContext'.
--   4. 'shutdown'      — release resources; called on runtime shutdown or
--                        explicit plugin unload.
--
-- Method ordering is a runtime concern; the typeclass makes no assumptions
-- about call order. Implementations must be idempotent on 'shutdown' —
-- the runtime may call it more than once (e.g. on crash recovery).

class Plugin p where

  -- | The plugin's unique identifier. Must be a valid 'PluginId' (alphanumeric
  -- plus hyphens/dots; see 'mkPluginId'). This value is used as the namespace
  -- prefix for all facts and rules submitted by the plugin.
  --
  -- Invariant: 'pluginId' is pure and returns the same value for the lifetime
  -- of the plugin instance. The runtime caches it after the first call.
  pluginId :: p -> PluginId

  -- | The API version this plugin was compiled against. The runtime compares
  -- this against its own version; a mismatch produces
  -- 'PluginApiVersionMismatch' and the plugin is rejected without calling
  -- 'initialize'.
  apiVersion :: p -> Version

  -- | Initialise the plugin. Called exactly once after version and namespace
  -- checks pass. The 'PluginContext' gives the plugin its submission
  -- callbacks and event subscription handle for the lifetime of this run.
  --
  -- Return 'Left' to signal a fatal initialisation failure; the runtime
  -- emits 'EvPluginFailed' with 'PluginInitFailed' and does not call
  -- 'shutdown'.
  --
  -- 'initialize' MUST NOT call 'pcSubmitFact' or 'pcRegisterRule' — the
  -- runtime is not yet ready to accept submissions. Use 'postInitialize' for
  -- seed data.
  initialize :: p -> PluginContext -> IO (Either PluginError p)

  -- | Called immediately after a successful 'initialize' to let the plugin
  -- submit its seed facts and rules. The runtime is fully ready at this point.
  --
  -- Default implementation: no-op (pure plugin with no seed data).
  postInitialize :: p -> PluginContext -> IO (Either PluginError p)
  postInitialize p _ = pure (Right p)

  -- | Tear down the plugin. Called on orderly shutdown or explicit unload.
  -- Must be idempotent: a second call after a successful first call should
  -- succeed silently.
  --
  -- 'shutdown' MUST NOT call 'pcSubmitFact' or 'pcRegisterRule'.
  shutdown :: p -> IO ()

-- ---------------------------------------------------------------------------
-- PluginContext — runtime-issued capability record
--
-- The runtime constructs a 'PluginContext' for each loaded plugin and passes
-- it to 'initialize'. The context is the /only/ channel through which a
-- plugin submits facts, registers rules, and subscribes to events.
--
-- All callbacks are bound to the plugin's 'PluginId'. A plugin cannot
-- obtain a context for a different plugin's namespace — the callbacks are
-- closures, not typeclass methods. This is an object-capability design:
-- possession of a 'PluginContext' is proof of the right to use it.

data PluginContext = PluginContext
  { pcPluginId :: !PluginId
    -- ^ The namespace this context is bound to. Read-only; for validation
    -- diagnostics. All callbacks already capture this value.

  , pcSubmitFact :: !(Fact 'NormalFact -> IO (Either PluginSubmissionError SnapshotId))
    -- ^ Submit a fact into the knowledge base.
    -- The runtime validates namespace ownership before insertion:
    -- 'factRecordType' must be prefixed with 'pcPluginId'.
    -- On success, returns the 'SnapshotId' stamped at insertion time.
    -- Inference runs asynchronously; subscribe to 'EvInferenceCompleted'
    -- to observe derived consequences.

  , pcRegisterRule :: !(Rule -> IO (Either PluginSubmissionError SnapshotId))
    -- ^ Register a rule into the knowledge base.
    -- The rule's 'rulePluginId' must equal 'pcPluginId' and the rule must
    -- pass 'validateRule'. Returns the 'SnapshotId' at registration time.

  , pcSubscribe :: !(EventFilter -> (Event -> IO ()) -> IO SubscriptionId)
    -- ^ Subscribe to runtime events. The returned 'SubscriptionId' is
    -- required to cancel via 'pcUnsubscribe'.

  , pcUnsubscribe :: !(SubscriptionId -> IO ())
    -- ^ Cancel a previously registered subscription. Idempotent.
  }

-- ---------------------------------------------------------------------------
-- PluginFact / PluginRule — validated submission wrappers
--
-- These newtypes carry a proof that namespace validation has already been
-- performed. The 'pcSubmitFact' and 'pcRegisterRule' callbacks accept the
-- raw types for convenience; these wrappers exist for callers that want to
-- validate eagerly (e.g. to surface errors before entering IO).

-- | A 'Fact 'NormalFact' that has been validated to lie within the
-- submitting plugin's namespace. Construct with 'validatePluginFact'.
newtype PluginFact = PluginFact { unPluginFact :: Fact 'NormalFact }
  deriving stock (Show)

-- | A 'Rule' that has been validated to carry the correct 'rulePluginId'
-- and pass 'validateRule'. Construct with 'validatePluginRule'.
newtype PluginRule = PluginRule { unPluginRule :: Rule }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- PluginSubmissionError
--
-- All structured failure modes for plugin submissions. No bare 'Text'
-- catch-all; every constructor carries a typed payload. The runtime wraps
-- these in 'EvFactRejected' / 'EvPluginRejected' as appropriate.

data PluginSubmissionError
  = PSENamespaceMismatch !PluginId !Text
    -- ^ (ownerPlugin, actualFactType): the fact's type prefix does not match
    -- the submitting plugin's namespace. Prevents namespace squatting.
  | PSERuleOwnerMismatch !PluginId !PluginId
    -- ^ (contextPlugin, rulePluginId): the rule's 'rulePluginId' does not
    -- match the plugin context it was submitted through.
  | PSEInvalidRule !(NonEmpty RuleValidationError)
    -- ^ The rule failed 'validateRule' (e.g. unbound conclusion variable).
  | PSEShuttingDown
    -- ^ The runtime is shutting down; submissions are no longer accepted.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Validation — pure, total, no IO
--
-- Both functions return structured errors so callers can decide whether to
-- log, retry with a corrected value, or propagate to the user.

-- | Validate that a fact's type is prefixed with the given 'PluginId'.
-- Returns 'Right (PluginFact f)' on success, 'Left PSENamespaceMismatch'
-- on failure. Pure and total.
validatePluginFact
  :: PluginId
  -> Fact 'NormalFact
  -> Either PluginSubmissionError PluginFact
validatePluginFact pid f =
  case verifyFactNamespace pid f of
    Right ()  -> Right (PluginFact f)
    Left  msg -> Left (PSENamespaceMismatch pid msg)

-- | Validate that a rule's 'rulePluginId' matches the given 'PluginId' and
-- that the rule passes structural validation ('validateRule').
-- Returns 'Right (PluginRule r)' on success; structured errors on failure.
validatePluginRule
  :: PluginId
  -> Rule
  -> Either PluginSubmissionError PluginRule
validatePluginRule pid r
  | rulePluginId r /= pid =
      Left (PSERuleOwnerMismatch pid (rulePluginId r))
  | otherwise =
      case validateRule r of
        Left  errs -> Left (PSEInvalidRule errs)
        Right ()   -> Right (PluginRule r)

-- ---------------------------------------------------------------------------
-- PluginRegistration — the runtime-side record
--
-- When the runtime loads a plugin it builds a 'PluginRegistration' that
-- captures the plugin's declared metadata alongside the callbacks it will
-- use to drive the lifecycle. This record is what Runtime stores in its
-- plugin registry.
--
-- 'PluginRegistration' is /not/ part of the 'Plugin' typeclass; it is
-- constructed by the runtime from a 'Plugin p' instance. This separation
-- means the runtime can store heterogeneous plugins without a type-level
-- list — all plugin identity is erased into the 'PluginId' and the IO
-- callbacks once registration is complete.

data PluginRegistration = PluginRegistration
  { prPluginId    :: !PluginId
    -- ^ Claimed namespace. Uniqueness is enforced by the runtime at
    -- registration time ('PluginNamespaceConflict' if already taken).
  , prApiVersion  :: !Version
    -- ^ Declared API version. Compared against the runtime's supported
    -- version before 'initialize' is called.
  , prInitialize  :: !(PluginContext -> IO (Either PluginError ()))
    -- ^ Wrapped 'initialize' callback. Returns unit on success — the
    -- concrete @p@ is kept inside the closure; the runtime never sees it.
  , prPostInit    :: !(PluginContext -> IO (Either PluginError ()))
    -- ^ Wrapped 'postInitialize' callback.
  , prShutdown    :: !(IO ())
    -- ^ Wrapped 'shutdown' callback.
  }

-- | Construct a 'PluginRegistration' from any 'Plugin' instance.
-- The concrete type @p@ is erased into IO callbacks; the runtime does not
-- need to know @p@ after this point.
--
-- The returned registration holds an 'MVar' over @p@ so that:
--   * State changes from 'initialize' and 'postInitialize' are visible to
--     'shutdown'.
--   * Concurrent calls to the same callback are serialised rather than racing.
--   * On a Left result from 'initialize' or 'postInitialize', the MVar is
--     restored to the pre-call value, leaving the plugin in its last known
--     good state for a subsequent 'shutdown' call.
mkPluginRegistration :: Plugin p => p -> IO PluginRegistration
mkPluginRegistration p = do
  ref <- newMVar p   -- MVar replaces IORef; concurrent calls serialise, not race
  pure PluginRegistration
    { prPluginId   = pluginId p
    , prApiVersion = apiVersion p
    , prInitialize = \ctx ->
        modifyMVar ref $ \current -> do
          result <- initialize current ctx
          pure $ case result of
            Left  err -> (current, Left err)   -- roll back on failure
            Right p'  -> (p',      Right ())
    , prPostInit   = \ctx ->
        modifyMVar ref $ \current -> do
          result <- postInitialize current ctx
          pure $ case result of
            Left  err -> (current, Left err)   -- roll back on failure
            Right p'  -> (p',      Right ())
    , prShutdown   = readMVar ref >>= shutdown
    }