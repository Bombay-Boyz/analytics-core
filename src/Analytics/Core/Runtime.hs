-- | Analytics.Core.Runtime
--
-- The orchestration layer. Runtime owns:
--
--   * The 'KBHandle ReadWrite' — the authoritative mutable knowledge base.
--   * The 'EventBus' — subscribers observe but cannot publish.
--   * A dedicated inference thread — woken by a dirty flag whenever the KB
--     changes, debounced, then run to fixed point.
--   * A 'ContradictionRegistry' — predicates registered by plugins; checked
--     after each inference cycle.
--
-- Architecture decisions:
--
--   * Runtime OWNS the event bus. Callers receive a subscribe-only interface
--     ('rtSubscribe' / 'rtUnsubscribe'). 'ebPublish' is internal; callers
--     cannot inject events that bypass the Runtime lifecycle.
--
--   * Inference is asynchronous. 'assertFact', 'retractFact', and
--     'registerRule' return immediately with a 'SnapshotId'. The inference
--     thread wakes on the dirty flag, waits for the debounce window to
--     collect batched writes, then runs 'runInference' to fixed point.
--     Callers that need to know inference is done subscribe to
--     'EvInferenceCompleted' or 'EvInferenceLimitReached'.
--
--   * A 'TMVar ()' serves as the dirty flag. Writing to it is idempotent
--     (tryPutTMVar); reading drains it atomically before each inference run.
--
--   * The inference thread holds a read-only snapshot of the KB at the
--     moment it wakes, so writes during an inference run are safe and will
--     trigger a subsequent run via the dirty flag.
--
--   * Contradiction detection runs after each inference cycle using
--     'detectContradictions'. Matches are acted on per their registered
--     'ContradictionPolicy' (RecordAndContinue / RecordAndHalt /
--     RecordAndResolve). A 'ContradictionHalt' match sets the cycle result
--     to halt the dirty-flag loop.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Rule
--   Analytics.Core.KnowledgeBase
--   Analytics.Core.Inference
--   Analytics.Core.Contradiction
--   Analytics.Core.Event.Types
--   Analytics.Core.Event.Event
--   <- this module

module Analytics.Core.Runtime
  ( -- * Runtime handle
    Runtime
    -- * Configuration
  , RuntimeConfig(..)
  , defaultRuntimeConfig
    -- * Lifecycle
  , newRuntime
  , shutdownRuntime
  , withRuntime
    -- * KB mutations (write path)
  , assertFact
  , assertFacts
  , retractFact
  , registerRule
  , disableRule
    -- * Contradiction predicate registration
  , rtRegisterPredicate
  , rtUnregisterPredicate
    -- * Event subscription (subscribe-only)
  , rtSubscribe
  , rtUnsubscribe
    -- * Read path — snapshot queries
  , withSnapshot
    -- * Errors
  , RuntimeError(..)
  ) where

import Relude hiding (state)
import Control.Concurrent              (threadDelay)
import Control.Concurrent.Async        (Async, async, cancel, waitCatch)
import Control.Exception               (bracket, try)
import Control.Monad                   (foldM)

import Data.Time.Clock                 (getCurrentTime)
import Data.UUID.V4                    (nextRandom)
import qualified Data.Map.Strict       as Map

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  , tagActive
  , factRecordId
  , mkAssertedFact
  )
import Analytics.Core.Rule
  ( Rule
  , enableRule
  , rulePluginId
  )
import Analytics.Core.KnowledgeBase
  ( KBHandle
  , KBError(..)
  , ReadWrite
  , ReadOnly
  , newKBHandle
  , roHandle
  , kbInsertFact
  , kbInsertFacts
  , kbRetractFact
  , kbResolveContradiction
  , kbInsertRule
  , kbDisableRule
  , kbQueryFacts
  , kbQueryRules
  , kbCurrentSnapshot
  )
import Analytics.Core.Storage
  ( emptyFactQuery
  , emptyRuleQuery
  , RuleQuery(..)
  )
import Analytics.Core.Inference
  ( InferenceConfig
  , InferenceResult(..)
  , defaultInferenceConfig
  , runInference
  )
import Analytics.Core.Contradiction
  ( ContradictionRegistry
  , ContradictionPredicate(..)
  , ContradictionMatch(..)
  , ContradictionPolicy(..)
  , RegistrationError
  , PredicateId
  , ResolvedFact(..)
  , newRegistry
  , registerPredicate
  , unregisterPredicate
  , listPredicates
  , detectContradictions
  , recordContradiction
  , contradictionEvent
  , resolverOf
  , validateResolvedFact
  )
import Analytics.Core.Event.Types
  ( Event(..)
  , EventFilter(..)
  , InferenceSummary(..)
  , SubscriptionId
  )
import Analytics.Core.Event.Event
  ( EventBus(..)
  , makeBoundedEventBus
  )

-- ---------------------------------------------------------------------------
-- RuntimeConfig

data RuntimeConfig = RuntimeConfig
  { rcInferenceConfig :: !InferenceConfig
    -- ^ Passed verbatim to 'runInference' on every inference cycle.
  , rcEventBusSize    :: !Int
    -- ^ Bounded queue capacity for the event bus.
    -- Minimum 1; values below 1 are clamped to 1 internally.
  , rcDebounceMs      :: !Int
    -- ^ Milliseconds to wait after a dirty-flag signal before running
    -- inference. Allows batched writes to settle before the engine wakes.
    -- Set to 0 to disable debouncing.
  , rcMaxSnapshots    :: !Int
    -- ^ Maximum number of historical KB snapshots retained in memory.
    -- When a new write would exceed this limit, the oldest snapshot is
    -- evicted. Callers holding a pruned SnapshotId receive
    -- 'KBSnapshotNotFound' (see 'QESnapshotNotFound' in Query).
    -- Minimum 1; values below 1 are clamped to 1 by 'newKBHandle'.
    -- Default 256: sufficient for point-in-time queries over short windows
    -- without unbounded memory growth.
  , rcMaxContradictionRecords :: !Int
    -- ^ Newest-first eviction cap for contradiction history.
    -- Minimum 1; values below 1 are clamped to 1 by 'newRegistry'.
    -- Default 1024.
  } deriving stock (Show)

defaultRuntimeConfig :: RuntimeConfig
defaultRuntimeConfig = RuntimeConfig
  { rcInferenceConfig         = defaultInferenceConfig
  , rcEventBusSize            = 4096
  , rcDebounceMs              = 10
  , rcMaxSnapshots            = 256
  , rcMaxContradictionRecords = 1024
  }

-- ---------------------------------------------------------------------------
-- RuntimeError

data RuntimeError
  = RTKBError      !KBError
    -- ^ A KB-layer error propagated from a write operation.
  | RTShuttingDown
    -- ^ The runtime is shutting down; no further writes are accepted.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Runtime — opaque handle

-- | Opaque runtime handle. Obtain one with 'newRuntime' or 'withRuntime'.
data Runtime = Runtime
  { _rtKB             :: !(KBHandle ReadWrite)
    -- ^ Authoritative mutable knowledge base.
  , _rtBus            :: !EventBus
    -- ^ Internal bus — only Runtime publishes; callers subscribe.
  , _rtDirty          :: !(TMVar ())
    -- ^ Dirty flag. 'tryPutTMVar' is idempotent; multiple writes before the
    -- inference thread wakes collapse to one inference run.
  , _rtInferThread    :: !(Async ())
    -- ^ Background inference thread. Cancelled on shutdown.
  , _rtConfig         :: !RuntimeConfig
  , _rtShutdown       :: !(TVar Bool)
    -- ^ Set to True by 'shutdownRuntime'. Write operations check this and
    -- return 'RTShuttingDown' rather than mutating a closing KB.
  , _rtContradictions :: !ContradictionRegistry
    -- ^ Registered contradiction predicates. Plugins register via
    -- 'rtRegisterPredicate'; Runtime calls 'detectContradictions' after
    -- each inference cycle.
  }

-- ---------------------------------------------------------------------------
-- Lifecycle

-- | Allocate a Runtime and start the background inference thread.
-- Prefer 'withRuntime' for automatic cleanup.
newRuntime :: RuntimeConfig -> IO Runtime
newRuntime cfg = do
  kb              <- newKBHandle (rcMaxSnapshots cfg)
  bus             <- makeBoundedEventBus (max 1 (rcEventBusSize cfg))
  dirty           <- newEmptyTMVarIO
  shutdownVar     <- newTVarIO False
  contradictions  <- newRegistry (rcMaxContradictionRecords cfg)
  thread          <- async (inferenceLoop cfg kb bus dirty shutdownVar contradictions)
  pure Runtime
    { _rtKB             = kb
    , _rtBus            = bus
    , _rtDirty          = dirty
    , _rtInferThread    = thread
    , _rtConfig         = cfg
    , _rtShutdown       = shutdownVar
    , _rtContradictions = contradictions
    }

-- | Signal shutdown and wait for the inference thread to finish its current
-- run. After this returns, all write operations return 'RTShuttingDown'.
shutdownRuntime :: Runtime -> IO ()
shutdownRuntime rt = do
  atomically $ writeTVar (_rtShutdown rt) True
  -- Wake the inference thread so it sees the shutdown flag and exits cleanly.
  void $ atomically $ tryPutTMVar (_rtDirty rt) ()
  void $ waitCatch (_rtInferThread rt)
  cancel (_rtInferThread rt)
  ebShutdown (_rtBus rt)

-- | 'bracket'-safe runtime acquisition.
withRuntime :: RuntimeConfig -> (Runtime -> IO a) -> IO a
withRuntime cfg = bracket (newRuntime cfg) shutdownRuntime

-- ---------------------------------------------------------------------------
-- Write path
--
-- Every write:
--   1. Checks the shutdown flag.
--   2. Delegates to the KnowledgeBase layer.
--   3. On success, publishes the appropriate event and marks the KB dirty.

-- | Assert a single fact into the knowledge base.
-- Returns the 'SnapshotId' stamped at insertion time.
-- Inference runs asynchronously; subscribe to 'EvInferenceCompleted' to
-- observe derived consequences.
assertFact
  :: Fact 'NormalFact
  -> Runtime
  -> IO (Either RuntimeError SnapshotId)
assertFact f rt = guardShutdown rt $ do
  result <- kbInsertFact f (_rtKB rt)
  case result of
    Left  err -> pure (Left (RTKBError err))
    Right sid -> do
      ebPublish (_rtBus rt) (EvFactAsserted f)
      markDirty rt
      pure (Right sid)

-- | Assert a non-empty batch of facts atomically under a single SnapshotId.
assertFacts
  :: NonEmpty (Fact 'NormalFact)
  -> Runtime
  -> IO (Either RuntimeError SnapshotId)
assertFacts fs rt = guardShutdown rt $ do
  result <- kbInsertFacts fs (_rtKB rt)
  case result of
    Left  err -> pure (Left (RTKBError err))
    Right sid -> do
      mapM_ (ebPublish (_rtBus rt) . EvFactAsserted) (toList fs)
      markDirty rt
      pure (Right sid)

-- | Logically retract a fact and cascade to all derived descendants.
retractFact
  :: FactId
  -> Text            -- ^ human-readable reason, carried in 'EvFactRetracted'
  -> Runtime
  -> IO (Either RuntimeError SnapshotId)
retractFact fid reason rt = guardShutdown rt $ do
  result <- kbRetractFact fid (_rtKB rt)
  case result of
    Left  err  -> pure (Left (RTKBError err))
    Right sid  -> do
      ebPublish (_rtBus rt) (EvFactRetracted fid reason)
      markDirty rt
      pure (Right sid)

-- | Register a rule. Enabled immediately; included in the next inference run.
registerRule
  :: Rule
  -> Runtime
  -> IO (Either RuntimeError SnapshotId)
registerRule r rt = guardShutdown rt $ do
  result <- kbInsertRule r (_rtKB rt)
  case result of
    Left  err -> pure (Left (RTKBError err))
    Right sid -> do
      markDirty rt
      pure (Right sid)

-- | Disable a rule. Facts exclusively derived by this rule are retracted.
disableRule
  :: RuleId
  -> Runtime
  -> IO (Either RuntimeError SnapshotId)
disableRule rid rt = guardShutdown rt $ do
  result <- kbDisableRule rid (_rtKB rt)
  case result of
    Left  err               -> pure (Left (RTKBError err))
    Right (rule, sid, retracted) -> do
      ebPublish (_rtBus rt) (EvRuleDisabled rid (rulePluginId rule) retracted sid)
      markDirty rt
      pure (Right sid)

-- ---------------------------------------------------------------------------
-- Contradiction predicate registration

-- | Register a contradiction predicate with this runtime.
-- The predicate will be evaluated after each inference cycle.
rtRegisterPredicate
  :: ContradictionPredicate
  -> Runtime
  -> IO (Either RegistrationError ())
rtRegisterPredicate cp rt =
  registerPredicate cp (_rtContradictions rt)

-- | Unregister a previously registered contradiction predicate.
rtUnregisterPredicate :: PredicateId -> Runtime -> IO ()
rtUnregisterPredicate pid rt =
  unregisterPredicate pid (_rtContradictions rt)

-- ---------------------------------------------------------------------------
-- Subscribe-only event interface
--
-- Callers can subscribe and unsubscribe. They cannot publish.
-- ebPublish is not exposed; Runtime is the only publisher on this bus.

-- | Subscribe to events matching the given filter.
rtSubscribe
  :: EventFilter
  -> (Event -> IO ())
  -> Runtime
  -> IO SubscriptionId
rtSubscribe filt handler rt =
  ebSubscribe (_rtBus rt) filt handler

-- | Cancel an event subscription.
rtUnsubscribe :: SubscriptionId -> Runtime -> IO ()
rtUnsubscribe sid rt = ebUnsubscribe (_rtBus rt) sid

-- ---------------------------------------------------------------------------
-- Read path — snapshot queries

-- | Take a consistent read-only snapshot of the current KB state and run
-- a query against it. No writes can interleave during the query.
--
-- Example:
-- @
--   facts <- withSnapshot rt $ \snap ->
--     kbQueryFacts (emptyFactQuery { fqType = Just "sensor:reading" }) snap
-- @
withSnapshot :: Runtime -> (KBHandle ReadOnly -> IO a) -> IO a
withSnapshot rt action = roHandle (_rtKB rt) >>= action

-- ---------------------------------------------------------------------------
-- Inference thread

-- | Main loop: block on the dirty flag, then run one inference cycle.
inferenceLoop
  :: RuntimeConfig
  -> KBHandle ReadWrite
  -> EventBus
  -> TMVar ()
  -> TVar Bool
  -> ContradictionRegistry
  -> IO ()
inferenceLoop cfg kb bus dirty shutdownVar contradictions = loop
  where
    loop = do
      -- Block until something marks the KB dirty.
      atomically $ takeTMVar dirty

      stopping <- readTVarIO shutdownVar
      unless stopping $ do

        -- Debounce: let batched writes settle before reading the KB.
        when (rcDebounceMs cfg > 0) $
          threadDelay (rcDebounceMs cfg * 1000)

        -- Drain any additional dirty signals that arrived during debounce;
        -- they are all served by this single upcoming run.
        atomically $ void $ tryTakeTMVar dirty

        -- Run one inference cycle, catching unexpected exceptions so the
        -- loop survives transient faults and emits EvRuntimeFault instead.
        outcome <- try (runCycle cfg kb bus contradictions) :: IO (Either SomeException Bool)
        case outcome of
          Left  ex       -> ebPublish bus (EvRuntimeFault (show ex) False)
          Right newFacts ->
            -- If the cycle derived new facts, mark dirty for another run.
            -- This handles chained rules: A -> B -> C across cycles.
            when newFacts $ void $ atomically $ tryPutTMVar dirty ()

        loop

-- | Run one complete inference cycle.
-- Returns True if new facts were derived (signals another cycle needed).
runCycle
  :: RuntimeConfig
  -> KBHandle ReadWrite
  -> EventBus
  -> ContradictionRegistry
  -> IO Bool
runCycle cfg kb bus contradictions = do
  snap   <- roHandle kb
  snapId <- kbCurrentSnapshot kb

  allFacts <- kbQueryFacts emptyFactQuery snap
  allRules <- kbQueryRules (emptyRuleQuery { rqEnabled = Just True }) snap

  case nonEmpty allRules of
    Nothing -> pure False
    Just enabledRules -> do
      let activeFacts = Map.fromList
            [ (factRecordId f, tagActive f)
            | f <- allFacts
            ]

      result <- runInference
        (rcInferenceConfig cfg)
        bus
        snapId
        activeFacts
        (fmap enableRule enabledRules)

      -- Publish the inference lifecycle event.
      -- toSummary lives here (not in Inference) to avoid the
      -- Inference -> Event.Types -> Inference dependency cycle.
      let summary = toSummary result
      case terminationReason result of
        FixedPoint        -> ebPublish bus (EvInferenceCompleted summary)
        LimitReached lim  -> ebPublish bus (EvInferenceLimitReached lim summary)
        ContradictionHalt -> ebPublish bus (EvRuntimeFault "ContradictionHalt during inference" True)

      -- Contradiction detection after inference settles.
      -- Uses finalFacts (initial + all derived) so that contradictions
      -- introduced by newly derived facts are caught in the same cycle.
      ts         <- getCurrentTime
      predicates <- listPredicates contradictions
      let activeForDetection = map tagActive (finalFacts result)
          matches = detectContradictions activeForDetection predicates snapId ts

      haltForContradiction <- foldM (handleMatch kb bus contradictions) False matches

      -- Write derived facts back into the KB.
      -- kbInsertFacts is idempotent on content; re-inserting known facts is safe.
      let derived = derivedFacts result
      case nonEmpty derived of
        -- No new facts derived and no halt requested: stop the dirty loop.
        Nothing  -> pure False
        -- New facts derived: write them back. Signal another cycle only if
        -- contradiction detection did not request a halt.
        Just nef -> do
          insertResult <- kbInsertFacts nef kb
          case insertResult of
            Left kbErr -> do
              ebPublish bus
                (EvRuntimeFault ("kbInsertFacts failed: " <> show kbErr) True)
              pure True   -- halt; do not retry a broken KB write
            Right _ ->
              pure (not haltForContradiction)

-- | Act on a single detected contradiction per its registered policy.
-- Returns True if the policy was RecordAndHalt (signals runCycle to stop).
handleMatch
  :: KBHandle ReadWrite
  -> EventBus
  -> ContradictionRegistry
  -> Bool                  -- accumulator: has any prior match already requested halt?
  -> ContradictionMatch
  -> IO Bool
handleMatch kb bus registry alreadyHalting match = do
  -- Record the contradiction; only emit events and dispatch policy if the record succeeds.
  recordResult <- recordContradiction (cmRecord match) registry
  case recordResult of
    Left errMsg -> do
      ebPublish bus (EvRuntimeFault
        ("recordContradiction invariant violated: " <> errMsg) True)
      pure alreadyHalting   -- malformed record; skip policy dispatch

    Right () -> do
      ebPublish bus (contradictionEvent match)   -- only emit when record stored

      case cpPolicy (cmPredicate match) of
        RecordAndContinue -> pure alreadyHalting

        RecordAndHalt -> pure True

        RecordAndResolve _ ->
          case resolverOf (cpPolicy (cmPredicate match)) of
            Nothing       -> pure alreadyHalting  -- structurally unreachable
            Just resolver -> do
              let rf = resolver (cmFact1 match) (cmFact2 match)
              case validateResolvedFact (cpOwner (cmPredicate match)) rf of
                Left  err -> do
                  -- Resolver produced an invalid namespace: treat as halt.
                  ebPublish bus (EvRuntimeFault
                    (  "RecordAndResolve produced invalid resolved fact: "
                    <> show err
                    ) True)
                  pure True
                Right _validRf -> do
                  newFid <- FactId <$> nextRandom
                  ts     <- getCurrentTime
                  case mkAssertedFact newFid (rfType rf) (rfAttrs rf) ts of
                    Left  errs -> do
                      ebPublish bus (EvRuntimeFault
                        (  "RecordAndResolve resolved fact failed validation: "
                        <> show errs
                        ) True)
                      pure True
                    Right resolvedFact -> do
                      -- Use the atomic helper: all three mutations (retract fid1,
                      -- retract fid2, insert resolved) occur under a single STM
                      -- commit. The previous three-op sequence was non-atomic:
                      -- an async exception between any two ops left the KB in a
                      -- permanently inconsistent state (one fact retracted, no
                      -- resolved fact present). kbResolveContradiction eliminates
                      -- that window entirely.
                      result <- kbResolveContradiction
                                   (factRecordId (cmFact1 match))
                                   (factRecordId (cmFact2 match))
                                   resolvedFact
                                   kb
                      case result of
                        Left err -> do
                          -- Surface the error rather than silently discarding it
                          -- (the previous code used void $ on all three ops).
                          ebPublish bus (EvRuntimeFault
                            (  "kbResolveContradiction failed: "
                            <> show err
                            ) True)
                          pure True
                        Right _sid -> pure alreadyHalting

-- | Project 'InferenceResult' into the narrow 'InferenceSummary' carried by
-- lifecycle events. Conversion lives in Runtime — not in Inference — to keep
-- Inference free of Event.Types imports (cycle prevention, see module header).
toSummary :: InferenceResult -> InferenceSummary
toSummary r = InferenceSummary
  { isFinalFactCount = fromIntegral (length (finalFacts  r))
  , isDerivedCount   = fromIntegral (length (derivedFacts r))
  , isTermination    = terminationReason r
  , isIterations     = iterationCount r
  , isElapsedMs      = elapsedMs r
  }

-- ---------------------------------------------------------------------------
-- Internal helpers

-- | Guard against writes after shutdown.
guardShutdown :: Runtime -> IO (Either RuntimeError a) -> IO (Either RuntimeError a)
guardShutdown rt action = do
  stopping <- readTVarIO (_rtShutdown rt)
  if stopping then pure (Left RTShuttingDown) else action

-- | Signal the inference thread that the KB has changed.
-- Idempotent: multiple signals before the thread wakes collapse to one run.
markDirty :: Runtime -> IO ()
markDirty rt = void $ atomically $ tryPutTMVar (_rtDirty rt) ()
