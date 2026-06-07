-- | Analytics.Core.Event.Types
--
-- Pure event ADT and supporting types.
--
-- This module occupies the lowest layer of the event system. It deliberately
-- does NOT import Analytics.Core.Inference or Analytics.Core.Contradiction —
-- that would create a three-way dependency cycle:
--
--   Contradiction → Event.Types → Inference → Contradiction
--
-- The cycle is broken by:
--   1. TerminationReason / LimitReason living in Analytics.Core.Types.
--   2. InferenceSummary (a lightweight value type) living here rather than
--      in Inference, so Event.Types never needs to import Inference.
--   3. Analytics.Core.Runtime assembles InferenceSummary from InferenceResult
--      after inference completes and publishes EvInferenceCompleted.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact   (Fact 'NormalFact, FactId, PluginId)
--   Analytics.Core.Rule   (RuleId)
--   ← this module

module Analytics.Core.Event.Types
  ( -- * Inference summary (cycle-breaking lightweight record)
    InferenceSummary(..)
    -- * Subscription identity
  , SubscriptionId(..)
    -- * Event ADT
  , Event(..)
    -- * Event classifier
  , EventType(..)
  , eventType
    -- * Event filter
  , EventFilter(..)
    -- * Plugin error (needed by EvPluginFailed)
  , PluginError(..)
  ) where

import Relude
import Data.UUID (UUID)

import Analytics.Core.Types
import Analytics.Core.Fact  (Fact, FactKind(..))
import Analytics.Core.Rule ()

-- ---------------------------------------------------------------------------
-- SubscriptionId

-- | Opaque identifier for an event-bus subscription.
-- Produced by 'ebSubscribe'; required to cancel via 'ebUnsubscribe'.
newtype SubscriptionId = SubscriptionId UUID
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

-- ---------------------------------------------------------------------------
-- InferenceSummary
--
-- A deliberately narrow projection of InferenceResult. Carrying the full
-- InferenceResult in an event would force every event subscriber to link
-- against Analytics.Core.Inference. Using a summary keeps the event
-- payload self-contained and Event.Types dependency-free of Inference.

data InferenceSummary = InferenceSummary
  { isFinalFactCount :: !Natural
    -- ^ Total facts in the knowledge base after this inference run.
  , isDerivedCount   :: !Natural
    -- ^ Facts newly derived during this run (not previously present).
  , isTermination    :: !TerminationReason
    -- ^ How the run ended: fixed point, limit breach, or contradiction halt.
  , isIterations     :: !Natural
    -- ^ Number of semi-naive iterations executed.
  , isElapsedMs      :: !Natural
    -- ^ Wall-clock duration in milliseconds (LOGIC-08: consistent unit).
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- PluginError
--
-- Defined here rather than in Plugin because:
--   - Event.Types needs it for EvPluginFailed.
--   - Plugin imports Event.Types (for EventSubscription).
-- Putting it here avoids a Plugin → Event → Plugin cycle.

data PluginError
  = PluginInitFailed        !Text
    -- ^ Plugin.initialize returned an error or threw an exception.
  | PluginApiVersionMismatch !Version !Version
    -- ^ (expected, actual): plugin's declared apiVersion is incompatible.
  | PluginNamespaceConflict  !PluginId
    -- ^ Another registered plugin already owns this namespace.
  | PluginTimeout            !Text
    -- ^ The named lifecycle method (\"initialize\", \"facts\", etc.) timed out.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Event
--
-- A closed ADT. There is no Text catch-all constructor.
--
-- Adding a new event kind requires:
--   1. A new constructor here.
--   2. A new 'EventType' constructor below.
--   3. A new arm in 'eventType'.
--   4. Updating every exhaustive pattern-match site in the codebase
--      (caught at compile time by -Wincomplete-patterns -Werror).
--
-- This is intentional: events are part of the observable contract of the
-- system. Silent additions should be impossible.

data Event
  -- Fact lifecycle
  = EvFactAsserted          !(Fact 'NormalFact)
    -- ^ A normal fact was successfully inserted into the knowledge base.
  | EvFactRetracted         !FactId !Text
    -- ^ A fact was logically retracted; second field is the reason string.
  | EvFactRejected          !PluginId !Text !(Fact 'NormalFact)
    -- ^ A fact was rejected before insertion: (submitting plugin, reason, raw fact).
  | EvFactPhysicallyDeleted !FactId  !Text
    -- ^ Administrative hard-delete (destroys provenance); requires explicit opt-in.

  -- Rule lifecycle
  | EvRuleFired             !RuleId !PluginId !(NonEmpty FactId) !FactId
    -- ^ Rule fired: (ruleId, owning plugin, parent facts, derived fact id).
    -- PluginId is carried here so 'EventsFromPlugin' filters can match rule
    -- events without a KB lookup at dispatch time.
  | EvRuleDisabled          !RuleId !PluginId ![FactId] !SnapshotId
    -- ^ Rule disabled; PluginId = owning plugin; third field = retracted facts.

  -- Plugin lifecycle
  | EvPluginLoaded          !PluginId !Version
  | EvPluginFailed          !PluginId !PluginError
  | EvPluginRejected        !PluginId !Text
    -- ^ Plugin rejected at registration; second field is the rejection reason.

  -- Inference lifecycle
  | EvInferenceStarted      !SnapshotId
    -- ^ Emitted before the first iteration; carries the snapshot at start time.
  | EvInferenceCompleted    !InferenceSummary
    -- ^ Inference reached a fixed point normally.
  | EvInferenceLimitReached !LimitReason !InferenceSummary
    -- ^ Inference terminated due to a configured limit.

  -- Contradiction detection
  | EvContradictionDetected !FactId !FactId !Text
    -- ^ (fact1, fact2, predicate-id): two facts violate a registered predicate.

  -- Snapshot management
  | EvSnapshotCreated       !SnapshotId

  -- Bus health
  | EvSubscriberTimeout     !SubscriptionId
    -- ^ A subscriber's handler did not complete within the deadline.

  -- Runtime faults
  | EvRuntimeFault          !Text !Bool
    -- ^ (reason, isFatal): non-fatal faults are logged; fatal faults halt.

  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- EventType
--
-- A mirror of Event's constructors as a plain enumeration. Used for
-- subscription filtering and logging without pattern-matching on full Event
-- values.
--
-- Derives Enum and Bounded so code can iterate over all event types
-- (e.g. to build a metrics counter per type) without a hardcoded list.

data EventType
  = ETFactAsserted
  | ETFactRetracted
  | ETFactRejected
  | ETFactPhysicallyDeleted
  | ETRuleFired
  | ETRuleDisabled
  | ETPluginLoaded
  | ETPluginFailed
  | ETPluginRejected
  | ETInferenceStarted
  | ETInferenceCompleted
  | ETInferenceLimitReached
  | ETContradictionDetected
  | ETSnapshotCreated
  | ETSubscriberTimeout
  | ETRuntimeFault
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Total classifier; -Wincomplete-patterns enforces exhaustiveness.
eventType :: Event -> EventType
eventType = \case
  EvFactAsserted          {} -> ETFactAsserted
  EvFactRetracted         {} -> ETFactRetracted
  EvFactRejected          {} -> ETFactRejected
  EvFactPhysicallyDeleted {} -> ETFactPhysicallyDeleted
  EvRuleFired             {} -> ETRuleFired
  EvRuleDisabled          {} -> ETRuleDisabled
  EvPluginLoaded          {} -> ETPluginLoaded
  EvPluginFailed          {} -> ETPluginFailed
  EvPluginRejected        {} -> ETPluginRejected
  EvInferenceStarted      {} -> ETInferenceStarted
  EvInferenceCompleted    {} -> ETInferenceCompleted
  EvInferenceLimitReached {} -> ETInferenceLimitReached
  EvContradictionDetected {} -> ETContradictionDetected
  EvSnapshotCreated       {} -> ETSnapshotCreated
  EvSubscriberTimeout     {} -> ETSubscriberTimeout
  EvRuntimeFault          {} -> ETRuntimeFault

-- ---------------------------------------------------------------------------
-- EventFilter
--
-- Subscribers specify which events they care about. The runtime applies
-- filters before invoking a subscriber's handler, so subscribers are not
-- responsible for their own filtering.

data EventFilter
  = AllEvents
    -- ^ Receive every event without filtering.
  | EventsOfType    !(NonEmpty EventType)
    -- ^ Receive only events whose 'eventType' is in this set.
    -- NonEmpty: an empty type set would produce a subscription that never
    -- fires, which is always a caller bug.
  | EventsFromPlugin !PluginId
    -- ^ Receive events associated with a specific plugin.
  deriving stock (Show, Eq)