-- | Analytics.Core.Inference
--
-- Semi-naive forward-chaining inference engine.
--
-- This module is the computational core of the system. It takes a snapshot
-- of active facts and enabled rules and iterates to a fixed point, deriving
-- new facts according to the rules and emitting provenance evidence for every
-- derivation.
--
-- Key design invariants enforced here:
--
--   * Only 'ActiveFact' values enter the engine. The phantom type wrapper
--     ensures retracted facts cannot be matched by rules at the call site.
--
--   * Only 'EnabledRule' values enter the engine. Disabled rules are
--     excluded by the caller (Runtime), not filtered inside this module.
--
--   * Semi-naive optimisation: in each iteration, at least one premise of a
--     rule must be satisfied by a fact from the *previous* iteration's delta
--     set. This bounds work to new derivations rather than re-evaluating all
--     rules against the full fact set on every round.
--
--   * The content index ('Set FactContent') provides O(1) idempotency checks
--     so a fact whose type+attributes already exist is suppressed without an
--     O(n) scan of 'isFactMap' (Def 3.12).
--
--   * Event emission is performed inline during the loop ('EvRuleFired',
--     'EvFactRejected') and from 'runLoop' on the first limit breach when
--     'WarnAndContinue' is in effect ('EvInferenceLimitReached'). This
--     module does NOT import the EventBus machinery module — the 'EventBus'
--     record is passed in as a value, keeping Inference independent of the
--     bus implementation and free of the Contradiction→Event→Inference cycle.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Rule
--   Analytics.Core.Evidence
--   Analytics.Core.Graph
--   Analytics.Core.Event.Types   (Event ADT, InferenceSummary)
--   Analytics.Core.Event.Event   (EventBus record — value only, no machinery)
--   ← this module

module Analytics.Core.Inference
  ( -- * Entry point
    runInference
    -- * Configuration
  , InferenceConfig(..)
  , defaultInferenceConfig
  , LimitPolicy(..)
  , ConflictPolicy(..)
    -- * Validated positive bound
  , Positive(..)
  , mkPositive
    -- * Result
  , InferenceResult(..)
    -- * Internal state (exposed for Runtime and tests)
  , InferenceState(..)
    -- * Unrecoverable fault
  , RuntimeFaultException(..)
  ) where

import Relude hiding (state)
import Control.Exception            (throwIO)
import Control.Monad                (foldM)
import Data.List                    (partition)
import Data.Time.Clock
  ( UTCTime
  , getCurrentTime
  , diffUTCTime
  )
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text
import           Data.UUID.V4       (nextRandom)

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  , FactContent
  , ActiveFact
  , Pattern(..)
  , TypeConstraint(..)
  , MatchResult(..)
  , factContent
  , factRecordId
  , factRecordType
  , tagActive
  , untagFact
  , instantiateTemplate
  , completeInferredFact
  , matchPattern
  )
import Analytics.Core.Rule
  ( Rule(..)
  , EnabledRule
  , untagRule
  , sortRules
  )
import Analytics.Core.Evidence
  ( Evidence(derivedFact, parentFacts, ruleUsed, rulePlugin, depth)
  , mkEvidence
  )
import Analytics.Core.Graph
  ( Graph
  , GraphNode(..)
  , GraphEdge(..)
  , EdgeType(..)
  , NodeId(..)
  , NodeStatus(..)
  , emptyGraph
  , addNode
  , addEdge
  , detectCycle
  )
import Analytics.Core.Event.Types
  ( Event(..)
  , InferenceSummary(..)
  )
import Analytics.Core.Event.Event (EventBus(..))

-- ---------------------------------------------------------------------------
-- Unrecoverable fault exception
--
-- Thrown via 'throwIO' (outside STM) only when the post-loop provenance
-- cycle check detects a cycle. This cannot be returned as an 'Either'
-- because it represents an invariant violation — the engine would have
-- already published events describing an unsound derivation chain — and
-- the only correct response is an immediate halt.
--
-- All other failure modes (bad input, limit breaches, template errors) are
-- returned as 'Left' or surfaced through 'InferenceResult'.

newtype RuntimeFaultException = RuntimeFaultException Text
  deriving stock (Show)

instance Exception RuntimeFaultException

-- ---------------------------------------------------------------------------
-- Positive — validated bound wrapper

-- | A value guaranteed to be strictly positive (> 0).
-- Construct with 'mkPositive'. Never use the 'Positive' constructor directly
-- except for statically-evident compile-time constants in 'defaultInferenceConfig'.
newtype Positive a = Positive { getPositive :: a }
  deriving stock (Show, Eq, Ord)

-- | Smart constructor. Returns 'Left' for zero or negative values.
mkPositive :: (Num a, Ord a, Show a) => a -> Either Text (Positive a)
mkPositive x
  | x > 0     = Right (Positive x)
  | otherwise  = Left $ "Value must be positive (> 0), got: " <> show x

-- ---------------------------------------------------------------------------
-- InferenceConfig

-- | What to do when a configured limit (maxFacts, maxDepth, timeoutMs) is
-- reached mid-run.
data LimitPolicy
  = Halt
    -- ^ Stop immediately and return the current state with the limit reason.
  | WarnAndContinue
    -- ^ Emit 'EvInferenceLimitReached' once on the first breach, then keep
    -- iterating. See the warning in 'InferenceConfig' before using this.
  deriving stock (Show, Eq)

-- | How to handle multiple rules firing in the same iteration.
data ConflictPolicy
  = PriorityOrder
    -- ^ Apply rules in descending priority order ('priority' field on 'Rule';
    -- higher number fires first). Tiebreaker: 'ruleName' ascending (LOGIC-09).
  | FailOnConflict
    -- ^ Reserved for future use; behaves like 'PriorityOrder' currently.
  deriving stock (Show, Eq)

-- | Configuration for a single inference run.
data InferenceConfig = InferenceConfig
  { maxFacts      :: !(Positive Int)
    -- ^ Maximum total number of facts (initial + derived) before the limit
    -- policy fires. Compared against Map.size isFactMap at the start of each
    -- iteration.
  , maxDepth      :: !(Positive Int)
    -- ^ Maximum number of semi-naive iterations before the limit policy fires.
    -- This is an *iteration counter*, not a per-fact derivation depth
    -- (LOGIC-03). Per-fact depth is tracked in 'Evidence.depth'.
  , timeoutMs     :: !(Positive Int)
    -- ^ Wall-clock deadline for the entire run in milliseconds. Checked once
    -- per iteration before executing that iteration (LOGIC-08).
  , onLimitBreach :: !LimitPolicy
    -- ^ ⚠  WARNING (LOGIC-04): 'WarnAndContinue' must only be used when the
    -- fact set is guaranteed to reach a fixed point by some other bound
    -- (e.g. acyclic rules with a finite type domain). If the set grows
    -- indefinitely past the limit, 'runLoop' will not terminate.
    -- 'EvInferenceLimitReached' is emitted at most once (spec §12.4);
    -- subsequent iterations are silent. Operators MUST monitor 'elapsedMs'
    -- or set an external process deadline when using this policy.
  , conflictPolicy :: !ConflictPolicy
  } deriving stock (Show)

-- | Statically-valid defaults. These values (1 M facts, 100 rounds, 60 s)
-- are compile-time constants whose positivity is structurally evident.
-- Callers supplying runtime values must use 'mkPositive'.
defaultInferenceConfig :: InferenceConfig
defaultInferenceConfig = InferenceConfig
  { maxFacts       = Positive 1000000
  , maxDepth       = Positive 100
  , timeoutMs      = Positive 60000
  , onLimitBreach  = Halt
  , conflictPolicy = PriorityOrder
  }

-- ---------------------------------------------------------------------------
-- InferenceResult

-- | The complete outcome of one inference run.
data InferenceResult = InferenceResult
  { finalFacts        :: ![Fact 'NormalFact]
    -- ^ All facts in scope at termination (initial + all derived).
  , derivedFacts      :: ![Fact 'NormalFact]
    -- ^ Only the facts newly derived during this run (not in the seed set).
  , evidenceChain     :: ![Evidence]
    -- ^ All evidence records produced during this run, in derivation order.
  , terminationReason :: !TerminationReason
    -- ^ How the run ended: 'FixedPoint', 'LimitReached', or
    -- 'ContradictionHalt' (the last set by Runtime, not this module).
  , iterationCount    :: !Natural
    -- ^ Number of semi-naive iterations completed (= 'isDepth' at exit).
  , elapsedMs         :: !Natural
    -- ^ Wall-clock duration in milliseconds (LOGIC-08: consistent unit).
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- InferenceState — pure inner-loop state

-- | All mutable inference state. Kept pure so the loop is testable without
-- IO until UUID generation is required in 'applyBinding'.
data InferenceState = InferenceState
  { isFactMap       :: !(Map FactId (Fact 'NormalFact))
    -- ^ All facts in scope: initial seed + every fact derived so far.
  , isContentSet    :: !(Set FactContent)
    -- ^ Secondary index for O(1) idempotency checks (Def 3.12).
    -- Must be kept in exact sync with 'isFactMap'.
  , isDeltaMap      :: !(Map FactId (Fact 'NormalFact))
    -- ^ Facts derived in the *previous* iteration.
    -- 'Map.null isDeltaMap' is the fixed-point termination condition.
  , isDepthIndex    :: !(Map FactId Natural)
    -- ^ Maps FactId → derivation depth. Depth 0 = asserted (seed) fact.
    -- Used by 'mkEvidence' to compute child depths (= max(parents) + 1).
  , isEvidence      :: !(Map FactId (NonEmpty Evidence))
    -- ^ Evidence keyed by 'derivedFact' for O(log n) lookup (LOGIC-02).
    -- Also used by 'findExclusivelyDerivedFacts' in KnowledgeBase.
  , isEvidenceList  :: ![Evidence]
    -- ^ Append-only list used for the post-loop provenance cycle check.
  , isDepth         :: !Natural
    -- ^ Iteration counter (LOGIC-03: not per-fact derivation depth).
  , isLimitBreached :: !(Maybe LimitReason)
    -- ^ Set on the first limit breach. 'WarnAndContinue' emits the event
    -- exactly once (when transitioning from Nothing to Just).
  }

-- ---------------------------------------------------------------------------
-- Entry point

-- | Run the inference engine to a fixed point or configured limit.
--
-- Caller responsibilities:
--   1. Pass only active facts ('Map FactId ActiveFact'). The phantom type
--      enforces this; retracted facts cannot appear here.
--   2. Pass only enabled rules ('NonEmpty EnabledRule'). The phantom type
--      enforces this; disabled rules cannot appear here.
--   3. Pass the current 'SnapshotId' as returned by 'kbCurrentSnapshot'
--      before this call. It is stamped onto every 'Evidence' record.
--   4. After this call returns, publish 'EvInferenceCompleted' or
--      'EvInferenceLimitReached' by building an 'InferenceSummary' from the
--      returned 'InferenceResult'. This is done in Runtime (not here) to
--      avoid importing Inference from Event, which would recreate the cycle.
--
-- This function emits:
--   'EvInferenceStarted'      — once, before the first iteration
--   'EvRuleFired'             — once per genuinely new derived fact
--   'EvFactRejected'          — once per suppressed content-duplicate
--   'EvInferenceLimitReached' — at most once, on first breach when
--                               'WarnAndContinue' is in effect
runInference
  :: InferenceConfig
  -> EventBus
  -> SnapshotId            -- current KB snapshot at inference-start time
  -> Map FactId ActiveFact -- only active facts; phantom enforces this
  -> NonEmpty EnabledRule  -- only enabled rules; phantom enforces this
  -> IO InferenceResult
runInference config bus snap initialActiveFacts rules = do
  startTime <- getCurrentTime

  -- Emit EvInferenceStarted before any iteration (spec §20).
  ebPublish bus (EvInferenceStarted snap)

  -- Unwrap the ActiveFact phantom. It has done its job at the call site.
  let initialFacts = Map.map untagFact initialActiveFacts

  -- Asserted facts start at depth 0.
  let initialDepthIdx = Map.map (const 0) initialFacts

  let initState = InferenceState
        { isFactMap       = initialFacts
        , isContentSet    = Set.fromList (map factContent (Map.elems initialFacts))
        , isDeltaMap      = initialFacts  -- first iteration is naive: delta = all
        , isDepthIndex    = initialDepthIdx
        , isEvidence      = Map.empty
        , isEvidenceList  = []
        , isDepth         = 0
        , isLimitBreached = Nothing
        }

  -- Sort rules once before the loop. sortRules is stable and deterministic
  -- (LOGIC-09): primary key = priority descending, tiebreaker = ruleName
  -- ascending. The EnabledRule phantom is preserved by sortRules.
  let sortedRules = sortRules (toList rules)

  finalState <- runLoop config bus snap sortedRules initState startTime

  endTime <- getCurrentTime
  let elapsed = diffTimeMs startTime endTime

  let termReason = case isLimitBreached finalState of
        Just r  -> LimitReached r
        Nothing -> FixedPoint

  -- Post-loop provenance cycle check (§13.3).
  -- Build a graph over FactNodes connected by DerivedFrom edges and verify
  -- it is acyclic. A cycle here means the engine derived a fact that
  -- transitively depends on itself — a soundness violation. We cannot
  -- surface this as a Left because events (EvRuleFired) have already been
  -- emitted for the invalid derivations; the only safe response is to halt.
  let provGraph = buildProvenanceGraph (isEvidenceList finalState)
  case detectCycle provGraph of
    Just cycle_ ->
      throwIO $ RuntimeFaultException $
        "Provenance cycle detected after inference: " <> show cycle_
    Nothing -> pure InferenceResult
      { finalFacts        = Map.elems (isFactMap finalState)
      , derivedFacts      = factListDiff
                              (Map.elems (isFactMap finalState))
                              (Map.elems initialFacts)
      , evidenceChain     = isEvidenceList finalState
      , terminationReason = termReason
      , iterationCount    = isDepth finalState
      , elapsedMs         = elapsed
      }

-- ---------------------------------------------------------------------------
-- Loop

runLoop
  :: InferenceConfig
  -> EventBus
  -> SnapshotId
  -> [EnabledRule]   -- pre-sorted; order is stable across iterations
  -> InferenceState
  -> UTCTime         -- wall-clock time at the start of the whole run
  -> IO InferenceState
runLoop config bus snap rules state startTime
  -- Fixed-point termination: no new facts were derived in the last iteration.
  | Map.null (isDeltaMap state) = pure state
  | otherwise = do
      now <- getCurrentTime
      let elapsedMs' = diffTimeMs startTime now

      -- Timeout check before starting a new iteration (LOGIC-08).
      if elapsedMs' >= fromIntegral (getPositive (timeoutMs config))
        then pure state { isLimitBreached = Just (TimeoutExceeded elapsedMs') }
        else case checkLimits config state of
          Just reason ->
            case onLimitBreach config of
              Halt ->
                pure state { isLimitBreached = Just reason }
              WarnAndContinue -> do
                -- Emit exactly once on the first breach (spec §12.4).
                when (isNothing (isLimitBreached state)) $
                  ebPublish bus
                    (EvInferenceLimitReached reason
                      (stateToSummary state elapsedMs'))
                state' <- runIteration bus snap rules state now
                runLoop config bus snap rules
                  state' { isLimitBreached = Just reason }
                  startTime
          Nothing -> do
            state' <- runIteration bus snap rules state now
            runLoop config bus snap rules state' startTime

-- | Check the fact-count and iteration-depth limits against the current state.
checkLimits :: InferenceConfig -> InferenceState -> Maybe LimitReason
checkLimits config state
  | Map.size (isFactMap state) >= getPositive (maxFacts config) =
      Just (MaxFactsExceeded (fromIntegral (Map.size (isFactMap state))))
  | isDepth state >= fromIntegral (getPositive (maxDepth config)) =
      Just (MaxDepthExceeded (isDepth state))
  | otherwise =
      Nothing

-- ---------------------------------------------------------------------------
-- Single iteration

-- | Fire every rule against the current fact set (semi-naive), collect
-- genuinely new derivations, update state, emit per-fact events.
runIteration
  :: EventBus
  -> SnapshotId
  -> [EnabledRule]
  -> InferenceState
  -> UTCTime
  -> IO InferenceState
runIteration bus snap rules state ts = do
  -- Fire all rules and accumulate (Fact, Evidence) pairs and any RuleFaults.
  -- Rules are already sorted; foldM preserves order.
  (pairs, faults) <- foldM (fireRule state snap ts) ([], []) rules

  -- Emit EvRuntimeFault (fatal=False) for each collected rule-template fault.
  -- Emitting here — after the fold — keeps all ebPublish calls in one place
  -- and avoids any blocking on the event bus queue inside the inner loop.
  forM_ faults $ \rf ->
    ebPublish bus (EvRuntimeFault (rfaultReason rf) False)

  -- BUG-04 fix: zip facts with their evidence *before* the idempotency
  -- partition so that both lists remain in 1:1 correspondence. The original
  -- filtered facts first and then zipped with the unfiltered evidence list,
  -- producing mismatched pairs whenever any fact was suppressed.
  let (genuinePairs, suppressedPairs) =
        partition
          (\(f, _ev) -> not (contentExistsIn (isContentSet state) f))
          pairs

  -- Emit EvFactRejected for each suppressed content-duplicate (spec §3.12).
  forM_ suppressedPairs $ \(f, _ev) ->
    ebPublish bus
      (EvFactRejected (inferPluginId f) "suppressed-duplicate-content" f)

  -- Emit EvRuleFired for each genuinely new derived fact (spec §20).
  -- The (Fact, Evidence) pairs are guaranteed 1:1 by construction above.
  -- PluginId is sourced from Evidence.rulePlugin so EventsFromPlugin filters
  -- can match without a KB lookup at dispatch time.
  forM_ genuinePairs $ \(f, ev) ->
    ebPublish bus
      (EvRuleFired (ruleUsed ev) (rulePlugin ev) (parentFacts ev) (factRecordId f))

  let genuinelyNew = map fst genuinePairs
      newEvidence  = map snd genuinePairs

  let newFactMap  = Map.fromList [(factRecordId f, f) | f <- genuinelyNew]
      newContents = Set.fromList (map factContent genuinelyNew)

  -- Merge new evidence into the keyed index (LOGIC-02).
  let newEvIndex = Map.fromListWith (<>)
        [ (derivedFact ev, ev :| []) | ev <- newEvidence ]

  -- Extend the depth index so future child derivations compute the correct
  -- depth (= max(parent depths) + 1) for their mkEvidence calls.
  let newDepthEntries = Map.fromList
        [ (derivedFact ev, depth ev) | ev <- newEvidence ]

  pure state
    { isFactMap       = Map.union (isFactMap state) newFactMap
    , isContentSet    = Set.union (isContentSet state) newContents
    , isDeltaMap      = newFactMap
    , isDepthIndex    = Map.union (isDepthIndex state) newDepthEntries
    , isEvidence      = Map.unionWith (<>) (isEvidence state) newEvIndex
    , isEvidenceList  = isEvidenceList state <> newEvidence
    , isDepth         = isDepth state + 1
      -- isLimitBreached is preserved across iterations.
      -- De-duplication of EvInferenceLimitReached is handled in runLoop:
      -- the event is emitted only once, on the transition from Nothing → Just
      -- (spec §12.4). Do not clear this field here.
    , isLimitBreached = isLimitBreached state
    }

-- ---------------------------------------------------------------------------
-- Rule firing

-- | A rule-template fault: the rule that failed and a human-readable reason.
-- Collected during 'fireRule' and emitted as 'EvRuntimeFault' events by
-- 'runIteration' after the fold, keeping all 'ebPublish' calls in one place
-- and avoiding any blocking IO inside the tight inner loop.
-- | The reason string carries the rule id and name, so no separate Rule
-- reference is needed; keeping this a plain newtype avoids an unused-field
-- warning under -Wall -Werror.
newtype RuleFault = RuleFault { rfaultReason :: Text }
  deriving stock (Show)

-- | Attempt to fire a single rule. Returns new (Fact, Evidence) pairs and
-- any 'RuleFault's prepended to their respective accumulators.
--
-- Failures from 'instantiateTemplate' or 'completeInferredFact' indicate a
-- malformed rule template not caught at registration time. They are collected
-- as 'RuleFault' values rather than silently discarded; 'runIteration' emits
-- them as 'EvRuntimeFault' events after the fold.
--
-- No 'ebPublish' call is made here: keeping this function IO-only for UUID
-- generation preserves the original design invariant and avoids blocking on
-- the event bus queue inside the tight inner loop.
fireRule
  :: InferenceState
  -> SnapshotId
  -> UTCTime
  -> ([(Fact 'NormalFact, Evidence)], [RuleFault])  -- accumulator
  -> EnabledRule
  -> IO ([(Fact 'NormalFact, Evidence)], [RuleFault])
fireRule state snap ts acc enabledRule = do
  let rule  = untagRule enabledRule
      prems = toList (premises rule)

  -- BUG-03 fix: joinPremises builds a type-keyed index once and restricts
  -- ExactType premises to the relevant subset — O(|matches|) not O(n^k).
  let bindingsWithParents =
        joinPremises
          (isFactMap state)
          (isDeltaMap state)
          (isDepthIndex state)
          prems

  foldM (applyBinding rule snap ts) acc bindingsWithParents

-- ---------------------------------------------------------------------------
-- Premise joining (semi-naive)

-- | Join all premises of a rule against the fact sets.
--
-- Returns a list of complete variable bindings, each paired with the
-- 'NonEmpty (FactId, Natural)' (parent id, depth) of the facts that
-- matched each premise. The NonEmpty guarantee ensures every returned
-- binding has at least one parent, satisfying the 'Derived' constructor.
--
-- Semi-naive constraint: a binding is only returned when at least one
-- matched fact comes from 'deltaFacts'. Bindings where all premises are
-- satisfied by pre-delta facts were already explored in a prior iteration.
--
-- Complexity: O(|allFacts|) to build the type index, then O(|matches|)
-- for the join — not O(n^k). See BUG-03 in the implementation document.
joinPremises
  :: Map FactId (Fact 'NormalFact)   -- full current fact set
  -> Map FactId (Fact 'NormalFact)   -- delta set (new in previous iteration)
  -> Map FactId Natural              -- depth index: FactId → depth
  -> [Pattern]                       -- ordered premises (NonEmpty by Rule)
  -> [(Binding, NonEmpty (FactId, Natural))]
joinPremises allFacts deltaFacts depthIndex pats =
  let -- Build the type-keyed candidate index once per call.
      typeIndex :: Map Text [ActiveFact]
      typeIndex = Map.fromListWith (<>)
        [ (factRecordType f, [tagActive f])
        | f <- Map.elems allFacts
        ]

      allActiveList :: [ActiveFact]
      allActiveList = map tagActive (Map.elems allFacts)

      deltaSet :: Set FactId
      deltaSet = Set.fromList (Map.keys deltaFacts)

      -- Look up candidate facts for a premise using the type index.
      candidatesFor :: Pattern -> [ActiveFact]
      candidatesFor pat = case matchType pat of
        ExactType t -> fromMaybe [] (Map.lookup t typeIndex)
        AnyType     -> allActiveList

  in go Map.empty [] False pats candidatesFor deltaSet
  where
    -- Base case: all premises consumed.
    -- Only return the binding if the semi-naive delta constraint was met.
    go binding parents touchedDelta [] _candidates _deltaSet
      | touchedDelta =
          case NonEmpty.nonEmpty (reverse parents) of
            Just ne -> [(binding, ne)]
            Nothing -> []  -- structurally unreachable (pats was non-empty)
      | otherwise = []     -- all premises matched pre-delta facts; skip

    -- Recursive case: try each candidate for the next premise.
    go binding parents touchedDelta (p:ps) candidatesFor deltaSet =
      [ result
      | af <- candidatesFor p
      , let f    = untagFact af
            fid  = factRecordId f
            d    = fromMaybe 0 (Map.lookup fid depthIndex)
      , Matched b <- [matchPattern binding p af]
      , let touchedDelta' = touchedDelta || Set.member fid deltaSet
      , result <- go b ((fid, d) : parents) touchedDelta' ps candidatesFor deltaSet
      ]

-- ---------------------------------------------------------------------------
-- Binding application (template instantiation + fact/evidence construction)

-- | Instantiate the rule's conclusion template, generate fresh IDs, build
-- an 'Evidence' record, and prepend the (Fact, Evidence) pair to the
-- successes accumulator.
--
-- On failure, a 'RuleFault' is prepended to the faults accumulator instead.
-- No IO other than UUID generation is performed here; 'runIteration' emits
-- all collected faults as 'EvRuntimeFault' events after the fold completes.
applyBinding
  :: Rule
  -> SnapshotId
  -> UTCTime
  -> ([(Fact 'NormalFact, Evidence)], [RuleFault])
  -> (Binding, NonEmpty (FactId, Natural))
  -> IO ([(Fact 'NormalFact, Evidence)], [RuleFault])
applyBinding rule snap ts (pairs, faults) (binding, parentsWithDepths) =
  case instantiateTemplate binding (conclusion rule) of
    Left errs ->
      pure ( pairs
           , RuleFault
               (  "instantiateTemplate failed for rule "
               <> show (ruleId rule)
               <> " (" <> ruleName rule <> "): "
               <> show errs
               ) : faults
           )
    Right pf -> do
      newFid <- FactId     <$> nextRandom
      newEid <- EvidenceId <$> nextRandom
      let parents      = fmap fst parentsWithDepths
          parentDepths = fmap snd parentsWithDepths
      case completeInferredFact pf newFid parents (ruleId rule) ts of
        Left errs ->
          pure ( pairs
               , RuleFault
                   (  "completeInferredFact failed for rule "
                   <> show (ruleId rule)
                   <> " (" <> ruleName rule <> "): "
                   <> show errs
                   ) : faults
               )
        Right fact ->
          let ev = mkEvidence
                     newEid
                     (factRecordId fact)
                     parents
                     (ruleId rule)
                     (rulePluginId rule)
                     binding
                     ts
                     snap
                     parentDepths
          in pure ((fact, ev) : pairs, faults)

-- ---------------------------------------------------------------------------
-- Helpers

-- | O(1) idempotency check against the pre-built content index.
contentExistsIn :: Set FactContent -> Fact 'NormalFact -> Bool
contentExistsIn cs f = Set.member (factContent f) cs

-- | Extract the PluginId from a derived fact's type namespace prefix.
-- Falls back to 'systemPluginId' when the type string has no colon or an
-- empty prefix — both indicate a system-internal or malformed type.
--
-- 'fromRight systemPluginId' is total here: 'mkPluginId' only rejects empty
-- or non-ASCII-alphanumeric strings; a namespace extracted from a colon-split
-- that passes the 'not (Text.null ns)' guard is a well-formed candidate.
-- If 'mkPluginId' rejects it (e.g. contains an invalid character), we fall
-- back to 'systemPluginId', which is the correct conservative behaviour.
-- No partial functions or 'error' calls appear anywhere in this function.
-- 'systemPluginId' is imported from Analytics.Core.Types; it is constructed
-- once, inside its own module using the internal constructor, and is valid by
-- inspection. No 'where' clause, no 'error', no partial function.
inferPluginId :: Fact 'NormalFact -> PluginId
inferPluginId f =
  case Text.breakOn ":" (factRecordType f) of
    (ns, rest)
      | not (Text.null ns), not (Text.null rest) ->
          fromRight systemPluginId (mkPluginId ns)
    _ -> systemPluginId

-- | List difference by content equality (Def 3.12).
-- Returns elements of @xs@ whose content does not appear in @ys@.
factListDiff
  :: [Fact 'NormalFact]
  -> [Fact 'NormalFact]
  -> [Fact 'NormalFact]
factListDiff xs ys =
  let yContents = Set.fromList (map factContent ys)
  in filter (\f -> Set.notMember (factContent f) yContents) xs

-- | Elapsed wall-clock time in milliseconds as a 'Natural'.
-- 'diffUTCTime' returns a 'NominalDiffTime' measured in seconds (not
-- picoseconds); multiplying by 1000 gives milliseconds (LOGIC-08).
diffTimeMs :: UTCTime -> UTCTime -> Natural
diffTimeMs start end =
  let seconds = toRational (diffUTCTime end start)
      ms      = seconds * 1000
  in max 0 (round ms)

-- ---------------------------------------------------------------------------
-- Provenance graph

-- | Build a minimal directed graph of FactNode → FactNode 'DerivedFrom'
-- edges for the post-loop cycle check (spec §13.3).
--
-- Only FactNodes and DerivedFrom edges are included. Rule and evidence
-- nodes are irrelevant for detecting cycles in the *fact derivation* graph.
buildProvenanceGraph :: [Evidence] -> Graph
buildProvenanceGraph = foldr addEvidenceEdges emptyGraph
  where
    addEvidenceEdges ev g =
      let derivedNode = FactNode (derivedFact ev)
          g' = addNode (GraphNode derivedNode "" NodeActive) g
      in foldr
           (\pid acc ->
             let parentNode = FactNode pid
             in addEdge
                  (GraphEdge parentNode derivedNode DerivedFrom)
                  (addNode (GraphNode parentNode "" NodeActive) acc))
           g'
           (toList (parentFacts ev))

-- ---------------------------------------------------------------------------
-- Summary

-- | Produce an 'InferenceSummary' from live state and elapsed time.
-- Used by the 'WarnAndContinue' path; Runtime builds a summary from the
-- final 'InferenceResult' for 'EvInferenceCompleted'.
stateToSummary :: InferenceState -> Natural -> InferenceSummary
stateToSummary state elapsed = InferenceSummary
  { isFinalFactCount = fromIntegral (Map.size (isFactMap state))
  , isDerivedCount   = fromIntegral (Map.size (isEvidence state))
  , isTermination    = case isLimitBreached state of
                         Just r  -> LimitReached r
                         Nothing -> FixedPoint
  , isIterations     = isDepth state
  , isElapsedMs      = elapsed
  }
