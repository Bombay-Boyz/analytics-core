-- | Analytics.Core.KnowledgeBase
--
-- Phantom-typed KB handles, mutable state, and all KB operations.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Rule
--   Analytics.Core.Evidence
--   Analytics.Core.Storage
--   Analytics.Core.Event.Types  (for KBError referencing FactValidationError)
--   ← this module

module Analytics.Core.KnowledgeBase
  ( -- * Access-level phantoms
    ReadOnly
  , ReadWrite
    -- * Handle
  , KBHandle(..)
  , newKBHandle
  , roHandle
    -- * Internal state (exposed for Runtime and Inference)
  , KBState(..)
  , KBSnapshot(..)
    -- * Errors
  , KBError(..)
    -- * Operations
  , kbInsertFact
  , kbInsertFacts
  , kbRetractFact
  , kbResolveContradiction
  , kbLookupFact
  , kbQueryFacts
  , kbInsertRule
  , kbDisableRule
  , kbLookupRule
  , kbQueryRules
  , kbCurrentSnapshot
  , kbAtSnapshot
    -- * Internal helpers (used by Inference and Runtime)
  , findCascadeTargets
  , findExclusivelyDerivedFacts
  , kbAddEvidence
  ) where

import Relude
import Data.Kind ()
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  , FactContent
  , factContent
  , factRecordId
  , factRecordAttrs
  , factRecordType
  , validateFact
  , FactValidationError
  )
import Analytics.Core.Rule
  ( Rule(..)
  , RuleValidationError
  , validateRule
  , unPriority
  )
import Analytics.Core.Evidence (Evidence(..))
import Analytics.Core.Storage
  ( FactQuery(..)
  , LifecycleFilter(..)
  , SourceFilter(..)
  , RuleQuery(..)
  , unPriorityRange
  )

-- ---------------------------------------------------------------------------
-- Access-level phantoms
--
-- KBHandleRW wraps a TVar — writes are possible.
-- KBHandleRO wraps a pure KBSnapshot — writes are structurally impossible
-- because there is no TVar to write to. coerce / unsafeCoerce cannot produce
-- a ReadWrite from a ReadOnly because the underlying data constructors differ.

data ReadOnly
data ReadWrite

data KBHandle (access :: Type) where
  KBHandleRW :: !(TVar KBState) -> KBHandle ReadWrite
  KBHandleRO :: !KBSnapshot     -> KBHandle ReadOnly

-- ---------------------------------------------------------------------------
-- Internal state

-- | kbEvidence is keyed by derivedFact for O(log n) lookups (LOGIC-02).
-- kbActiveFacts is maintained incrementally; never recomputed from kbFacts.
data KBState = KBState
  { kbFacts        :: !(Map FactId (Fact 'NormalFact))
  , kbRules        :: !(Map RuleId Rule)
  , kbEvidence     :: !(Map FactId (NonEmpty Evidence))
  , kbSnapshots    :: !(Map SnapshotId KBSnapshot)
    -- ^ Bounded ring of recent snapshots. The oldest entry is evicted when
    -- 'Map.size kbSnapshots' would exceed 'kbMaxSnapshots'. Callers that
    -- hold a 'SnapshotId' older than the window receive 'KBSnapshotNotFound'.
  , kbCurrentSnap  :: !SnapshotId
  , kbActiveFacts  :: !(Set FactId)
  , kbContentIndex :: !(Set FactContent)   -- O(1) idempotency checks
  , kbEnabledRules :: !(Set RuleId)        -- subset of kbRules that are enabled
  , kbMaxSnapshots :: !Int
    -- ^ Maximum number of snapshots retained. Must be >= 1.
    -- Supplied by the caller at construction time via 'newKBHandle'.
  }

data KBSnapshot = KBSnapshot
  { snapFacts         :: !(Map FactId (Fact 'NormalFact))
  , snapRules         :: !(Map RuleId Rule)
  , snapActiveFacts   :: !(Set FactId)   -- ^ active-fact set captured at snapshot time
  , snapEnabledRules  :: !(Set RuleId)   -- ^ enabled-rule set captured at snapshot time
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Errors

data KBError
  = KBDuplicateFactId     !FactId
  | KBFactNotFound        !FactId
  | KBRuleNotFound        !RuleId
  | KBInvalidFact         !(NonEmpty FactValidationError)
  | KBInvalidFacts        !(NonEmpty (FactId, NonEmpty FactValidationError))
  -- ^ batch variant: all invalid facts with their errors, reported together
  | KBInvalidRule         !(NonEmpty RuleValidationError)
  | KBSnapshotNotFound    !SnapshotId
  | KBSnapshotIdOverflow
  | KBDuplicateRuleId     !RuleId
  deriving stock (Show, Eq)

-- | Unrecoverable fault — only used for SnapshotId overflow inside STM
-- where we cannot return an Either.
newtype KBFaultException = KBFaultException Text
  deriving stock (Show)
instance Exception KBFaultException

-- ---------------------------------------------------------------------------
-- Construction

-- | Create a fresh, empty KB handle.
--
-- 'maxSnapshots' bounds the number of historical snapshots retained.
-- When a new snapshot would exceed the limit, the oldest entry is evicted.
-- Must be >= 1; values below 1 are clamped to 1.
-- The Runtime supplies this from 'RuntimeConfig.rcMaxSnapshots'.
newKBHandle :: Int -> IO (KBHandle ReadWrite)
newKBHandle maxSnaps = do
  tv <- newTVarIO KBState
    { kbFacts        = Map.empty
    , kbRules        = Map.empty
    , kbEvidence     = Map.empty
    , kbSnapshots    = Map.empty
    , kbCurrentSnap  = initialSnapshotId
    , kbActiveFacts  = Set.empty
    , kbContentIndex = Set.empty
    , kbEnabledRules = Set.empty
    , kbMaxSnapshots = max 1 maxSnaps
    }
  pure (KBHandleRW tv)

-- | Take a consistent read-only snapshot of the current KB state.
roHandle :: KBHandle ReadWrite -> IO (KBHandle ReadOnly)
roHandle (KBHandleRW tv) = do
  st <- readTVarIO tv
  pure $ KBHandleRO KBSnapshot
    { snapFacts        = kbFacts st
    , snapRules        = kbRules st
    , snapActiveFacts  = kbActiveFacts st
    , snapEnabledRules = kbEnabledRules st
    }

-- ---------------------------------------------------------------------------
-- Snapshot helpers

-- | Advance the snapshot counter inside STM, storing a new KBSnapshot.
-- Evicts the oldest snapshot when the retention window is full.
-- Returns Left on overflow (caller should throwIO KBFaultException).
commitSnapshot :: KBState -> Either Text (KBState, SnapshotId)
commitSnapshot st =
  case nextSnapshotId (kbCurrentSnap st) of
    Left msg  -> Left msg
    Right sid ->
      let snap = KBSnapshot
            { snapFacts        = kbFacts st
            , snapRules        = kbRules st
            , snapActiveFacts  = kbActiveFacts st
            , snapEnabledRules = kbEnabledRules st
            }
          inserted = Map.insert sid snap (kbSnapshots st)
          -- Evict the oldest entry (minimum SnapshotId = lowest Word64)
          -- whenever the map exceeds the configured retention window.
          pruned
            | Map.size inserted > kbMaxSnapshots st =
                Map.deleteMin inserted
            | otherwise =
                inserted
          st' = st
            { kbCurrentSnap = sid
            , kbSnapshots   = pruned
            }
      in Right (st', sid)

-- ---------------------------------------------------------------------------
-- Fact operations

-- | Insert a single asserted fact. Idempotent on content (Def 3.12):
-- a fact whose type+attributes already exist in the active set is silently
-- accepted and the existing SnapshotId is returned.
kbInsertFact
  :: Fact 'NormalFact
  -> KBHandle ReadWrite
  -> IO (Either KBError SnapshotId)
kbInsertFact f (KBHandleRW tv) = do
  case validateFact f of
    Left errs -> pure (Left (KBInvalidFact errs))
    Right ()  -> atomically $ do
      st <- readTVar tv
      let fid = factRecordId f
      -- Duplicate FactId check.
      if Map.member fid (kbFacts st)
        then pure (Left (KBDuplicateFactId fid))
        else
          -- Idempotency: if content already active, return current snapshot.
          if Set.member (factContent f) (kbContentIndex st)
            then pure (Right (kbCurrentSnap st))
            else case commitSnapshot st of
              Left msg  -> throwSTM (KBFaultException msg)
              Right (st', sid) ->
                let st'' = st'
                      { kbFacts        = Map.insert fid f (kbFacts st')
                      , kbActiveFacts  = Set.insert fid (kbActiveFacts st')
                      , kbContentIndex = Set.insert (factContent f) (kbContentIndex st')
                      }
                in writeTVar tv st'' $> Right sid

-- | Atomically insert a non-empty batch of facts.
-- The entire batch is committed under a single SnapshotId.
kbInsertFacts
  :: NonEmpty (Fact 'NormalFact)
  -> KBHandle ReadWrite
  -> IO (Either KBError SnapshotId)
kbInsertFacts facts (KBHandleRW tv) = do
  -- Validate all facts before touching the TVar; collect *all* errors.
  let errs = [ (factRecordId f, es)
             | f <- toList facts
             , Left es <- [validateFact f] ]
  case nonEmpty errs of
    Just batch -> pure (Left (KBInvalidFacts batch))
    Nothing    -> atomically $ do
      st <- readTVar tv
      -- Check for duplicate FactIds within the batch or against existing KB.
      let newIds    = fmap factRecordId facts
          idList    = toList newIds
          idSet     = Set.fromList idList
          intraDup  = if Set.size idSet < length idList
                        then listToMaybe
                               [ fid
                               | (fid, cnt) <- Map.toList
                                   (Map.fromListWith (+) (map (, 1 :: Int) idList))
                               , cnt > 1
                               ]
                        else Nothing
          allExist  = filter (\fid -> Map.member fid (kbFacts st)) idList
      case intraDup <|> listToMaybe allExist of
        Just dup -> pure (Left (KBDuplicateFactId dup))
        Nothing  ->
          -- Filter out content-duplicates (idempotency).
          let genuinelyNew = filter
                (\f -> not (Set.member (factContent f) (kbContentIndex st)))
                (toList facts)
          in case genuinelyNew of
               [] -> pure (Right (kbCurrentSnap st))
               _  -> case commitSnapshot st of
                 Left msg  -> throwSTM (KBFaultException msg)
                 Right (st', sid) ->
                   let st'' = foldr insertOne st' genuinelyNew
                   in writeTVar tv st'' $> Right sid
  where
    insertOne f s = s
      { kbFacts        = Map.insert (factRecordId f) f (kbFacts s)
      , kbActiveFacts  = Set.insert (factRecordId f) (kbActiveFacts s)
      , kbContentIndex = Set.insert (factContent f)  (kbContentIndex s)
      }

-- | Logically retract a fact and cascade to all derived descendants (§9.5).
kbRetractFact
  :: FactId
  -> KBHandle ReadWrite
  -> IO (Either KBError SnapshotId)
kbRetractFact fid (KBHandleRW tv) = atomically $ do
  st <- readTVar tv
  case Map.lookup fid (kbFacts st) of
    Nothing -> pure (Left (KBFactNotFound fid))
    Just _ ->
      let cascade    = findCascadeTargets fid (kbFacts st) (kbActiveFacts st)
          retracted  = Set.insert fid cascade
          newActive  = Set.difference (kbActiveFacts st) retracted
          -- Remove retracted facts from the content index.
          retractedContents = Set.fromList
            [ factContent f'
            | rid <- Set.toList retracted
            , Just f' <- [Map.lookup rid (kbFacts st)]
            ]
          newContentIdx = Set.difference (kbContentIndex st) retractedContents
      in case commitSnapshot st of
           Left msg  -> throwSTM (KBFaultException msg)
           Right (st', sid) ->
             let st'' = st'
                   { kbActiveFacts  = newActive
                   , kbContentIndex = newContentIdx
                   , kbEvidence     = Map.withoutKeys (kbEvidence st) retracted
                     -- ^ Prune evidence for all retracted facts (root + cascade).
                     -- findExclusivelyDerivedFacts queries kbEvidence; stale entries
                     -- for logically-retracted facts would cause over-retraction on
                     -- kbDisableRule. Retraction is logical (facts remain in kbFacts)
                     -- so the Map.member guard in findExclusivelyDerivedFacts is not
                     -- sufficient protection against this.
                   }
             in writeTVar tv st'' $> Right sid

-- | Atomically retract two contradicting facts and insert one resolved fact.
--
-- All three mutations occur under a single STM commit: no intermediate state
-- is ever observable by concurrent readers. This is the only correct way to
-- perform contradiction resolution — the three-operation sequence that
-- previously appeared in Runtime.handleMatch was non-atomic and left the KB
-- in an inconsistent state if an async exception arrived between any two ops.
--
-- The resolved fact is validated before entering STM (validation is pure and
-- does not touch the TVar). Both retraction targets are verified to exist
-- inside STM before any state is modified: if either is absent, the whole
-- operation returns 'Left KBFactNotFound' with no side effects.
--
-- Returns 'Left KBFactNotFound' if either retraction target is absent.
-- Returns 'Left KBInvalidFact' if the resolved fact fails validation.
-- On success, returns 'Right' with the committed 'SnapshotId'.
--
-- Note: this function intentionally does NOT cascade-retract derived children
-- of fid1/fid2. Contradiction resolution replaces two contradicting facts with
-- a single resolved fact; cascading is the caller's responsibility if needed.
-- The two retracted FactIds are removed from kbActiveFacts and kbContentIndex
-- but remain in kbFacts (logical retraction, consistent with kbRetractFact).
-- Their evidence entries are pruned for the same reason as kbRetractFact.
kbResolveContradiction
  :: FactId             -- ^ First contradicting fact to retract
  -> FactId             -- ^ Second contradicting fact to retract
  -> Fact 'NormalFact   -- ^ Resolved fact to insert (caller constructs with mkAssertedFact)
  -> KBHandle ReadWrite
  -> IO (Either KBError SnapshotId)
kbResolveContradiction fid1 fid2 resolved (KBHandleRW tv) = do
  -- Validate outside STM: validation is pure; no reason to hold the TVar
  -- lock during a computation that does not read or write shared state.
  case validateFact resolved of
    Left errs -> pure (Left (KBInvalidFact errs))
    Right ()  -> atomically $ do
      st <- readTVar tv
      -- Verify both retraction targets exist before touching any state.
      -- Using a nested case rather than guards so GHC can verify exhaustiveness.
      case (Map.lookup fid1 (kbFacts st), Map.lookup fid2 (kbFacts st)) of
        (Nothing, _) -> pure (Left (KBFactNotFound fid1))
        (_, Nothing) -> pure (Left (KBFactNotFound fid2))
        (Just f1, Just f2) ->
          let retracted      = Set.fromList [fid1, fid2]
              -- Remove both retracted facts from the active set and content index.
              retractedConts = Set.fromList [factContent f1, factContent f2]
              newActive      = Set.difference (kbActiveFacts st) retracted
              newContentIdx0 = Set.difference (kbContentIndex st) retractedConts
              rfid           = factRecordId resolved
              -- Insert the resolved fact's content into the index.
              newContentIdx1 = Set.insert (factContent resolved) newContentIdx0
          in case commitSnapshot st of
               Left msg     -> throwSTM (KBFaultException msg)
               Right (st', sid) ->
                 let st'' = st'
                       { kbFacts        = Map.insert rfid resolved (kbFacts st')
                       , kbActiveFacts  = Set.insert rfid newActive
                       , kbContentIndex = newContentIdx1
                       , kbEvidence     = Map.withoutKeys (kbEvidence st) retracted
                         -- ^ Prune evidence for the two retracted facts.
                         -- Same invariant as kbRetractFact: stale evidence entries
                         -- for logically-retracted facts cause over-retraction in
                         -- findExclusivelyDerivedFacts / kbDisableRule.
                       }
                 in writeTVar tv st'' $> Right sid

-- | Look up a fact by ID. Works on both ReadOnly and ReadWrite handles.
kbLookupFact
  :: FactId
  -> KBHandle access
  -> IO (Maybe (Fact 'NormalFact))
kbLookupFact fid (KBHandleRW tv) = do
  st <- readTVarIO tv
  pure (Map.lookup fid (kbFacts st))
kbLookupFact fid (KBHandleRO snap) =
  pure (Map.lookup fid (snapFacts snap))

-- | Query facts. Works on both ReadOnly and ReadWrite handles.
kbQueryFacts
  :: FactQuery
  -> KBHandle access
  -> IO [Fact 'NormalFact]
kbQueryFacts q handle = do
  (allFacts, activeIds) <- case handle of
    KBHandleRW tv -> do
      -- Single read: allFacts and activeIds are always consistent with each
      -- other. A concurrent kbRetractFact between two separate readTVarIO
      -- calls could produce a fact list and an active-set from different
      -- committed states, yielding incorrect lifecycle-filter results.
      st <- readTVarIO tv
      pure (Map.elems (kbFacts st), kbActiveFacts st)
    KBHandleRO sn ->
      -- KBHandleRO wraps a pure KBSnapshot; both fields are already consistent.
      pure (Map.elems (snapFacts sn), snapActiveFacts sn)
  let filtered = filter (applyFactQuery q activeIds) allFacts
  pure $ case fqLimit q of
    Nothing  -> filtered
    Just lim -> take (fromIntegral lim) filtered

applyFactQuery :: FactQuery -> Set FactId -> Fact 'NormalFact -> Bool
applyFactQuery q activeIds f =
  checkLifecycle && checkType && checkAttrs && checkSource
  where
    fid = factRecordId f
    checkLifecycle = case fqLifecycle q of
      ActiveFacts        -> Set.member    fid activeIds
      RetractedFacts     -> Set.notMember fid activeIds
      AllLifecycleStates -> True
    checkType = case fqType q of
      Nothing -> True
      Just t  -> factRecordType f == t
    checkAttrs =
      all (\(k, v) -> Map.lookup k (factRecordAttrs f) == Just v)
          (Map.toList (fqAttributes q))
    checkSource = case fqSource q of
      Nothing          -> True
      Just OnlyAsserted -> case f of
        Asserted {} -> True
        _           -> False
      Just OnlyDerived  -> case f of
        Derived {} -> True
        _          -> False
      Just (DerivedByRule rid) -> case f of
        Derived _ _ _ _ r _ -> r == rid
        _                   -> False

-- ---------------------------------------------------------------------------
-- Rule operations

-- | Insert and enable a rule.
kbInsertRule
  :: Rule
  -> KBHandle ReadWrite
  -> IO (Either KBError SnapshotId)
kbInsertRule r (KBHandleRW tv) =
  case validateRule r of
    Left errs -> pure (Left (KBInvalidRule errs))
    Right ()  -> atomically $ do
      st <- readTVar tv
      if Map.member (ruleId r) (kbRules st)
        then pure (Left (KBDuplicateRuleId (ruleId r)))
        else case commitSnapshot st of
          Left msg  -> throwSTM (KBFaultException msg)
          Right (st', sid) ->
            let st'' = st'
                  { kbRules        = Map.insert (ruleId r) r (kbRules st')
                  , kbEnabledRules = Set.insert (ruleId r) (kbEnabledRules st')
                  }
            in writeTVar tv st'' $> Right sid

-- | Disable a rule and retract all facts exclusively derived by it (§11.2.1).
-- Returns the disabled Rule, the SnapshotId, and the list of retracted FactIds.
-- The Rule is returned so callers can include its PluginId in EvRuleDisabled
-- without a separate KB lookup.
kbDisableRule
  :: RuleId
  -> KBHandle ReadWrite
  -> IO (Either KBError (Rule, SnapshotId, [FactId]))
kbDisableRule rid (KBHandleRW tv) = atomically $ do
  st <- readTVar tv
  case Map.lookup rid (kbRules st) of
    Nothing   -> pure (Left (KBRuleNotFound rid))
    Just rule ->
      let toRetract = findExclusivelyDerivedFacts rid (kbFacts st) (kbEvidence st)
          retractedContents = Set.fromList
            [ factContent f
            | fid <- toRetract
            , Just f <- [Map.lookup fid (kbFacts st)]
            ]
          newActive     = Set.difference (kbActiveFacts st)  (Set.fromList toRetract)
          newContentIdx = Set.difference (kbContentIndex st) retractedContents
      in case commitSnapshot st of
           Left msg  -> throwSTM (KBFaultException msg)
           Right (st', sid) ->
             let st'' = st'
                   { kbEnabledRules = Set.delete rid (kbEnabledRules st')
                   , kbActiveFacts  = newActive
                   , kbContentIndex = newContentIdx
                   , kbEvidence     = Map.withoutKeys (kbEvidence st) (Set.fromList toRetract)
                     -- ^ Prune evidence for facts being logically retracted here.
                     -- Same reasoning as kbRetractFact: stale evidence entries
                     -- would cause over-retraction in findExclusivelyDerivedFacts.
                   }
             in writeTVar tv st'' $> Right (rule, sid, toRetract)

-- | Look up a rule by ID.
kbLookupRule
  :: RuleId
  -> KBHandle access
  -> IO (Maybe Rule)
kbLookupRule rid (KBHandleRW tv) = do
  st <- readTVarIO tv
  pure (Map.lookup rid (kbRules st))
kbLookupRule rid (KBHandleRO snap) =
  pure (Map.lookup rid (snapRules snap))

-- | Query rules.
kbQueryRules
  :: RuleQuery
  -> KBHandle access
  -> IO [Rule]
kbQueryRules q handle = do
  (allRules, enabledIds) <- case handle of
    KBHandleRW tv -> do
      -- Single read: allRules and enabledIds must come from the same committed
      -- state. A concurrent kbDisableRule between two separate readTVarIO calls
      -- could yield a rule list and an enabled-set from different states,
      -- producing incorrect enabled/disabled filter results. Identical reasoning
      -- to kbQueryFacts (Blocker 4).
      st <- readTVarIO tv
      pure (Map.elems (kbRules st), kbEnabledRules st)
    KBHandleRO sn ->
      pure (Map.elems (snapRules sn), snapEnabledRules sn)
  pure (filter (applyRuleQuery q enabledIds) allRules)

applyRuleQuery :: RuleQuery -> Set RuleId -> Rule -> Bool
applyRuleQuery q enabledIds r =
  checkPlugin && checkEnabled && checkPriority
  where
    checkPlugin = case rqPluginId q of
      Nothing  -> True
      Just pid -> rulePluginId r == pid
    checkEnabled = case rqEnabled q of
      Nothing    -> True
      Just True  -> Set.member    (ruleId r) enabledIds
      Just False -> Set.notMember (ruleId r) enabledIds
    checkPriority = case rqPriority q of
      Nothing -> True
      Just pr ->
        let (lo, hi) = unPriorityRange pr
            rp       = unPriority (priority r)
        in rp >= lo && rp <= hi

-- ---------------------------------------------------------------------------
-- Snapshot operations

kbCurrentSnapshot :: KBHandle ReadWrite -> IO SnapshotId
kbCurrentSnapshot (KBHandleRW tv) = kbCurrentSnap <$> readTVarIO tv

-- | Retrieve a read-only handle at a past snapshot.
kbAtSnapshot
  :: SnapshotId
  -> KBHandle ReadWrite
  -> IO (Either KBError (KBHandle ReadOnly))
kbAtSnapshot sid (KBHandleRW tv) = do
  st <- readTVarIO tv
  case Map.lookup sid (kbSnapshots st) of
    Nothing   -> pure (Left (KBSnapshotNotFound sid))
    Just snap -> pure (Right (KBHandleRO snap))

-- ---------------------------------------------------------------------------
-- Evidence

-- | Add evidence for a derived fact. Called by the inference engine.
kbAddEvidence
  :: Evidence
  -> KBHandle ReadWrite
  -> IO ()
kbAddEvidence ev (KBHandleRW tv) =
  atomically $ modifyTVar' tv $ \st ->
    st { kbEvidence = Map.insertWith (<>) (derivedFact ev) (ev :| []) (kbEvidence st) }

-- ---------------------------------------------------------------------------
-- Internal helpers

-- | BFS over active facts, collecting all active derived descendants of root.
-- Used by kbRetractFact for cascading retraction (§9.5).
findCascadeTargets
  :: FactId
  -> Map FactId (Fact 'NormalFact)
  -> Set FactId
  -> Set FactId
findCascadeTargets root factMap activeFacts =
  go (Set.singleton root) Set.empty
  where
    go frontier visited
      | Set.null frontier = visited
      | otherwise =
          let children    = Map.foldlWithKey' (collectChildren frontier) Set.empty factMap
              newVisited  = Set.union visited frontier
              newFrontier = Set.difference children newVisited
          in go newFrontier newVisited
    collectChildren frontier acc fid fact =
      case fact of
        Derived _ _ _ parents _ _ ->
          if any (`Set.member` frontier) (toList parents) && Set.member fid activeFacts
          then Set.insert fid acc
          else acc
        Asserted {} -> acc

-- | Identify facts to retract when a rule is disabled (§11.2.1).
-- A fact is exclusively derived by rid iff ALL evidence records for it
-- name rid as their ruleUsed.
findExclusivelyDerivedFacts
  :: RuleId
  -> Map FactId (Fact 'NormalFact)
  -> Map FactId (NonEmpty Evidence)
  -> [FactId]
findExclusivelyDerivedFacts rid factMap evByFact =
  [ fid
  | (fid, evs) <- Map.toList evByFact
  , Map.member fid factMap
  , all (\ev -> ruleUsed ev == rid) (toList evs)
  ]