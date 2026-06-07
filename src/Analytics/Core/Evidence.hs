module Analytics.Core.Evidence
  ( Evidence(..)
  , mkEvidence
  , EvidenceValidationError(..)
  , validateEvidence
  ) where

import Relude
import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map
import Analytics.Core.Types

-- ---------------------------------------------------------------------------
-- Evidence record

data Evidence = Evidence
  { evidenceId  :: !EvidenceId
  , derivedFact :: !FactId
  , parentFacts :: !(NonEmpty FactId)
  , ruleUsed    :: !RuleId
  , rulePlugin  :: !PluginId
    -- ^ The plugin that owns the rule that produced this derivation.
    -- Stored here so event subscribers can match 'EvRuleFired' by plugin
    -- without a KB lookup at dispatch time.
  , binding     :: !Binding
  , timestamp   :: !UTCTime
  , snapshotId  :: !SnapshotId
  , depth       :: !Natural
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Depth computation

-- depth(f') = max(depth(parents)) + 1
computeDepth :: NonEmpty Natural -> Natural
computeDepth (x :| xs) = foldl' max x xs + 1

-- ---------------------------------------------------------------------------
-- Smart constructor

mkEvidence
  :: EvidenceId
  -> FactId
  -> NonEmpty FactId
  -> RuleId
  -> PluginId
  -> Binding
  -> UTCTime
  -> SnapshotId
  -> NonEmpty Natural   -- depths of parent facts, 1:1 with parentFacts
  -> Evidence
mkEvidence eid dfid parents rid rpid bind ts snap parentDepths = Evidence
  { evidenceId  = eid
  , derivedFact = dfid
  , parentFacts = parents
  , ruleUsed    = rid
  , rulePlugin  = rpid
  , binding     = bind
  , timestamp   = ts
  , snapshotId  = snap
  , depth       = computeDepth parentDepths
  }

-- ---------------------------------------------------------------------------
-- Validation

data EvidenceValidationError
  = UnknownParentFact FactId
  | MismatchedParentCount Int Int   -- expected, actual
  deriving stock (Show, Eq)

-- | Validate that all parent FactIds exist in the provided depth index.
-- LOGIC-05: returns UnknownParentFact for any parent absent from the index;
-- the silent fromMaybe 0 default is removed.
validateEvidence
  :: Map FactId Natural   -- depth index: all known active facts
  -> Evidence
  -> Either (NonEmpty EvidenceValidationError) ()
validateEvidence depthIndex ev =
  let missing = [ fid | fid <- toList (parentFacts ev)
                       , not (Map.member fid depthIndex) ]
  in case nonEmpty missing of
       Nothing  -> Right ()
       Just nel -> Left (fmap UnknownParentFact nel)