module Analytics.Core.Storage
  ( -- * Backend typeclass
    StorageBackend(..)
  , withStorage
    -- * Errors
  , StorageError(..)
    -- * Configuration
  , StorageConfig(..)
    -- * Fact queries
  , FactQuery(..)
  , emptyFactQuery
  , SourceFilter(..)
  , LifecycleFilter(..)
    -- * Rule queries
  , RuleQuery(..)
  , emptyRuleQuery
    -- * Priority range
  , PriorityRange(..)
  , mkPriorityRange
  , unPriorityRange
    -- * Typed read helpers
  , readNormalFacts
  , readRetractionFacts
  ) where

import Relude
import Control.Exception (bracket)

import Analytics.Core.Types
import Analytics.Core.Fact
  ( AnyFact(..)
  , Fact(..)
  , FactKind(..)
  )
import Analytics.Core.Rule     (Rule)
import Analytics.Core.Evidence (Evidence)

-- ---------------------------------------------------------------------------
-- Fact query

data FactQuery = FactQuery
  { fqType       :: !(Maybe Text)
    -- ^ Restrict to facts with this exact namespaced type.
  , fqAttributes :: !Attributes
    -- ^ All listed key/value pairs must be present. Empty map = no filter.
  , fqSource     :: !(Maybe SourceFilter)
    -- ^ Restrict by provenance; Nothing = no restriction.
  , fqLifecycle  :: !LifecycleFilter
    -- ^ Restrict by lifecycle status.
  , fqLimit      :: !(Maybe Natural)
    -- ^ Cap the result set. Nothing = no limit.
  } deriving stock (Show, Eq)

-- | Sensible default: all active normal facts, no filters, no limit.
-- Narrow with record update: emptyFactQuery { fqType = Just "sensor:reading" }
emptyFactQuery :: FactQuery
emptyFactQuery = FactQuery
  { fqType       = Nothing
  , fqAttributes = mempty
  , fqSource     = Nothing
  , fqLifecycle  = ActiveFacts
  , fqLimit      = Nothing
  }

data SourceFilter
  = OnlyAsserted
  | OnlyDerived
  | DerivedByRule !RuleId
  deriving stock (Show, Eq)

data LifecycleFilter
  = ActiveFacts
  | RetractedFacts
  | AllLifecycleStates
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Rule query

data RuleQuery = RuleQuery
  { rqPluginId :: !(Maybe PluginId)
  , rqEnabled  :: !(Maybe Bool)
  , rqPriority :: !(Maybe PriorityRange)
  } deriving stock (Show, Eq)

emptyRuleQuery :: RuleQuery
emptyRuleQuery = RuleQuery
  { rqPluginId = Nothing
  , rqEnabled  = Nothing
  , rqPriority = Nothing
  }

-- ---------------------------------------------------------------------------
-- PriorityRange
--
-- Both bounds inclusive. Stored as raw Int to avoid a dependency cycle with
-- Rule.Priority; callers use unPriority when constructing from a Priority.

data PriorityRange = PriorityRange !Int !Int
  deriving stock (Show, Eq)

mkPriorityRange :: Int -> Int -> Either Text PriorityRange
mkPriorityRange lo hi
  | lo <= hi  = Right (PriorityRange lo hi)
  | otherwise = Left $ "Invalid priority range: lo=" <> show lo <> " > hi=" <> show hi

unPriorityRange :: PriorityRange -> (Int, Int)
unPriorityRange (PriorityRange lo hi) = (lo, hi)

-- ---------------------------------------------------------------------------
-- StorageBackend typeclass
--
-- Design decisions vs the original:
--
-- * openStorage/closeStorage are NOT part of the public API — use withStorage.
--   The old open/close pair forced callers to manage resource safety manually.
--
-- * readFacts returns [AnyFact] because the storage layer does not know the
--   kind of a fact at query time — that is encoded in the data. Use the typed
--   helpers readNormalFacts / readRetractionFacts to filter after the fetch.
--
-- * Writes are split by kind: writeNormalFact / writeRetractionFact.
--   A backend can route them to different tables or streams without a runtime
--   type check. There is no untyped writeFact that silently accepts anything.
--
-- * deleteFact is renamed markFactRetracted — facts are marked, not destroyed.

class StorageBackend s where

  -- Facts
  readFacts
    :: FactQuery -> s
    -> IO (Either StorageError [AnyFact])

  writeNormalFact
    :: Fact 'NormalFact -> s
    -> IO (Either StorageError ())

  writeNormalFactBatch
    :: NonEmpty (Fact 'NormalFact) -> s
    -> IO (Either StorageError ())

  writeRetractionFact
    :: Fact 'RetractionFact -> s
    -> IO (Either StorageError ())

  -- Rules
  readRules
    :: RuleQuery -> s
    -> IO (Either StorageError [Rule])

  writeRule
    :: Rule -> s
    -> IO (Either StorageError ())

  -- Evidence
  readEvidence
    :: FactId -> s
    -> IO (Either StorageError [Evidence])

  writeEvidence
    :: Evidence -> s
    -> IO (Either StorageError ())

  -- Snapshots
  checkpoint
    :: s
    -> IO (Either StorageError SnapshotId)

  readAtSnapshot
    :: SnapshotId -> s
    -> IO (Either StorageError s)

  -- Retraction / cleanup
  markFactRetracted
    :: FactId -> s
    -> IO (Either StorageError ())

  deleteEvidence
    :: FactId -> s
    -> IO (Either StorageError ())

  -- Internal lifecycle — use withStorage, not these directly
  openStorage  :: StorageConfig -> IO (Either StorageError s)
  closeStorage :: s -> IO ()

-- ---------------------------------------------------------------------------
-- Resource-safe acquisition
--
-- The only sanctioned way to obtain a storage handle. bracket guarantees
-- closeStorage runs even if the action throws. Never call openStorage or
-- closeStorage directly.

withStorage
  :: StorageBackend s
  => StorageConfig
  -> (s -> IO (Either StorageError a))
  -> IO (Either StorageError a)
withStorage cfg action =
  openStorage cfg >>= \case
    Left  err     -> pure (Left err)
    Right backend -> bracket (pure backend) closeStorage action
    -- action returns IO (Either StorageError a) directly.
    -- No fmap Right: openStorage Left and action Left are both Left.

-- ---------------------------------------------------------------------------
-- Typed read helpers
--
-- Pure post-processing over readFacts — no extra IO round-trip.
--
-- The challenge: AnyFact is an existential (forall k. Fact k), so when we
-- open it, GHC assigns a fresh skolem variable to k. We cannot return that
-- skolem in a list — the type of the list would be ambiguous across branches.
-- The solution is to never bind `f` across the case; instead each branch
-- that wants to keep the fact reconstructs it from its known concrete type,
-- which GHC can check against the declared return type.

asNormalFact :: AnyFact -> Maybe (Fact 'NormalFact)
asNormalFact (AnyFact f) = case f of
  Asserted  fid t a ts         -> Just (Asserted  fid t a ts)
  Derived   fid t a ps rid ts  -> Just (Derived   fid t a ps rid ts)
  Retraction {}                -> Nothing

asRetractionFact :: AnyFact -> Maybe (Fact 'RetractionFact)
asRetractionFact (AnyFact f) = case f of
  Asserted  {}                 -> Nothing
  Derived   {}                 -> Nothing
  Retraction fid tid ts        -> Just (Retraction fid tid ts)

readNormalFacts
  :: StorageBackend s
  => FactQuery -> s
  -> IO (Either StorageError [Fact 'NormalFact])
readNormalFacts q s = fmap (fmap (mapMaybe asNormalFact)) (readFacts q s)

readRetractionFacts
  :: StorageBackend s
  => FactQuery -> s
  -> IO (Either StorageError [Fact 'RetractionFact])
readRetractionFacts q s = fmap (fmap (mapMaybe asRetractionFact)) (readFacts q s)

-- ---------------------------------------------------------------------------
-- StorageError

data StorageError
  = SEConnectionFailed   !Text
  | SEWriteConflict      !Text
  | SEReadFailed         !Text
  | SESerializationError !Text
  | SESnapshotNotFound   !SnapshotId
  | SEBackendFull
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- StorageConfig

data StorageConfig = StorageConfig
  { scBackendType :: !Text
  , scConnectInfo :: !(Map Text Text)
  , scMaxConns    :: !Int
  } deriving stock (Show)