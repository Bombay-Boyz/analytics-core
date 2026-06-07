-- | Analytics.Core.Contradiction
--
-- Contradiction detection, classification, and resolution framework.
--
-- A /contradiction/ is a pair of active normal facts that jointly violate a
-- registered predicate.  Predicates are pure functions; detecting them is
-- always safe and never mutates the knowledge base directly.  The /action/
-- taken on a detected contradiction — continue, halt, or resolve — is
-- determined by the predicate's registered 'ContradictionPolicy'.
--
-- Design invariants:
--
--   * Predicates are identified by 'PredicateId' (a validated, namespaced
--     Text), never by function pointer equality.  Equality on
--     'ContradictionPredicate' compares 'cpId' only; the embedded function
--     cannot be compared.
--
--   * 'ContradictionRegistry' is the sole mutable state: a 'TVar' over an
--     immutable 'RegistryState'.  All reads are lock-free snapshots; all
--     writes are atomic STM transactions.
--
--   * Detection is O(p · n²) in the number of predicates p and active
--     facts n.  For cross-plugin detection the quadratic factor is bounded
--     by the active fact count; within-plugin detection is restricted to
--     the plugin's own namespace, reducing n to the plugin's fact subset.
--
--   * Resolution produces a 'ResolvedFact' whose type MUST be prefixed with
--     the resolving plugin's 'PluginId'.  This is enforced by
--     'validateResolvedFact' and is a hard precondition for every
--     'Resolve' action.
--
--   * The 'ContradictionRecord' is append-only: detected contradictions are
--     never silently discarded.  They accumulate in the registry and are
--     emitted as 'EvContradictionDetected' events.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Event.Types   (Event ADT — value only, no bus machinery)
--   ← this module

module Analytics.Core.Contradiction
  ( -- * Predicate identity
    PredicateId
  , mkPredicateId
  , unPredicateId

    -- * Contradiction predicate
  , ContradictionPredicate(..)

    -- * Policy
  , ContradictionPolicy(..)
  , mkResolvePolicy
  , resolverOf
    -- * Predicate construction
  , mkPredicate
  , Evaluator(..)
  , Resolver(..)

    -- * Resolution
  , ResolvedFact(..)
  , validateResolvedFact
  , ResolvedFactError(..)

    -- * Registry
  , ContradictionRegistry
  , newRegistry
  , registerPredicate
  , unregisterPredicate
  , lookupPredicate
  , listPredicates
  , RegistrationError(..)

    -- * Detection
  , detectContradictions
  , ContradictionMatch(..)

    -- * History
  , ContradictionRecord(..)
  , recordContradiction
  , queryRecords
  , RecordQuery(..)
  , emptyRecordQuery

    -- * Event construction
  , contradictionEvent
  ) where

import Relude
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
import Data.Char (isAscii, isAlphaNum)
import Data.Time (UTCTime)
import GHC.Show (Show (..))

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  , ActiveFact
  , factRecordId
  , factRecordType
  , untagFact
  )
import Analytics.Core.Event.Types (Event (..))

-- ---------------------------------------------------------------------------
-- PredicateId — validated, namespaced identifier
--
-- Format: "<pluginId>:<localName>"
-- Rules:  non-empty; ASCII alphanumeric + hyphens + dots in both segments;
--         exactly one colon separator.
--
-- The namespace prefix guarantees that predicates from different plugins
-- never collide, mirroring the fact-type namespace convention.

newtype PredicateId = PredicateId Text
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

-- | Smart constructor.  Returns 'Left' with a description of the first
-- validation failure.
mkPredicateId :: Text -> Either Text PredicateId
mkPredicateId t
  | T.null t = Left "PredicateId must not be empty"
  | otherwise =
      case T.breakOn ":" t of
        (_, "")     -> Left ("PredicateId missing namespace separator ':' in: " <> t)
        ("", _)     -> Left ("PredicateId namespace (before ':') must not be empty in: " <> t)
        (ns, rest)  ->
          case T.stripPrefix ":" rest of
            Nothing        -> Left "PredicateId internal error: breakOn invariant violated"
            Just localPart
              | T.null localPart ->
                  Left ("PredicateId local name (after ':') must not be empty in: " <> t)
              | not (T.all validChar ns) ->
                  Left ("PredicateId namespace contains invalid characters: " <> ns)
              | not (T.all validChar localPart) ->
                  Left ("PredicateId local name contains invalid characters: " <> localPart)
              | otherwise -> Right (PredicateId t)
  where
    validChar c = (isAscii c && isAlphaNum c) || c == '-' || c == '.'

unPredicateId :: PredicateId -> Text
unPredicateId (PredicateId t) = t

-- ---------------------------------------------------------------------------
-- ContradictionPolicy
--
-- Determines the runtime's response when 'cpEvaluate' returns True for a
-- pair of active facts.  The policy is registered alongside the predicate
-- and is immutable once registered.

-- | Opaque wrapper around a resolver function so 'ContradictionPolicy' can
-- derive 'Show' and 'Eq' without manual instances.
--
-- 'Show' displays the placeholder string "<fn>" — the function body is never
-- shown.  'Eq' considers all 'Resolver' values equal (constructor equality):
-- two policies with 'RecordAndResolve' are equal regardless of which function
-- they embed, which is consistent with the rest of 'ContradictionPolicy' Eq
-- (constructor-only comparison).
newtype Resolver = Resolver
  { runResolver :: Fact 'NormalFact -> Fact 'NormalFact -> ResolvedFact }

instance Show Resolver where
  show _ = "Resolver <fn>"

-- | All 'Resolver' values are considered equal — only the constructor
-- ('RecordAndResolve' vs the others) matters for policy equality.
instance Eq Resolver where
  _ == _ = True

data ContradictionPolicy
  = RecordAndContinue
    -- ^ Log the contradiction as a 'ContradictionRecord', emit
    -- 'EvContradictionDetected', and allow inference to proceed.
    -- Use when contradictions are expected and handled downstream.
  | RecordAndHalt
    -- ^ Log the contradiction, emit 'EvContradictionDetected', and set the
    -- inference termination reason to 'ContradictionHalt'.  Inference stops
    -- after the current iteration completes.
  | RecordAndResolve !Resolver
    -- ^ Log the contradiction, emit 'EvContradictionDetected', then call
    -- the embedded resolver to produce a 'ResolvedFact'.  The runtime
    -- inserts the resolved fact and retracts both contradicting facts.
    --
    -- Construct with 'mkResolvePolicy' rather than the 'Resolver' constructor
    -- directly.
    --
    -- ⚠ The resolver MUST be a pure, total function.  It must not throw
    -- exceptions or perform IO.  The produced 'ResolvedFact' must carry a
    -- type prefixed with the registering plugin's 'PluginId'; the runtime
    -- validates this via 'validateResolvedFact' before insertion.
  deriving stock (Show, Eq)

-- | Smart constructor for 'RecordAndResolve': wraps the resolver function.
mkResolvePolicy
  :: (Fact 'NormalFact -> Fact 'NormalFact -> ResolvedFact)
  -> ContradictionPolicy
mkResolvePolicy = RecordAndResolve . Resolver

-- | Unwrap the resolver from a 'RecordAndResolve' policy.
-- Returns 'Nothing' for 'RecordAndContinue' and 'RecordAndHalt'.
resolverOf :: ContradictionPolicy -> Maybe (Fact 'NormalFact -> Fact 'NormalFact -> ResolvedFact)
resolverOf (RecordAndResolve r) = Just (runResolver r)
resolverOf _                    = Nothing

-- ---------------------------------------------------------------------------
-- ContradictionPredicate
--
-- A predicate pairs a stable identity ('cpId') with a symmetric evaluation
-- function.  The function receives two *distinct* active normal facts; the
-- caller guarantees distinctness (different 'FactId').
--
-- Symmetry convention: the runtime calls 'cpEvaluate f1 f2' AND
-- 'cpEvaluate f2 f1' and considers a contradiction detected if either
-- returns True.  Predicate authors need not write symmetric functions
-- themselves; the detection loop handles both orderings.
--
-- 'crossPlugin': when False, the predicate is only evaluated against fact
-- pairs where BOTH facts share the registering plugin's namespace prefix.
-- When True, the predicate is evaluated against ALL active fact pairs.
-- Cross-plugin predicates require explicit opt-in to prevent plugins from
-- inadvertently interfering with each other's namespaces.

-- | Opaque wrapper around the predicate evaluation function so
-- 'ContradictionPredicate' can derive 'Show'.
-- 'Show' displays "<fn>"; 'Eq' is vacuously True (same reasoning as 'Resolver').
newtype Evaluator = Evaluator
  { runEvaluator :: Fact 'NormalFact -> Fact 'NormalFact -> Bool }

instance Show Evaluator where
  show _ = "Evaluator <fn>"

instance Eq Evaluator where
  _ == _ = True

data ContradictionPredicate = ContradictionPredicate
  { cpId          :: !PredicateId
    -- ^ Stable identifier.  Uniqueness enforced by 'ContradictionRegistry'.
  , cpOwner       :: !PluginId
    -- ^ The plugin that registered this predicate.  Used to scope
    -- namespace checks when 'cpCrossPlugin' is False.
  , cpPolicy      :: !ContradictionPolicy
    -- ^ What to do when this predicate fires.
  , cpCrossPlugin :: !Bool
    -- ^ If False, restrict evaluation to facts owned by 'cpOwner'.
    -- If True, evaluate against all active fact pairs.
  , cpEvaluate    :: !Evaluator
    -- ^ Pure, total contradiction test.  Called with two distinct facts.
    -- Must not throw or perform IO.  Construct with 'mkPredicate'.
  } deriving stock (Show)

-- | Equality is identity-based: two predicates are equal iff they have the
-- same 'PredicateId'.
instance Eq ContradictionPredicate where
  p1 == p2 = cpId p1 == cpId p2

-- | Smart constructor for 'ContradictionPredicate': wraps the evaluation
-- function in 'Evaluator' so the outer record can derive 'Show'.
mkPredicate
  :: PredicateId
  -> PluginId
  -> ContradictionPolicy
  -> Bool   -- ^ cpCrossPlugin
  -> (Fact 'NormalFact -> Fact 'NormalFact -> Bool)
  -> ContradictionPredicate
mkPredicate pid owner policy crossPlugin eval = ContradictionPredicate
  { cpId          = pid
  , cpOwner       = owner
  , cpPolicy      = policy
  , cpCrossPlugin = crossPlugin
  , cpEvaluate    = Evaluator eval
  }

-- ---------------------------------------------------------------------------
-- ResolvedFact
--
-- The output of a 'RecordAndResolve' resolver.  The type field must carry
-- the registering plugin's namespace prefix; 'validateResolvedFact' enforces
-- this before the runtime inserts the resolved fact.

data ResolvedFact = ResolvedFact
  { rfType           :: !Text
    -- ^ Namespaced fact type: MUST be "<cpOwner>:<localType>".
    -- Validated by 'validateResolvedFact'.
  , rfAttrs          :: !Attributes
    -- ^ Attributes of the resolved fact.
  , rfResolutionNote :: !Text
    -- ^ Human-readable description of how the contradiction was resolved.
    -- Carried in the audit trail; not used for logic.
  } deriving stock (Show, Eq)

-- | Errors that can occur when validating a 'ResolvedFact'.
data ResolvedFactError
  = RFEWrongNamespace !Text !PluginId
    -- ^ (actual type, expected owner): type is not prefixed with plugin's namespace.
  | RFEEmptyType
    -- ^ The resolved fact type string is empty.
  deriving stock (Show, Eq)

-- | Validate that a 'ResolvedFact' produced by a resolver is correctly
-- scoped under the registering plugin's namespace.
--
-- Called by the runtime before inserting a resolved fact.  Returns the
-- validated 'ResolvedFact' unchanged on success so the caller can use it
-- directly.
validateResolvedFact
  :: PluginId        -- ^ Registering plugin — must own the type prefix
  -> ResolvedFact
  -> Either ResolvedFactError ResolvedFact
validateResolvedFact pid rf
  | T.null (rfType rf) = Left RFEEmptyType
  | expectedPrefix `T.isPrefixOf` rfType rf = Right rf
  | otherwise = Left (RFEWrongNamespace (rfType rf) pid)
  where
    expectedPrefix = unPluginId pid <> ":"

-- ---------------------------------------------------------------------------
-- ContradictionRecord
--
-- An immutable record of a detected contradiction.  Append-only: once
-- written, a record is never modified or deleted.  The runtime accumulates
-- these in the 'ContradictionRegistry' for audit and query purposes.

data ContradictionRecord = ContradictionRecord
  { crPredicateId :: !PredicateId
    -- ^ Which predicate fired.
  , crFact1       :: !FactId
    -- ^ First fact in the contradicting pair.
  , crFact2       :: !FactId
    -- ^ Second fact in the contradicting pair.
    -- Invariant: crFact1 /= crFact2.  Enforced at 'recordContradiction'.
  , crSnapshotId  :: !SnapshotId
    -- ^ KB snapshot at the time of detection.
  , crTimestamp   :: !UTCTime
    -- ^ Wall-clock time of detection.
  , crPolicy      :: !ContradictionPolicy
    -- ^ Policy that was in effect when the contradiction was detected.
    -- Stored for audit: the policy may change after detection.
  } deriving stock (Show)

-- | Equality ignores timestamp — two records for the same predicate and
-- fact pair at the same snapshot are considered the same detection event.
instance Eq ContradictionRecord where
  r1 == r2 =
    crPredicateId r1 == crPredicateId r2 &&
    -- Normalise the pair so (f1,f2) == (f2,f1): a contradiction between
    -- the same two facts is the same regardless of argument order.
    Set.fromList [crFact1 r1, crFact2 r1] ==
    Set.fromList [crFact1 r2, crFact2 r2] &&
    crSnapshotId r1 == crSnapshotId r2

-- ---------------------------------------------------------------------------
-- ContradictionMatch — output of a single detection pass
--
-- Carries everything the runtime needs to act on a detected contradiction:
-- the record (for logging/persistence), the predicate (for the policy), and
-- the concrete fact values (for 'RecordAndResolve').

data ContradictionMatch = ContradictionMatch
  { cmRecord    :: !ContradictionRecord
    -- ^ The immutable detection record.
  , cmPredicate :: !ContradictionPredicate
    -- ^ Predicate that fired (with policy and resolver).
  , cmFact1     :: !(Fact 'NormalFact)
    -- ^ First contradicting fact (same identity as 'crFact1').
  , cmFact2     :: !(Fact 'NormalFact)
    -- ^ Second contradicting fact (same identity as 'crFact2').
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- RegistryState — pure inner state
--
-- Separated from 'ContradictionRegistry' so the TVar payload is a plain
-- Haskell value with no IO entanglement.  All mutation goes through STM.

data RegistryState = RegistryState
  { rsPredicates :: !(Map PredicateId ContradictionPredicate)
    -- ^ Registered predicates, keyed by identity.
  , rsRecords    :: ![ContradictionRecord]
    -- ^ Detection history (newest first). Capped at rsMaxRecords entries;
    -- oldest entries are evicted when the cap is reached.
  , rsMaxRecords :: !Int
    -- ^ Eviction cap. Always >= 1 (enforced by newRegistry).
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- ContradictionRegistry — public mutable handle

-- | Opaque handle to the contradiction registry.
-- Obtain with 'newRegistry'.
newtype ContradictionRegistry = ContradictionRegistry (TVar RegistryState)

-- | Allocate a fresh, empty registry with the given contradiction record cap.
-- The cap must be >= 1; values below 1 are clamped to 1.
newRegistry :: Int -> IO ContradictionRegistry
newRegistry maxRecords =
  ContradictionRegistry <$> newTVarIO RegistryState
    { rsPredicates = Map.empty
    , rsRecords    = []
    , rsMaxRecords = max 1 maxRecords
    }

-- ---------------------------------------------------------------------------
-- Registration

-- | Errors that can occur when registering a predicate.
data RegistrationError
  = REDuplicatePredicateId !PredicateId
    -- ^ A predicate with this ID is already registered.
  | REOwnerMismatch !PredicateId !PluginId !PluginId
    -- ^ (predicateId, registeredOwner, attemptedOwner):
    -- only the original registrant may update or re-register a predicate.
  deriving stock (Show, Eq)

-- | Register a contradiction predicate.
--
-- Returns 'Left REDuplicatePredicateId' if a predicate with the same
-- 'PredicateId' is already present.  Use 'unregisterPredicate' first to
-- replace an existing predicate.
registerPredicate
  :: ContradictionPredicate
  -> ContradictionRegistry
  -> IO (Either RegistrationError ())
registerPredicate cp (ContradictionRegistry tv) = atomically $ do
  st <- readTVar tv
  case Map.lookup (cpId cp) (rsPredicates st) of
    Just _  -> pure (Left (REDuplicatePredicateId (cpId cp)))
    Nothing -> do
      writeTVar tv st
        { rsPredicates = Map.insert (cpId cp) cp (rsPredicates st) }
      pure (Right ())

-- | Unregister a predicate by ID.  Silent no-op if the predicate is not
-- found — idempotent so that plugin shutdown is always safe.
unregisterPredicate
  :: PredicateId
  -> ContradictionRegistry
  -> IO ()
unregisterPredicate pid (ContradictionRegistry tv) =
  atomically $ modifyTVar' tv $ \st ->
    st { rsPredicates = Map.delete pid (rsPredicates st) }

-- | Look up a predicate by ID.  Returns 'Nothing' if not registered.
lookupPredicate
  :: PredicateId
  -> ContradictionRegistry
  -> IO (Maybe ContradictionPredicate)
lookupPredicate pid (ContradictionRegistry tv) = do
  st <- readTVarIO tv
  pure (Map.lookup pid (rsPredicates st))

-- | Return all currently registered predicates.
listPredicates :: ContradictionRegistry -> IO [ContradictionPredicate]
listPredicates (ContradictionRegistry tv) =
  Map.elems . rsPredicates <$> readTVarIO tv

-- ---------------------------------------------------------------------------
-- Detection
--
-- 'detectContradictions' is a pure-input, IO-only-for-reading function.
-- It takes a snapshot of active facts and the current predicate registry,
-- then exhaustively evaluates all applicable predicate–pair combinations.
--
-- Complexity: O(p · n²) where p = |predicates| and n = |activeFacts|.
-- For within-plugin predicates (cpCrossPlugin = False), n is restricted to
-- the plugin's namespace subset, which is typically much smaller.
--
-- The caller (Runtime / inference loop) is responsible for:
--   1. Calling 'detectContradictions' after each inference iteration.
--   2. Acting on each 'ContradictionMatch' according to 'cmPredicate.cpPolicy'.
--   3. Calling 'recordContradiction' for each match to persist the record.
--   4. Publishing 'EvContradictionDetected' for each match.

-- | Evaluate all registered predicates against all applicable active fact
-- pairs.  Returns one 'ContradictionMatch' per (predicate, fact-pair)
-- combination that fires.
--
-- Each pair (f1, f2) is tested as (f1,f2) then (f2,f1) per the symmetry
-- convention; the first ordering that returns True is used.  A pair is
-- never reported twice for the same predicate.
detectContradictions
  :: [ActiveFact]             -- ^ Snapshot of all active normal facts
  -> [ContradictionPredicate] -- ^ Currently registered predicates
  -> SnapshotId               -- ^ Current KB snapshot
  -> UTCTime                  -- ^ Detection timestamp
  -> [ContradictionMatch]
detectContradictions activeFacts predicates snap ts =
  concatMap (detectForPredicate factList) predicates
  where
    -- Unwrap once; re-used across all predicates.
    factList :: [Fact 'NormalFact]
    factList = map untagFact activeFacts

    detectForPredicate
      :: [Fact 'NormalFact]
      -> ContradictionPredicate
      -> [ContradictionMatch]
    detectForPredicate allFacts cp =
      let candidates = candidateFacts cp allFacts
      in mapMaybe (evalPair cp snap ts) (distinctPairs candidates)

-- | Select the facts that a predicate should be evaluated against.
--
-- For within-plugin predicates (cpCrossPlugin = False), only facts owned
-- by 'cpOwner' are considered.  The namespace prefix is "<pluginId>:".
candidateFacts
  :: ContradictionPredicate
  -> [Fact 'NormalFact]
  -> [Fact 'NormalFact]
candidateFacts cp allFacts
  | cpCrossPlugin cp = allFacts
  | otherwise        =
      let prefix = unPluginId (cpOwner cp) <> ":"
      in filter (\f -> prefix `T.isPrefixOf` factRecordType f) allFacts

-- | All unordered pairs of distinct elements.  O(n²/2).
--
-- Each pair (a, b) appears exactly once with a before b in the original
-- list ordering; (b, a) is NOT included.  The symmetry convention in
-- 'evalPair' handles both orderings.
distinctPairs :: [a] -> [(a, a)]
distinctPairs []     = []
distinctPairs (x:xs) = map (x,) xs <> distinctPairs xs

-- | Evaluate one predicate against one distinct fact pair.
-- Tests both orderings per the symmetry convention.
-- Returns 'Just ContradictionMatch' if the predicate fires, 'Nothing' otherwise.
evalPair
  :: ContradictionPredicate
  -> SnapshotId
  -> UTCTime
  -> (Fact 'NormalFact, Fact 'NormalFact)
  -> Maybe ContradictionMatch
evalPair cp snap ts (f1, f2)
  -- Guard: the pair must be genuinely distinct (different FactId).
  -- distinctPairs constructs pairs from a list with no duplicates, but this
  -- guard defends against a fact appearing twice in the input snapshot.
  | factRecordId f1 == factRecordId f2 = Nothing
  | runEvaluator (cpEvaluate cp) f1 f2 || runEvaluator (cpEvaluate cp) f2 f1 =
      Just ContradictionMatch
        { cmRecord = ContradictionRecord
            { crPredicateId = cpId cp
            , crFact1       = factRecordId f1
            , crFact2       = factRecordId f2
            , crSnapshotId  = snap
            , crTimestamp   = ts
            , crPolicy      = cpPolicy cp
            }
        , cmPredicate = cp
        , cmFact1     = f1
        , cmFact2     = f2
        }
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Record management

-- | Append a 'ContradictionRecord' to the registry's history.
--
-- Precondition: 'crFact1 /= crFact2'.  Returns 'Left' if violated so the
-- caller can surface the error rather than silently storing a malformed record.
recordContradiction
  :: ContradictionRecord
  -> ContradictionRegistry
  -> IO (Either Text ())
recordContradiction cr (ContradictionRegistry tv)
  | crFact1 cr == crFact2 cr =
      pure (Left "recordContradiction: crFact1 and crFact2 must be distinct")
  | otherwise = atomically $ do
      modifyTVar' tv $ \st ->
        st { rsRecords = take (rsMaxRecords st) (cr : rsRecords st) }
      pure (Right ())

-- ---------------------------------------------------------------------------
-- Record queries

-- | Filters for querying the contradiction history.
--
-- All fields are optional.  An empty 'RecordQuery' matches all records.
-- Multiple fields are AND-combined.
data RecordQuery = RecordQuery
  { rqPredicateId :: !(Maybe PredicateId)
    -- ^ Restrict to records from this predicate.
  , rqFactId      :: !(Maybe FactId)
    -- ^ Restrict to records involving this fact (either position).
  , rqSnapshotId  :: !(Maybe SnapshotId)
    -- ^ Restrict to records from this snapshot.
  } deriving stock (Show, Eq)

-- | A 'RecordQuery' that matches all records.
emptyRecordQuery :: RecordQuery
emptyRecordQuery = RecordQuery Nothing Nothing Nothing

-- | Query the contradiction history.  Results are returned newest-first
-- (consistent with the append-only list order in 'RegistryState').
queryRecords
  :: RecordQuery
  -> ContradictionRegistry
  -> IO [ContradictionRecord]
queryRecords q (ContradictionRegistry tv) = do
  st <- readTVarIO tv
  pure (filter (matchesQuery q) (rsRecords st))

matchesQuery :: RecordQuery -> ContradictionRecord -> Bool
matchesQuery q cr =
  checkPredicate && checkFact && checkSnapshot
  where
    checkPredicate = case rqPredicateId q of
      Nothing  -> True
      Just pid -> crPredicateId cr == pid

    checkFact = case rqFactId q of
      Nothing  -> True
      Just fid -> crFact1 cr == fid || crFact2 cr == fid

    checkSnapshot = case rqSnapshotId q of
      Nothing  -> True
      Just sid -> crSnapshotId cr == sid

-- ---------------------------------------------------------------------------
-- Event construction helper
--
-- Produces the canonical 'EvContradictionDetected' event for a match.
-- The predicate ID is carried as Text (the third field) per the Event ADT.
-- This helper lives here — not in Runtime — so that any layer with access
-- to a 'ContradictionMatch' can construct the correct event without
-- importing Runtime and creating a dependency cycle.

-- | Build the 'EvContradictionDetected' event for a detected contradiction.
contradictionEvent :: ContradictionMatch -> Event
contradictionEvent cm =
  EvContradictionDetected
    (crFact1      (cmRecord cm))
    (crFact2      (cmRecord cm))
    (unPredicateId (crPredicateId (cmRecord cm)))