-- | Analytics.Core.Query
--
-- GADT-indexed query language, result types, and the 'QueryEngine' typeclass.
--
-- The central design invariant is that the /response type is a compile-time
-- function of the query type/.  'Query' is a GADT indexed by 'QueryResultKind';
-- 'QueryResultOf' is a closed type family mapping each kind to its result type.
-- This eliminates the possibility of returning a 'WhyResult' where a
-- 'HowResult' is expected — mismatches are a compile error, not a runtime
-- exception.
--
-- Architecture:
--
--   * 'Query k' — the query GADT.  Each constructor carries its own
--     sub-query record, keeping the top-level type small and the
--     per-query parameters well-typed.
--
--   * 'QueryResultOf k' — closed type family; one arm per 'QueryResultKind'.
--
--   * 'QueryEngine' — the single-method typeclass.  Concrete engines
--     (in-memory KB, storage-backed, etc.) implement 'runQuery'.
--
--   * Sub-query records ('WhyQuery', 'HowQuery', …) and result records
--     ('WhyResult', 'HowResult', …) are defined here so callers have a
--     single import.
--
--   * 'Justification' — a recursive ADT for provenance trees.  Traversal
--     is always total; no partial functions are used.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Fact
--   Analytics.Core.Rule
--   Analytics.Core.Evidence
--   Analytics.Core.Graph
--   Analytics.Core.Storage    (FactQuery, RuleQuery re-exported for convenience)
--   Analytics.Core.Contradiction  (ContradictionRecord)
--   ← this module

module Analytics.Core.Query
  ( -- * Query result kinds
    QueryResultKind(..)
  , QueryResultOf

    -- * Query GADT
  , Query(..)

    -- * Sub-query records
  , WhyQuery(..)
  , HowQuery(..)
  , ContradictionsQuery(..)
  , ProvenanceQuery(..)
  , EvidenceQuery(..)

    -- * Result types
  , WhyResult(..)
  , HowResult(..)
  , ContradictionsResult(..)
  , ProvenanceResult(..)
  , EvidenceResult(..)

    -- * Justification tree
  , Justification(..)
  , DerivedByData(..)
  , DerivationStep(..)

    -- * Query engine typeclass
  , QueryEngine(..)

    -- * Errors
  , QueryError(..)

    -- * Depth limit (validated positive bound)
  , QueryDepth
  , mkQueryDepth
  , unQueryDepth
  , defaultQueryDepth
  ) where

import Relude
import Numeric.Natural ()

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Fact(..)
  , FactKind(..)
  )
import Analytics.Core.Rule    (Rule(..))
import Analytics.Core.Evidence (Evidence(..))
import Analytics.Core.Graph
  ( Graph
  )
import Analytics.Core.Storage
  ( FactQuery(..)
  , RuleQuery(..)
  )
import Analytics.Core.Contradiction (ContradictionRecord(..), PredicateId)

-- ---------------------------------------------------------------------------
-- QueryDepth — validated positive bound
--
-- Justification and dependency traversals are recursive.  An unbounded depth
-- would loop on cyclic graphs (even though the inference engine prevents
-- provenance cycles, the full KB graph may have cycles via plugin edges).
-- 'QueryDepth' is a positive Natural so callers cannot accidentally pass 0.

newtype QueryDepth = QueryDepth Natural
  deriving stock   (Show)
  deriving newtype (Eq, Ord)

-- | Smart constructor.  Returns 'Left' for zero.
mkQueryDepth :: Natural -> Either Text QueryDepth
mkQueryDepth 0 = Left "QueryDepth must be > 0"
mkQueryDepth n = Right (QueryDepth n)

unQueryDepth :: QueryDepth -> Natural
unQueryDepth (QueryDepth n) = n

-- | Sensible default: 50 levels of justification depth.
defaultQueryDepth :: QueryDepth
defaultQueryDepth = QueryDepth 50

-- ---------------------------------------------------------------------------
-- QueryResultKind — kind-level tag, one per query constructor

data QueryResultKind
  = WhyKind
  | HowKind
  | ContradictionsKind
  | ProvenanceKind
  | EvidenceKind
  | FactsKind
  | PluginKind
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- QueryResultOf — closed type family
--
-- Maps each 'QueryResultKind' to its concrete result type.  The family is
-- closed so no external module can add arms; adding a new query kind
-- requires editing this file, which is caught at all call sites.

type family QueryResultOf (k :: QueryResultKind) where
  QueryResultOf 'WhyKind            = WhyResult
  QueryResultOf 'HowKind            = HowResult
  QueryResultOf 'ContradictionsKind = ContradictionsResult
  QueryResultOf 'ProvenanceKind     = ProvenanceResult
  QueryResultOf 'EvidenceKind       = EvidenceResult
  QueryResultOf 'FactsKind          = [Fact 'NormalFact]
  QueryResultOf 'PluginKind         = Value

-- ---------------------------------------------------------------------------
-- Sub-query records
--
-- Each query constructor carries one of these records.  Keeping parameters
-- in a named record (rather than as positional GADT fields) means callers
-- can use record update syntax and the field names are self-documenting.

-- | Ask: why does this fact hold?
-- Returns the full provenance tree rooted at the requested fact.
data WhyQuery = WhyQuery
  { wqFactId    :: !FactId
    -- ^ The fact whose justification is requested.
  , wqMaxDepth  :: !QueryDepth
    -- ^ Maximum recursion depth for the justification tree.
    -- Prevents runaway traversal on deep derivation chains.
  } deriving stock (Show, Eq)

-- | Ask: how was this fact derived, step by step?
-- Returns the ordered sequence of derivation steps from seed facts to the
-- requested fact, shallowest first.
data HowQuery = HowQuery
  { hqFactId   :: !FactId
    -- ^ The fact whose derivation chain is requested.
  , hqMaxDepth :: !QueryDepth
    -- ^ Maximum number of derivation steps to return.
  } deriving stock (Show, Eq)

-- | Ask: what contradictions are currently recorded?
-- Filters by predicate, fact, or snapshot.
data ContradictionsQuery = ContradictionsQuery
  { cqPredicateId :: !(Maybe PredicateId)
    -- ^ Restrict to contradictions from this predicate.
  , cqFactId      :: !(Maybe FactId)
    -- ^ Restrict to contradictions involving this fact (either position).
  , cqSnapshotId  :: !(Maybe SnapshotId)
    -- ^ Restrict to contradictions from this snapshot.
  , cqLimit       :: !(Maybe Natural)
    -- ^ Cap the result count.  'Nothing' = no limit.
  } deriving stock (Show, Eq)

-- | Ask: what does this fact depend on (transitively)?
-- Returns the subgraph of facts reachable via 'DerivedFrom' edges from the
-- requested fact, up to the specified depth.
--
-- Note: this is a provenance traversal over the derivation graph — it answers
-- "which ancestor facts was this fact derived from?"  It is distinct from the
-- spec's DependenciesQuery (§15.3.4), which asks "which fact types does a
-- rule structurally depend on?" via 'DependsOn' edges.  That query operates
-- on rules, not facts, and is not yet implemented.
data ProvenanceQuery = ProvenanceQuery
  { dqFactId   :: !FactId
    -- ^ Root of the provenance traversal.
  , dqMaxDepth :: !QueryDepth
    -- ^ Maximum graph traversal depth.
  , dqGraph    :: !Graph
    -- ^ The provenance graph to traverse.
    -- Passed explicitly so the engine does not need to rebuild it per query.
  } deriving stock (Show)

-- | Ask: what evidence records exist for this fact?
data EvidenceQuery = EvidenceQuery
  { eqFactId :: !FactId
    -- ^ The fact whose evidence records are requested.
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Query GADT
--
-- Each constructor is indexed by its 'QueryResultKind' so 'runQuery' returns
-- the exact type corresponding to the query — no casts, no runtime checks.

data Query (k :: QueryResultKind) where
  QWhy            :: !WhyQuery            -> Query 'WhyKind
  QHow            :: !HowQuery            -> Query 'HowKind
  QContradictions :: !ContradictionsQuery -> Query 'ContradictionsKind
  QProvenance     :: !ProvenanceQuery     -> Query 'ProvenanceKind
  QEvidence       :: !EvidenceQuery       -> Query 'EvidenceKind
  QFacts          :: !FactQuery           -> Query 'FactsKind
  QRules          :: !RuleQuery           -> Query 'PluginKind
    -- ^ Query the rule set.  Returns a serialised 'Value' (VNull for now;
    -- concrete engines serialise the rule list as needed by callers).

deriving instance Show (Query k)

-- ---------------------------------------------------------------------------
-- Justification — recursive provenance tree
--
-- Tracks *why* a fact holds.  'BaseAsserted' is the base case (a fact that
-- was directly asserted, not derived).  'DerivedBy' is the recursive case:
-- the fact was produced by a rule whose premises were each justified
-- recursively.
--
-- The tree is always finite because:
--   1. The inference engine prevents provenance cycles (§13.3 cycle check).
--   2. 'WhyQuery.wqMaxDepth' bounds the traversal depth.
-- Both conditions are enforced by the engine; 'Justification' itself is
-- a pure data structure with no built-in depth guard.

data Justification
  = BaseAsserted
    -- ^ The fact was directly asserted; no further justification exists.
  | DerivedBy !DerivedByData
    -- ^ The fact was produced by a rule; see 'DerivedByData' for fields.
  deriving stock (Show)

-- | Payload for the 'DerivedBy' constructor.  Defined as a separate product
-- type so all record selectors are total (no partiality warning).
data DerivedByData = DerivedByData
  { jRule     :: !Rule
    -- ^ The rule that produced the fact.
  , jBinding  :: !Binding
    -- ^ The variable binding that matched the premises.
  , jPremises :: !(NonEmpty (Fact 'NormalFact, Justification))
    -- ^ Each premise paired with its own justification, recursively.
    -- 'NonEmpty': a rule always has at least one premise (enforced by
    -- the 'Rule' type).
  , jSnapshot :: !SnapshotId
    -- ^ Snapshot at which this derivation step occurred.
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- DerivationStep — one step in a 'HowResult' chain
--
-- A linearised view of one rule application: which rule fired, which facts
-- it consumed (premises), what it produced (conclusion), and at which snapshot.

data DerivationStep = DerivationStep
  { dsRule       :: !Rule
    -- ^ The rule that fired.
  , dsPremises   :: !(NonEmpty (Fact 'NormalFact))
    -- ^ The facts that matched the rule's premises (in premise order).
    -- 'NonEmpty': a rule always has at least one premise.
  , dsConclusion :: !(Fact 'NormalFact)
    -- ^ The fact produced by this step.
  , dsEvidence   :: !Evidence
    -- ^ The evidence record created for this derivation.
  , dsSnapshot   :: !SnapshotId
    -- ^ KB snapshot at derivation time.
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Result types

-- | Result of 'QWhy': the fact and its full provenance tree.
data WhyResult = WhyResult
  { wrFact          :: !(Fact 'NormalFact)
    -- ^ The fact whose justification was requested.
  , wrJustification :: !Justification
    -- ^ The provenance tree.  'BaseAsserted' for directly-asserted facts.
  } deriving stock (Show)

-- | Result of 'QHow': the ordered derivation chain, earliest step first.
data HowResult = HowResult
  { hrSteps :: ![DerivationStep]
    -- ^ Derivation steps in chronological order (shallowest derivation first).
    -- Empty list means the fact was directly asserted (no derivation steps).
  , hrFact  :: !(Fact 'NormalFact)
    -- ^ The fact whose derivation chain was requested.
  } deriving stock (Show)

-- | Result of 'QContradictions': all matching contradiction records.
data ContradictionsResult = ContradictionsResult
  { crRecords    :: ![ContradictionRecord]
    -- ^ Matching records, newest first (consistent with registry append order).
  , crTotalCount :: !Natural
    -- ^ Total number of records before the limit was applied.
    -- Useful for pagination: if 'crTotalCount > length crRecords', there are
    -- more records than the query limit returned.
  } deriving stock (Show)

-- | Result of 'QProvenance': the provenance subgraph reachable from a fact.
data ProvenanceResult = ProvenanceResult
  { drRootFact     :: !FactId
    -- ^ The root fact the traversal started from.
  , drReachable    :: ![Fact 'NormalFact]
    -- ^ All facts reachable from the root via 'DerivedFrom' edges within
    -- 'dqMaxDepth' hops.  Does not include the root fact itself.
  , drEdgeCount    :: !Natural
    -- ^ Number of 'DerivedFrom' edges in the traversed subgraph.
  , drDepthReached :: !Natural
    -- ^ Maximum depth actually traversed (≤ 'unQueryDepth dqMaxDepth').
  } deriving stock (Show)

-- | Result of 'QEvidence': all evidence records for a fact.
data EvidenceResult = EvidenceResult
  { erFactId   :: !FactId
    -- ^ The fact whose evidence was requested.
  , erEvidence :: ![Evidence]
    -- ^ All evidence records for the fact, ordered by derivation depth
    -- ascending (shallowest first).
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- QueryError

-- | All reasons a query can fail.  Every constructor carries a structured
-- payload — no bare 'Text' catch-all.
data QueryError
  = QEFactNotFound     !FactId
    -- ^ The requested fact does not exist in the knowledge base.
  | QERuleNotFound     !RuleId
    -- ^ The referenced rule does not exist.
  | QEEvidenceNotFound !FactId
    -- ^ Evidence was expected for a derived fact but none was recorded.
    -- Indicates a KB inconsistency (derived fact with no provenance).
  | QESnapshotNotFound !SnapshotId
    -- ^ The requested snapshot does not exist or has been pruned.
  | QEDepthExceeded    !QueryDepth
    -- ^ The traversal reached 'QueryDepth' without finding a base case.
    -- Caller should retry with a larger 'QueryDepth' or inspect the graph
    -- for unexpected depth.
  | QEGraphMissing
    -- ^ A 'QProvenance' query was submitted but the graph was empty.
  | QEQueryTimeout     !Natural
    -- ^ The query exceeded a wall-clock deadline; elapsed milliseconds carried.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- QueryEngine typeclass
--
-- A single method, 'runQuery', takes any 'Query k' and returns
-- 'IO (Either QueryError (QueryResultOf k))'.  The return type is a function
-- of the query's kind index, so each implementation must handle all
-- constructors and the compiler enforces exhaustiveness via
-- '-Wincomplete-patterns -Werror'.
--
-- Concrete engines supply a value of their own type as the context.  The
-- engine handle carries whatever state is needed (KB handle, evidence index,
-- contradiction registry, graph, etc.).

class QueryEngine qe where
  -- | Execute a query against the engine's current state.
  --
  -- Caller responsibilities:
  --   * Pass a 'ProvenanceQuery' with an up-to-date 'Graph' for
  --     'QProvenance' queries; the engine does not maintain the graph
  --     internally.
  --   * 'QFacts' and 'QRules' queries use 'FactQuery' / 'RuleQuery' from
  --     'Analytics.Core.Storage'; the engine applies them directly.
  --
  -- Implementation responsibilities:
  --   * Return 'Left QEFactNotFound' rather than 'Left QEEvidenceNotFound'
  --     when a fact does not exist at all.
  --   * Respect 'QueryDepth' bounds in 'QWhy' and 'QHow'; return
  --     'Left QEDepthExceeded' if the bound is hit before finding a base
  --     case.
  --   * All errors use structured 'QueryError' constructors; no 'Text'
  --     fallback.
  runQuery
    :: Query k
    -> qe
    -> IO (Either QueryError (QueryResultOf k))