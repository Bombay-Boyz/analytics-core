module Analytics.Core.Fact
  ( -- * Fact kind (compile-time)
    FactKind(..)
    -- * Kind-indexed fact type
  , Fact(..)
    -- * Lifecycle status (compile-time)
  , LifecycleStatus(..)
  , TaggedFact(..)
  , ActiveFact
  , RetractedFact
  , tagActive
  , tagRetracted
  , untagFact
    -- * Existential wrappers (kind unknown at compile time)
  , AnyStagedFact(..)
  , AnyFact(..)
  , forgetKind
    -- * Field accessors (work on any kind)
  , factRecordId
  , factRecordType
  , factRecordAttrs
  , factRecordTimestamp
    -- * Content equality (Def 3.12)
  , FactContent(..)
  , factContent
  , isContentEqual
    -- * Smart constructors
  , mkAssertedFact
  , mkDerivedFact
  , mkRetractionFact
    -- * Retraction accessor (total — no Either)
  , retractionTarget
    -- * Completion from template output
    --
    -- 'completeFact' (the old Maybe-provenance variant) has been removed.
    -- Use 'completeInferredFact' for facts produced by the inference engine
    -- and 'completeAssertedFact' for externally-asserted facts.
    -- Both run the full validation suite (namespace + attr count + NaN).
  , completeInferredFact
  , completeAssertedFact
    -- * Validation
  , validateFact
  , FactValidationError(..)
  , validateNamespace
    -- * Namespace helpers
  , verifyFactNamespace
    -- * PartialFact
  , PartialFact(..)
    -- * Template types and instantiation
  , Template(..)
  , AttrExpr(..)
  , TemplateError(..)
  , instantiateTemplate
  , templateFreeVars
    -- * AttrExpr structural checks
  , hasEmptyConcat
    -- * Pattern types and matching
  , Pattern(..)
  , TypeConstraint(..)
  , AttrConstraint(..)
  , MatchResult(..)
  , matchPattern
  , patternBoundVars
    -- * UUID helper
  , unFactId
  ) where

import Relude
import Data.UUID                  (UUID)
import qualified Data.Text        as T
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import Data.Time                  (UTCTime)
import Validation                 (Validation (..))
import Analytics.Core.Types

-- ---------------------------------------------------------------------------
-- Fact kind — what a fact *is*, tracked at the type level.
--
-- NormalFact     — carries user-defined attributes; produced by assertion
--                  or inference.
-- RetractionFact — carries the FactId it retracts directly as a typed
--                  field, never as a serialised string inside attributes.

data FactKind = NormalFact | RetractionFact

-- ---------------------------------------------------------------------------
-- Kind-indexed Fact GADT
--
-- FactSource (Asserted vs Derived) is now a constructor distinction inside
-- NormalFact, not a separate record field — source and content are unified.
-- RetractionFact stores its target as a typed FactId: no UUID round-tripping,
-- no Maybe, no Either.

data Fact (k :: FactKind) where
  Asserted   :: !FactId            -- unique id for this fact
             -> !Text              -- namespaced type, e.g. "sensor:reading"
             -> !Attributes        -- user-defined key/value pairs
             -> !UTCTime           -- wall-clock assertion time
             -> Fact 'NormalFact

  Derived    :: !FactId            -- unique id for this fact
             -> !Text              -- namespaced type
             -> !Attributes
             -> !(NonEmpty FactId) -- parent facts whose match fired the rule
             -> !RuleId            -- rule that produced this fact
             -> !UTCTime
             -> Fact 'NormalFact

  Retraction :: !FactId            -- unique id for the retraction event itself
             -> !FactId            -- the fact being retracted (typed — never a string)
             -> !UTCTime
             -> Fact 'RetractionFact

deriving instance Show (Fact k)
deriving instance Eq   (Fact k)

-- ---------------------------------------------------------------------------
-- Lifecycle status — Active vs Retracted, orthogonal to FactKind.
--
-- TaggedFact (s :: LifecycleStatus) (k :: FactKind) carries both dimensions
-- at the type level. The inference engine works exclusively with
-- TaggedFact 'Active 'NormalFact; retraction events are
-- TaggedFact 'Active 'RetractionFact until processed, after which the
-- targeted fact becomes TaggedFact 'Retracted 'NormalFact.

data LifecycleStatus = Active | Retracted

newtype TaggedFact (s :: LifecycleStatus) (k :: FactKind) =
  TaggedFact { unTagFact :: Fact k }

deriving instance Show (Fact k) => Show (TaggedFact s k)

type ActiveFact    = TaggedFact 'Active    'NormalFact
type RetractedFact = TaggedFact 'Retracted 'NormalFact

tagActive :: Fact k -> TaggedFact 'Active k
tagActive = TaggedFact

tagRetracted :: Fact 'NormalFact -> RetractedFact
tagRetracted = TaggedFact

-- | Project the underlying Fact, discarding the lifecycle status phantom.
-- Use sparingly; prefer pattern matching on TaggedFact directly.
untagFact :: TaggedFact s k -> Fact k
untagFact = unTagFact

-- ---------------------------------------------------------------------------
-- Existential wrappers
--
-- AnyStagedFact s — kind unknown at compile time, lifecycle status fixed.
-- AnyFact         — both dimensions unknown; used at storage boundaries.

data AnyStagedFact (s :: LifecycleStatus) =
  forall k. AnyStagedFact (TaggedFact s k)

data AnyFact =
  forall k. AnyFact (Fact k)

forgetKind :: TaggedFact s k -> AnyStagedFact s
forgetKind = AnyStagedFact

-- ---------------------------------------------------------------------------
-- Uniform field accessors
--
-- These operate on any Fact k without the caller case-splitting on kind.

factRecordId :: Fact k -> FactId
factRecordId (Asserted   fid _ _ _)     = fid
factRecordId (Derived    fid _ _ _ _ _) = fid
factRecordId (Retraction fid _ _)       = fid

factRecordType :: Fact k -> Text
factRecordType (Asserted   _ t _ _)     = t
factRecordType (Derived    _ t _ _ _ _) = t
factRecordType (Retraction _ _ _)       = "system:retracted"

factRecordAttrs :: Fact 'NormalFact -> Attributes
factRecordAttrs (Asserted _ _ a _)      = a
factRecordAttrs (Derived  _ _ a _ _ _)  = a

factRecordTimestamp :: Fact k -> UTCTime
factRecordTimestamp (Asserted   _ _ _ ts)     = ts
factRecordTimestamp (Derived    _ _ _ _ _ ts) = ts
factRecordTimestamp (Retraction _ _ ts)       = ts

-- ---------------------------------------------------------------------------
-- Content equality (Def 3.12)
--
-- Applies only to NormalFact — retraction events are identity facts,
-- not content facts. Two facts are content-equal when they have the same
-- namespaced type and the same attribute map, regardless of identity (FactId)
-- or provenance (Asserted vs Derived).

data FactContent = FactContent
  { contentType  :: !Text
  , contentAttrs :: !Attributes
  } deriving stock (Eq, Ord, Show)

factContent :: Fact 'NormalFact -> FactContent
factContent f = FactContent (factRecordType f) (factRecordAttrs f)

isContentEqual :: Fact 'NormalFact -> Fact 'NormalFact -> Bool
isContentEqual f1 f2 = factContent f1 == factContent f2

-- ---------------------------------------------------------------------------
-- Validation errors

data FactValidationError
  = EmptyFactType
  | MissingNamespaceSeparator Text
  | EmptyNamespace            Text
  | EmptyLocalType            Text
  | TooManyAttributes         Int
  | NaNFloatAttribute         Text
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Validation helpers

validateNamespace :: Text -> Validation (NonEmpty FactValidationError) ()
validateNamespace t
  | T.null t  = Failure (EmptyFactType :| [])
  | otherwise = case T.breakOn ":" t of
      (_, "")     -> Failure (MissingNamespaceSeparator t :| [])
      ("", _)     -> Failure (EmptyNamespace t :| [])
      (_, suffix) ->
        case T.stripPrefix ":" suffix of
          Just localPart | T.null localPart -> Failure (EmptyLocalType t :| [])
          _                                 -> pure ()

validateAttrCount :: Attributes -> Validation (NonEmpty FactValidationError) ()
validateAttrCount attrs
  | Map.size attrs > 256 = Failure (TooManyAttributes (Map.size attrs) :| [])
  | otherwise            = pure ()

validateNoNaN :: Attributes -> Validation (NonEmpty FactValidationError) ()
validateNoNaN attrs =
  let nanKeys = [ k | (k, VFloat v) <- Map.toList attrs, isNaN v ]
  in case nonEmpty nanKeys of
       Nothing  -> pure ()
       Just nel -> Failure (fmap NaNFloatAttribute nel)

-- ---------------------------------------------------------------------------
-- Smart constructors

-- | Construct and validate an asserted fact.
mkAssertedFact
  :: FactId -> Text -> Attributes -> UTCTime
  -> Either (NonEmpty FactValidationError) (Fact 'NormalFact)
mkAssertedFact fid ftype attrs ts =
  case validateNamespace ftype *> validateAttrCount attrs *> validateNoNaN attrs of
    Failure errs -> Left errs
    Success ()   -> Right (Asserted fid ftype attrs ts)

-- | Construct and validate a derived fact (produced by the inference engine).
mkDerivedFact
  :: FactId -> Text -> Attributes -> NonEmpty FactId -> RuleId -> UTCTime
  -> Either (NonEmpty FactValidationError) (Fact 'NormalFact)
mkDerivedFact fid ftype attrs parents rid ts =
  case validateNamespace ftype *> validateAttrCount attrs *> validateNoNaN attrs of
    Failure errs -> Left errs
    Success ()   -> Right (Derived fid ftype attrs parents rid ts)

-- | Construct a retraction fact.
-- The target FactId is a typed field — no serialisation, no UUID
-- round-trip, no runtime failure possible.
mkRetractionFact :: FactId -> FactId -> UTCTime -> Fact 'RetractionFact
mkRetractionFact newId targetId ts = Retraction newId targetId ts

-- ---------------------------------------------------------------------------
-- Retraction accessor — total, no Either
--
-- Previously: retractionTarget :: Fact -> Either Text FactId
-- Required parsing a UUID out of a Text-valued attributes entry.
-- That entire failure mode is now impossible: the target is a FactId
-- field directly on the Retraction constructor.

retractionTarget :: Fact 'RetractionFact -> FactId
retractionTarget (Retraction _ targetId _) = targetId

-- ---------------------------------------------------------------------------
-- Completion from template output
--
-- The old 'completeFact' took 'Maybe (NonEmpty FactId, RuleId)' for
-- provenance, meaning a caller could silently produce an Asserted fact
-- from inference context by passing Nothing. That footgun is eliminated:
-- the two cases are now separate functions with unambiguous types.
--
-- Both functions run the full validation suite (namespace + attr count + NaN)
-- so that malformed template output is caught before the fact enters the KB,
-- not silently persisted.

-- | Complete a fact produced by the inference engine (always Derived).
-- Provenance is mandatory — parents and ruleId are required at the call site.
-- Runs: namespace check, attr-count check, NaN check.
completeInferredFact
  :: PartialFact -> FactId -> NonEmpty FactId -> RuleId -> UTCTime
  -> Either (NonEmpty FactValidationError) (Fact 'NormalFact)
completeInferredFact pf fid parents rid ts =
  case validateNamespace (partialType pf)
         *> validateAttrCount (partialAttrs pf)
         *> validateNoNaN     (partialAttrs pf) of
    Failure errs -> Left errs
    Success ()   -> Right (Derived fid (partialType pf) (partialAttrs pf) parents rid ts)

-- | Complete a fact from an external assertion (always Asserted).
-- Used by Runtime / Plugin for seed facts and resolved contradiction
-- assertions. Runs: namespace check, attr-count check, NaN check.
completeAssertedFact
  :: PartialFact -> FactId -> UTCTime
  -> Either (NonEmpty FactValidationError) (Fact 'NormalFact)
completeAssertedFact pf fid ts =
  case validateNamespace (partialType pf)
         *> validateAttrCount (partialAttrs pf)
         *> validateNoNaN     (partialAttrs pf) of
    Failure errs -> Left errs
    Success ()   -> Right (Asserted fid (partialType pf) (partialAttrs pf) ts)

-- | Re-validate a NormalFact already in memory (e.g. after deserialisation).
validateFact :: Fact 'NormalFact -> Either (NonEmpty FactValidationError) ()
validateFact f =
  case validateNamespace (factRecordType f)
         *> validateAttrCount (factRecordAttrs f)
         *> validateNoNaN     (factRecordAttrs f) of
    Failure errs -> Left errs
    Success ()   -> Right ()

-- ---------------------------------------------------------------------------
-- Namespace helpers

-- | Verify that a NormalFact's type is scoped under the given plugin's
-- namespace prefix. Retraction facts ("system:retracted") are excluded
-- because they are not plugin-owned.
verifyFactNamespace :: PluginId -> Fact 'NormalFact -> Either Text ()
verifyFactNamespace pid f =
  let prefix = unPluginId pid <> ":"
  in if prefix `T.isPrefixOf` factRecordType f
     then Right ()
     else Left $ "Fact type " <> factRecordType f
              <> " does not begin with plugin namespace " <> unPluginId pid

-- ---------------------------------------------------------------------------
-- PartialFact
-- Produced by template instantiation; promoted to Fact by the inference
-- engine once it assigns a FactId and provenance.

data PartialFact = PartialFact
  { partialType  :: !Text
  , partialAttrs :: !Attributes
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Template types

data TemplateError
  = UnboundVariable VarName
  | ConcatOnNonText AttrExpr Value
  deriving stock (Show, Eq)

data Template = Template
  { outType  :: !Text
  , outAttrs :: !(Map Text AttrExpr)
  } deriving stock (Show, Eq)

data AttrExpr
  = Literal !Value
  | Bound   !VarName
  | Concat  ![AttrExpr]
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- AttrExpr structural checks

-- | True if the AttrExpr contains an empty Concat node anywhere in its tree.
--
-- 'Concat []' is structurally valid but semantically wrong: it silently
-- produces VText "" rather than signalling a missing expression. Rule
-- validation calls this to reject templates that contain such nodes before
-- they reach the inference engine (Bug 10).
hasEmptyConcat :: AttrExpr -> Bool
hasEmptyConcat (Literal _)    = False
hasEmptyConcat (Bound _)      = False
hasEmptyConcat (Concat [])    = True
hasEmptyConcat (Concat exprs) = any hasEmptyConcat exprs

-- ---------------------------------------------------------------------------
-- Template instantiation

instantiateTemplate
  :: Binding -> Template
  -> Either (NonEmpty TemplateError) PartialFact
instantiateTemplate binding tmpl =
  -- Resolve the output type and all attribute expressions independently,
  -- accumulating errors from both passes before returning.
  case (resolveOutType binding (outType tmpl), resolveAttrs binding tmpl) of
    (Failure e1, Failure e2) -> Left (e1 <> e2)
    (Failure e1, Success _)  -> Left e1
    (Success _,  Failure e2) -> Left e2
    (Success t,  Success attrs) -> Right (PartialFact t attrs)

-- | Resolve the output type expression.
-- Currently the output type is always a literal Text. This function is the
-- extension point for variable interpolation in type names if required.
resolveOutType :: Binding -> Text -> Validation (NonEmpty TemplateError) Text
resolveOutType _binding t = pure t

resolveAttrs
  :: Binding -> Template
  -> Validation (NonEmpty TemplateError) Attributes
resolveAttrs binding tmpl =
  traverse (resolveAttrExpr binding) (outAttrs tmpl)

resolveAttrExpr
  :: Binding -> AttrExpr
  -> Validation (NonEmpty TemplateError) Value
resolveAttrExpr _       (Literal v)  = pure v
resolveAttrExpr binding (Bound var)  =
  case Map.lookup var binding of
    Just v  -> pure v
    Nothing -> Failure (UnboundVariable var :| [])
resolveAttrExpr binding (Concat exprs) =
  case traverse (resolveAttrExpr binding) exprs of
    Failure e    -> Failure e
    Success vals ->
      case traverse asText vals of
        Just txts -> pure (VText (mconcat txts))
        Nothing   ->
          let offender = fromMaybe VNull (find (isNothing . asText) vals)
          in Failure (ConcatOnNonText (Concat exprs) offender :| [])
  where
    asText (VText t) = Just t
    asText _         = Nothing

-- ---------------------------------------------------------------------------
-- Pattern types and matching

data TypeConstraint
  = ExactType !Text
  | AnyType
  deriving stock (Show, Eq)

data AttrConstraint
  = AttrEquals !Value
  | AttrBound  !VarName
  | AttrExists
  | AttrAbsent
  deriving stock (Show, Eq)

data Pattern = Pattern
  { matchType  :: !TypeConstraint
  , matchAttrs :: !(Map Text AttrConstraint)
  , bindAs     :: !(Maybe VarName)
  } deriving stock (Show, Eq)

data MatchResult
  = NoMatch
  | Matched !Binding
  deriving stock (Show, Eq)

-- | Match a pattern against an active normal fact.
-- Type constraint is checked first (fast path); attribute constraints are
-- evaluated only on a type match.
matchPattern :: Binding -> Pattern -> ActiveFact -> MatchResult
matchPattern binding pat af =
  let fact  = untagFact af
      ftype = factRecordType fact
      attrs = factRecordAttrs fact
  in case checkTypeConstraint (matchType pat) ftype of
       False -> NoMatch
       True  ->
         case runAttrMatches binding (Map.toList (matchAttrs pat)) attrs of
           Nothing       -> NoMatch
           Just binding' ->
             let binding'' = case bindAs pat of
                   Nothing  -> binding'
                   Just var -> Map.insert var (VText ftype) binding'
             in Matched binding''

checkTypeConstraint :: TypeConstraint -> Text -> Bool
checkTypeConstraint AnyType       _  = True
checkTypeConstraint (ExactType t) ft = t == ft

runAttrMatches
  :: Binding
  -> [(Text, AttrConstraint)]
  -> Attributes
  -> Maybe Binding
runAttrMatches b []               _     = Just b
runAttrMatches b ((key, ac):rest) attrs =
  applyAttrConstraint b key ac attrs >>= \b' -> runAttrMatches b' rest attrs

applyAttrConstraint
  :: Binding -> Text -> AttrConstraint -> Attributes
  -> Maybe Binding
applyAttrConstraint b key (AttrEquals v) attrs =
  case Map.lookup key attrs of
    Just v' | v' == v -> Just b
    _                 -> Nothing
applyAttrConstraint b key (AttrBound var) attrs =
  case Map.lookup key attrs of
    Nothing -> Nothing
    Just v  ->
      case Map.lookup var b of
        Nothing -> Just (Map.insert var v b)
        Just v' -> if v == v' then Just b else Nothing
applyAttrConstraint b key AttrExists attrs =
  if Map.member    key attrs then Just b else Nothing
applyAttrConstraint b key AttrAbsent attrs =
  if Map.notMember key attrs then Just b else Nothing

-- ---------------------------------------------------------------------------
-- Variable analysis helpers

patternBoundVars :: Pattern -> Set VarName
patternBoundVars pat =
  let attrVars = Set.fromList [ v | AttrBound v <- Map.elems (matchAttrs pat) ]
      asVar    = maybe Set.empty Set.singleton (bindAs pat)
  in attrVars <> asVar

templateFreeVars :: Template -> Set VarName
templateFreeVars = foldMap attrExprFreeVars . Map.elems . outAttrs

attrExprFreeVars :: AttrExpr -> Set VarName
attrExprFreeVars (Literal _)    = Set.empty
attrExprFreeVars (Bound var)    = Set.singleton var
attrExprFreeVars (Concat exprs) = foldMap attrExprFreeVars exprs

-- ---------------------------------------------------------------------------
-- UUID helper

unFactId :: FactId -> UUID
unFactId (FactId u) = u
