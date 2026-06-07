module Analytics.Core.Rule
  ( -- * Rule status (compile-time)
    RuleStatus(..)
  , TaggedRule(..)
  , EnabledRule
  , DisabledRule
  , enableRule
  , disableRule
  , untagRule
    -- * Rule record
  , Rule(..)
    -- * Priority (validated newtype)
  , Priority
  , mkPriority
  , unPriority
  , zeroPriority
    -- * Validation
  , RuleValidationError(..)
  , validateRule
    -- * Deterministic ordering
  , sortRules
  ) where

import Relude
import qualified Data.Set  as Set
import qualified Data.Map.Strict as Map

import Analytics.Core.Types
import Analytics.Core.Fact
  ( Pattern
  , Template(..)
  , patternBoundVars
  , templateFreeVars
  , hasEmptyConcat
  )

-- ---------------------------------------------------------------------------
-- Priority — validated newtype
--
-- The old `priority :: !Int` on Rule allowed negative values with no
-- documented meaning and no guard. Priority is now a non-negative Int
-- with a smart constructor. Zero is the default / lowest priority.

newtype Priority = Priority Int
  deriving stock   (Show)
  deriving newtype (Eq, Ord)

mkPriority :: Int -> Either Text Priority
mkPriority n
  | n < 0    = Left $ "Priority must be non-negative, got: " <> show n
  | otherwise = Right (Priority n)

unPriority :: Priority -> Int
unPriority (Priority n) = n

zeroPriority :: Priority
zeroPriority = Priority 0

-- ---------------------------------------------------------------------------
-- Rule status — compile-time phantom
--
-- The old design mirrored the Fact pattern: a plain `data SomeRule s` GADT
-- wrapping a bare `Rule`. The problem was identical — `extractRule` discarded
-- the phantom immediately, making the type-level distinction cosmetic.
--
-- The new design uses the same `TaggedRule` newtype approach as `TaggedFact`:
-- the phantom `s` is on the wrapper, and `untagRule` makes the information
-- loss explicit at the call site. The `Rule` record underneath never needs
-- to know its own status — that is the engine's concern.

data RuleStatus = Enabled | Disabled

newtype TaggedRule (s :: RuleStatus) = TaggedRule { unTagRule :: Rule }
  deriving stock (Show, Eq)

type EnabledRule  = TaggedRule 'Enabled
type DisabledRule = TaggedRule 'Disabled

enableRule :: Rule -> EnabledRule
enableRule = TaggedRule

disableRule :: Rule -> DisabledRule
disableRule = TaggedRule

-- | Project the underlying Rule, discarding the status phantom.
-- Use pattern matching on TaggedRule where status matters.
untagRule :: TaggedRule s -> Rule
untagRule = unTagRule

-- ---------------------------------------------------------------------------
-- Rule record

data Rule = Rule
  { ruleId       :: !RuleId
  , ruleName     :: !Text
  , premises     :: !(NonEmpty Pattern)  -- NonEmpty: at least one premise required
  , conclusion   :: !Template
  , priority     :: !Priority            -- non-negative, validated at construction
  , rulePluginId :: !PluginId
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Validation errors

data RuleValidationError
  = UnboundConclusionVariable VarName
    -- ^ A variable used in the conclusion is not bound by any premise pattern.
  | InvalidPluginId Text
    -- ^ The plugin id string failed mkPluginId validation.
  | EmptyConcatInTemplate Text
    -- ^ The conclusion template contains a 'Concat []' node at the named
    -- attribute key. 'Concat []' silently produces VText "" at runtime
    -- rather than signalling a missing expression. Rejected at rule-load
    -- time so it cannot reach the inference engine.
  deriving stock (Show, Eq)

-- | Validate a Rule.
-- NoPremises is no longer a possible error: `premises` is `NonEmpty Pattern`,
-- so the compiler rejects zero-premise rules at construction time.
-- Checks performed:
--   1. All variables used in the conclusion are bound by at least one premise.
--   2. No 'Concat []' node exists anywhere in the conclusion template.
validateRule :: Rule -> Either (NonEmpty RuleValidationError) ()
validateRule rule =
  checkConclusionVarsAllBound rule
  *> checkNoEmptyConcat rule

checkConclusionVarsAllBound :: Rule -> Either (NonEmpty RuleValidationError) ()
checkConclusionVarsAllBound rule =
  let boundByPremises  = foldMap patternBoundVars (premises rule)
      usedInConclusion = templateFreeVars (conclusion rule)
      unbound          = Set.difference usedInConclusion boundByPremises
  in case nonEmpty (Set.toList unbound) of
       Nothing  -> Right ()
       Just nel -> Left (fmap UnboundConclusionVariable nel)

-- | Reject any conclusion template that contains a 'Concat []' node.
-- An empty Concat produces VText "" silently, masking missing expressions
-- in rule templates (Bug 10). Caught here at rule-load time rather than
-- at inference time, so malformed rules never enter the enabled set.
checkNoEmptyConcat :: Rule -> Either (NonEmpty RuleValidationError) ()
checkNoEmptyConcat rule =
  case nonEmpty badKeys of
    Nothing  -> Right ()
    Just nel -> Left (fmap EmptyConcatInTemplate nel)
  where
    badKeys =
      [ k
      | (k, expr) <- Map.toList (outAttrs (conclusion rule))
      , hasEmptyConcat expr
      ]

-- ---------------------------------------------------------------------------
-- Deterministic sort (LOGIC-09)
--
-- Primary: priority descending (higher priority fires first).
-- Tiebreaker: ruleName lexicographic ascending (deterministic across runs).
-- `sortRules` works on any status-tagged rule list; the phantom `s` is
-- preserved — a list of EnabledRule sorts to a list of EnabledRule.

sortRules :: [TaggedRule s] -> [TaggedRule s]
sortRules = sortOn $ \r ->
  let rule = untagRule r
  in (Down (priority rule), ruleName rule)
