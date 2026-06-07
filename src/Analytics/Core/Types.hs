module Analytics.Core.Types
  ( -- * Identifiers
    FactId(..)
  , RuleId(..)
  , EvidenceId(..)
  , PluginId
  , mkPluginId
  , unPluginId
  , systemPluginId
  , SnapshotId
  , mkSnapshotId
  , unSnapshotId
  , initialSnapshotId
  , nextSnapshotId
    -- * Versioning
  , Version(..)
    -- * Value domain
  , Value(..)
  , Attributes
    -- * Variable binding
  , VarName(..)
  , Binding
    -- * Inference termination
  , TerminationReason(..)
  , LimitReason(..)
  ) where

import Relude
import Data.Char       (isAlphaNum, isAscii)
import Data.Map.Strict ()
import Data.UUID       (UUID)
import Data.Time       ()
import Numeric.Natural ()
import qualified Data.Text as T
-- ---------------------------------------------------------------------------
-- Identifiers

newtype FactId     = FactId     UUID
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

newtype RuleId     = RuleId     UUID
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

newtype EvidenceId = EvidenceId UUID
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

newtype PluginId = PluginId Text
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

mkPluginId :: Text -> Either Text PluginId
mkPluginId t
  | T.null t               = Left "PluginId must not be empty"
  | not (T.all validChar t) = Left ("PluginId contains invalid characters: " <> t)
  | otherwise              = Right (PluginId t)
  where
    validChar c = (isAscii c && isAlphaNum c) || c == '-' || c == '.'

unPluginId :: PluginId -> Text
unPluginId (PluginId t) = t

-- | The canonical system-owned namespace. Used as a fallback when a fact
-- type has no valid plugin namespace prefix. Constructed directly from the
-- internal 'PluginId' constructor; validity is structurally evident by
-- inspection — "system" satisfies all 'mkPluginId' constraints (non-empty,
-- ASCII alphanumeric). Defined here so callers never need to reconstruct it
-- via 'mkPluginId' and never need to handle an impossible Left branch.
systemPluginId :: PluginId
systemPluginId = PluginId "system"

-- SnapshotId: monotonically increasing Word64, starts at 1.
-- Never construct with minBound/toEnum/fromIntegral.
-- Always use mkSnapshotId, initialSnapshotId, or nextSnapshotId.
newtype SnapshotId = SnapshotId Word64
  deriving stock   (Show, Generic)
  deriving newtype (Eq, Ord, Hashable)

mkSnapshotId :: Word64 -> Either Text SnapshotId
mkSnapshotId 0 = Left "SnapshotId must be >= 1"
mkSnapshotId n = Right (SnapshotId n)

unSnapshotId :: SnapshotId -> Word64
unSnapshotId (SnapshotId n) = n

initialSnapshotId :: SnapshotId
initialSnapshotId = SnapshotId 1

nextSnapshotId :: SnapshotId -> Either Text SnapshotId
nextSnapshotId (SnapshotId n)
  | n == maxBound = Left "SnapshotId overflow: system has reached maximum mutation count"
  | otherwise     = Right (SnapshotId (n + 1))

-- ---------------------------------------------------------------------------
-- Versioning

data Version = Version
  { versionMajor :: !Word32
  , versionMinor :: !Word32
  , versionPatch :: !Word32
  } deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Value domain

data Value
  = VText  !Text
  | VInt   !Int64
  | VFloat !Double
  | VBool  !Bool
  | VNull
  deriving stock (Eq, Ord, Show)

type Attributes = Map Text Value

-- ---------------------------------------------------------------------------
-- Variable binding

newtype VarName = VarName Text
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Hashable)

type Binding = Map VarName Value

-- ---------------------------------------------------------------------------
-- Inference termination reasons
-- Defined in Types so Event.Types can reference them without importing Inference.

data TerminationReason
  = FixedPoint
  | LimitReached   !LimitReason
  | ContradictionHalt
  deriving stock (Show, Eq)

data LimitReason
  = MaxFactsExceeded !Natural
  | MaxDepthExceeded !Natural
  | TimeoutExceeded  !Natural   -- elapsed milliseconds (LOGIC-08)
  deriving stock (Show, Eq)