module Analytics.Core.Graph
  ( Graph
  , emptyGraph
  , NodeId(..)
  , GraphNode(..)
  , GraphEdge(..)
  , EdgeType(..)
  , NodeStatus(..)
  , Direction(..)
  , addNode
  , addEdge
  , markNodeInactive
  , neighbors
  , reachable
  , pathsBetween
  , hasCycle
  , detectCycle
  , subgraph
  , inDegree
  , outDegree
  , reservedEdgeTypeNames
  ) where

import Relude
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Control.Monad (foldM)
import Analytics.Core.Types

-- ---------------------------------------------------------------------------
-- Node and edge types

data NodeId
  = FactNode     !FactId
  | RuleNode     !RuleId
  | EvidenceNode !EvidenceId
  deriving stock (Eq, Ord, Show)

data NodeStatus = NodeActive | NodeInactive
  deriving stock (Eq, Show)

data GraphNode = GraphNode
  { gnNodeId :: !NodeId
  , gnLabel  :: !Text
  , gnStatus :: !NodeStatus
  } deriving stock (Show, Eq)

data GraphEdge = GraphEdge
  { geFrom :: !NodeId
  , geTo   :: !NodeId
  , geType :: !EdgeType
  } deriving stock (Show, Eq, Ord)

data EdgeType
  = DerivedFrom
  | AppliedRule
  | Supports
  | Contradicts
  | DependsOn
  | PluginEdge !Text
  deriving stock (Eq, Ord, Show)

reservedEdgeTypeNames :: Set Text
reservedEdgeTypeNames = Set.fromList ["derived-from", "applied-rule", "depends-on"]

-- ---------------------------------------------------------------------------
-- Graph representation

data Graph = Graph
  { gNodes    :: !(Map NodeId GraphNode)
  , gOutEdges :: !(Map NodeId (Set GraphEdge))
  , gInEdges  :: !(Map NodeId (Set GraphEdge))
  } deriving stock (Show)

emptyGraph :: Graph
emptyGraph = Graph Map.empty Map.empty Map.empty

-- ---------------------------------------------------------------------------
-- Operations

addNode :: GraphNode -> Graph -> Graph
addNode n g = g { gNodes = Map.insertWith keepExisting (gnNodeId n) n (gNodes g) }
  where keepExisting _new old = old

addEdge :: GraphEdge -> Graph -> Graph
addEdge e g = g
  { gOutEdges = Map.insertWith Set.union (geFrom e) (Set.singleton e) (gOutEdges g)
  , gInEdges  = Map.insertWith Set.union (geTo   e) (Set.singleton e) (gInEdges  g)
  }

markNodeInactive :: NodeId -> Graph -> Graph
markNodeInactive nid g =
  g { gNodes = Map.adjust (\n -> n { gnStatus = NodeInactive }) nid (gNodes g) }

data Direction = Inbound | Outbound | Both
  deriving stock (Show, Eq)

neighbors :: NodeId -> Direction -> Graph -> [NodeId]
neighbors nid dir g =
  let outNbrs = fmap geTo   . Set.toList . fromMaybe Set.empty $ Map.lookup nid (gOutEdges g)
      inNbrs  = fmap geFrom . Set.toList . fromMaybe Set.empty $ Map.lookup nid (gInEdges  g)
  in case dir of
       Outbound -> outNbrs
       Inbound  -> inNbrs
       Both     -> outNbrs <> inNbrs

reachable :: NodeId -> Graph -> Set NodeId
reachable start g = go (Set.singleton start) (Set.singleton start)
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let next     = foldMap (\n -> Set.fromList (neighbors n Outbound g)) frontier
              newNodes = Set.difference next visited
          in go (Set.union visited newNodes) newNodes

-- | Find all simple paths from @src@ to @dst@ up to length @maxLen@.
--
-- Bug fix (Bug 8): @dst@ is added to @onPath@ before expanding neighbours.
-- Without this, a node that appears both as an intermediate hop and as the
-- final destination could be visited twice in the same path — once mid-path
-- and once as the terminal — producing paths where @dst@ is not only the
-- last node. Adding @dst@ to the visited set ensures the traversal can only
-- arrive at @dst@ by terminating, never by passing through it.
pathsBetween :: NodeId -> NodeId -> Int -> Graph -> [[NodeId]]
pathsBetween src dst maxLen g = go src Set.empty [] maxLen
  where
    go current onPath pathAcc remaining
      | current == dst = [reverse (current : pathAcc)]
      | remaining == 0 = []
      | otherwise      =
          -- Include dst in onPath so we cannot pass through it as an
          -- intermediate node. A path where dst appears mid-path and again
          -- at the end is semantically invalid for provenance chains.
          let nexts     = neighbors current Outbound g
              onPath'   = Set.insert current (Set.insert dst onPath)
              pathAcc'  = current : pathAcc
              unvisited = filter (`Set.notMember` onPath') nexts
          in concatMap (\n -> go n onPath' pathAcc' (remaining - 1)) unvisited

hasCycle :: Graph -> Bool
hasCycle = isJust . detectCycle

detectCycle :: Graph -> Maybe [NodeId]
detectCycle g = go (Map.keys (gNodes g)) Set.empty
  where
    go [] _          = Nothing
    go (n:ns) visited
      | Set.member n visited = go ns visited
      | otherwise            =
          case dfs n visited Set.empty [n] of
            Left cycle_    -> Just cycle_
            Right visited' -> go ns visited'

    dfs node visited inStack path
      -- IMPORTANT: inStack must be checked before visited.
      -- A node in `visited` from a prior DFS call may also be on the current
      -- call stack (inStack). Checking visited first would short-circuit to
      -- Right, masking the back-edge and failing to detect the cycle.
      -- Correct colouring: inStack = grey (in progress), visited = black (done).
      | Set.member node inStack =
          let fullPath = reverse (node : path)
          in Left (dropWhile (/= node) fullPath)
      | Set.member node visited = Right visited
      | otherwise =
          let visited' = Set.insert node visited
              inStack' = Set.insert node inStack
              nexts    = neighbors node Outbound g
          in foldM (\v n -> dfs n v inStack' (n : path)) visited' nexts

subgraph :: Set NodeId -> Graph -> Graph
subgraph nodes g = Graph
  { gNodes    = Map.filterWithKey (\k _ -> Set.member k nodes) (gNodes g)
  , gOutEdges = Map.mapMaybe filterEdges
      $ Map.filterWithKey (\k _ -> Set.member k nodes) (gOutEdges g)
  , gInEdges  = Map.mapMaybe filterEdges
      $ Map.filterWithKey (\k _ -> Set.member k nodes) (gInEdges g)
  }
  where
    filterEdges edges =
      let s = Set.filter (\e -> Set.member (geTo e) nodes) edges
      in if Set.null s then Nothing else Just s

inDegree :: NodeId -> Graph -> Int
inDegree nid g = maybe 0 Set.size (Map.lookup nid (gInEdges g))

outDegree :: NodeId -> Graph -> Int
outDegree nid g = maybe 0 Set.size (Map.lookup nid (gOutEdges g))
