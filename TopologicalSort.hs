{-|
This module provides a function for computing the topological sort
of a directed acyclic graph.
-}
module TopologicalSort
       (
         topologicalSort
       ) where

import GraphUtils
import Data.IntSet as IntSet
import Control.Monad.RWS
import Control.Monad
import Control.Lens
import Control.Lens.TH

-- | The state threaded through the topological sort algorithm. Simply
-- tracks visited vertices.
data TSState = TSState {
  _visitedVertices :: IntSet,
  _topologicalSorting :: [Vertex]
  }
makeLenses ''TSState

-- | Monad for the topological sort.
type TopSort = RWS Graph () TSState

-- | Runs a computation in the topological sort monad.
runTopSort :: Graph -> TopSort a -> [Vertex]
runTopSort graph tssAction = 
  (fst $ execRWS tssAction graph (TSState empty [])) 
  ^. topologicalSorting

-- | Computes a topological sort of a directed acyclic graph.
-- Uses a DFS, runs in O(|V|+|E|).
topologicalSort :: Graph -> [Vertex]
topologicalSort graph = runTopSort graph topologicalSort'

-- | Computes a topological sort in the topological sort monad.
-- Simply the main loop of a DFS.
topologicalSort' :: TopSort ()
topologicalSort' = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    visit vertex

-- | Visits a vertex, visits its successors, then adds itself
-- to the head of the topological sort, which puts all vertices
-- in their post ordering, therefore yielding a topological sort.
visit :: Vertex -> TopSort ()
visit vertex = do
  visitedVertices' <- use visitedVertices
  when (notMember vertex visitedVertices') $ do
    visitedVertices %= insert vertex
    graph <- ask
    forM_ (successors graph vertex) $ \successor -> do
      visit successor
    topologicalSorting %= (vertex:)
