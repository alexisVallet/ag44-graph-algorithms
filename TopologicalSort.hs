{-|
This module provides a function for computing the topological sort
of a directed acyclic graph.
-}
module TopologicalSort
       (
         topologicalSort
       ) where

import Prelude hiding (replicate)
import Data.Vector.Unboxed.Mutable
import Control.Monad.RWS
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Control.Monad.ST

import GraphUtils
import VectorUtils

-- | The state threaded through the topological sort algorithm. Simply
-- tracks visited vertices.
data TSState s = TSState {
  _visitedVertices :: STVector s Bool,
  _topologicalSorting :: [Vertex]
  }
makeLenses ''TSState

-- | Monad for the topological sort.
type TopSort s = RWST Graph () (TSState s) (ST s)

-- | Runs a computation in the topological sort monad.
runTopSort :: Graph -> (forall s . TopSort s a) -> [Vertex]
runTopSort graph tssAction = runST $ do
  initialVisitedVertices <- replicate (order graph) False
  (finalState,_) <- 
    execRWST tssAction graph (TSState initialVisitedVertices [])
  return $ finalState ^. topologicalSorting

-- | Computes a topological sort of a directed acyclic graph.
-- Uses a DFS, runs in O(|V|+|E|).
topologicalSort :: Graph -> [Vertex]
topologicalSort graph = runTopSort graph topologicalSort'

-- | Computes a topological sort in the topological sort monad.
-- Simply the main loop of a DFS.
topologicalSort' :: TopSort s ()
topologicalSort' = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    visit vertex

-- | Visits a vertex, visits its successors, then adds itself
-- to the head of the topological sort, which puts all vertices
-- in their post ordering, therefore yielding a topological sort.
visit :: Vertex -> TopSort s ()
visit vertex = do
  isVisited <- readL visitedVertices vertex
  when (not isVisited) $ do
    writeL visitedVertices vertex True
    graph <- ask
    forM_ (successors graph vertex) $ \successor -> do
      visit successor
    topologicalSorting %= (vertex:)
