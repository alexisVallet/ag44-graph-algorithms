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

type TopSort = RWS Graph () TSState

runTopSort :: Graph -> TopSort a -> [Vertex]
runTopSort graph tssAction = 
  (fst $ execRWS tssAction graph (TSState empty [])) 
  ^. topologicalSorting

topologicalSort :: Graph -> [Vertex]
topologicalSort graph = runTopSort graph topologicalSort'

topologicalSort' :: TopSort ()
topologicalSort' = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    visit vertex

visit :: Vertex -> TopSort ()
visit vertex = do
  visitedVertices' <- use visitedVertices
  when (notMember vertex visitedVertices') $ do
    visitedVertices %= insert vertex
    graph <- ask
    forM_ (successors graph vertex) $ \successor -> do
      visit successor
    topologicalSorting %= (vertex:)
