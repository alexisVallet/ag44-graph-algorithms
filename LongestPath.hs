{-|
This module provides a function to compute the longest path
between two vertices in a connected directed acyclic graph.
The first vertex must be a vertex with no incoming edges,
the second must have no outgoing edges.
-}
module LongestPath (longestPath) where

import GraphUtils
import TopologicalSort

import Prelude hiding (read, replicate)
import Control.Monad.ST
import Control.Monad.RWS.Strict hiding (modify)
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Data.Vector.Mutable
import Data.Maybe

-- | Data associated to a vertex for the longest path algorithm.
-- Keeps track of the longest length to the vertex, and its
-- predecessor in the longest path containing it.
data LPVertexData = LPVertexData {
  _lengthTo :: Int,
  _mPredecessor :: Maybe Vertex
  }
makeLenses ''LPVertexData

-- | State threaded throughout the longest path algorithm.
-- Keeps track of the data associated to each vertex, and
-- of the best last vertex in the path.
data LPState s = LPState {
  _verticesData :: STVector s LPVertexData,
  _bestLastVertex :: Maybe Vertex
  }
makeLenses ''LPState

-- | Longest path monad.
type LongestPath s = RWST Graph () (LPState s) (ST s)

-- | Runs a computation in the longest path monad.
runLongestPath :: Graph -> (forall s . LongestPath s a) -> [Vertex]
runLongestPath graph lpAction = runST $ do
  initialVerticesData <- 
    replicate (order graph) (LPVertexData 0 Nothing)
  let initialState = LPState initialVerticesData Nothing
  resultState <- 
    fmap fst
    $ execRWST lpAction graph initialState
  inversePath <- reconstructPath resultState
  return $ reverse inversePath

-- | Reconstructs the path from the final state in
-- the longest path algorithm.
reconstructPath :: LPState s -> ST s [Vertex]
reconstructPath state = 
  reconstructPath' state (state ^. bestLastVertex)

-- | Auxiliary function to reconstructPath', recurses over
-- predecessors until the entire path is found.
reconstructPath' :: LPState s -> Maybe Vertex -> ST s [Vertex]
reconstructPath' state mVertex =
  case mVertex of
    Nothing -> return []
    Just vertex -> do
      vertexData <- read (state ^. verticesData) vertex
      rest <- reconstructPath' state (vertexData ^. mPredecessor)
      return $ vertex:rest

-- | Computes the longest path in a directed acyclic graph.
-- Uses a dynamic programming approach to run in linear time.
-- If the graph is connected, and has only one source and only one
-- sink, then this will return the longest path between the source
-- and the sink.
longestPath :: Graph -> [Vertex]
longestPath graph =
  runLongestPath graph longestPath'

-- | Utility function to modify an element in a mutable vector.
modify :: STVector s a -> Int -> (a -> a) -> ST s a
modify vector index modification = do
  value <- read vector index
  let newValue = modification value
  write vector index newValue
  return newValue

-- | Computes the longest path inside de longest path monad.
longestPath' :: LongestPath s ()
longestPath' = do
  graph <- ask
  let topSort = topologicalSort graph
  case topSort of
    -- If we have a null graph then the best (only) path is empty,
    -- the loop will not get executed and the best vertex will stay
    -- Nothing which will result in an empty path.
    [] -> return ()
    -- Otherwise we initialize the best vertex as the first, so
    -- we can assume in the rest of the algorithm that the best vertex
    -- is defined.
    (first:_) -> do
      bestLastVertex .= Just first
  -- We take the vertices in the topological order, and examine each
  -- of their successors in turn.
  forM_ topSort $ \vertex -> do
    forM_ (successors graph vertex) $ \successor -> do
      verticesData' <- use $ verticesData
      vertexData <- lift $ read verticesData' vertex
      successorData <- lift $ read verticesData' successor
      -- When we have found a longer path to the successor, update
      -- length and predecessor label accordingly.
      when (successorData ^. lengthTo <= vertexData ^. lengthTo + 1) $ do
        lift 
          $ modify verticesData' successor 
          $ lengthTo .~ vertexData ^. lengthTo + 1
        newSuccessorData <- lift 
                            $ modify verticesData' successor
                            $ mPredecessor .~ Just vertex
        -- Updates the best end node if the path found is longer.
        currentBest <- fmap fromJust $ use bestLastVertex
        currentBestData <- lift $ read verticesData' currentBest
        bestLastVertex .=
          (Just $ if currentBestData ^. lengthTo 
                     < newSuccessorData ^. lengthTo
                  then successor
                  else currentBest)
