{-|
This module implements Tarjan's algorithm to determine the strongly
connected components of a directed graph.
-}
module StronglyConnectedComponents
       (
         tarjan
       ) where

import Data.Array as Array
import Data.Vector.Unboxed.Mutable as Vector
import Prelude as Prelude hiding ((.),replicate,read)
import Control.Category
import Control.Monad.RWS.Strict
import Control.Lens
import Control.Lens.IndexedLens
import Control.Lens.TH
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import Data.Maybe

import GraphUtils
import VectorUtils

-- | State threaded throughout tarjan's algorithm.
data SCCState s = SCCState {
  _globalIndex :: Int,
  _verticesIndex :: STVector s Int,
  _verticesLowLink :: STVector s Int,
  _verticesInStack :: STVector s Bool,
  _visitedStack :: [Vertex],
  _sccs :: [[Vertex]]
  }
makeLenses ''SCCState

-- | The SCC monad for Tarjan's algorithm, is a reader on the input graph, a writer on the output strongly connected components, and threads the necessary state.
type SCC s = RWST Graph () (SCCState s) (ST s)

-- | Runs a computation in the SCC monad.
runSCC :: Graph -> (forall s . SCC s a) -> [[Vertex]]
runSCC graph sccAction = runST $ do
  let numberOfVertices = order graph
  initialIndices <- replicate numberOfVertices (-1)
  initialLowLinks <- replicate numberOfVertices (-1)
  initialInStack <- replicate numberOfVertices False
  let initialState = 
        SCCState 0 initialIndices initialLowLinks initialInStack [] []
  (lastState,_) <- execRWST sccAction graph initialState
  return $ lastState ^. sccs

-- | Returns the strongly connected components of the graph,
-- computed using Tarjan's algorithm.
tarjan :: Graph -> [[Vertex]]
tarjan graph = runSCC graph tarjanSCC

-- | Main loop procedure for Tarjan's algorithm, iterates
-- over all vertices in the graph which have not yet been
-- associated to a strongly connected component.
tarjanSCC :: SCC s ()
tarjanSCC = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    vertexIndex <- readL verticesIndex vertex
    when (vertexIndex < 0) $ do
      strongConnect vertex

-- | Visits the current vertices and its unvisited successors
-- recursively to determine the stongly connected component of
-- a vertex.
strongConnect :: Vertex -> SCC s ()
strongConnect vertex = do
  -- Marks the vertex as visited and pushes it to the stack.
  currentIndex <- use globalIndex
  writeL verticesIndex vertex currentIndex 
  writeL verticesLowLink vertex currentIndex 
  writeL verticesInStack vertex True
  globalIndex += 1
  visitedStack %= (vertex:)
  graph <- ask
  -- Iterates over each successor of the current vertex. If the
  -- successor has not been visited, visit it recursively. Determines
  -- the new root of the strongly connected component accordingly.
  forM_ (successors graph vertex) $ \successor -> do
    successorIndex <- readL verticesIndex successor
    if successorIndex < 0 
      then do
        strongConnect successor
        newSuccessorLowLink <- readL verticesLowLink successor
        modifyL verticesLowLink vertex $ min newSuccessorLowLink
      else do
        successorInStack <- readL verticesInStack successor
        when successorInStack $ do
        modifyL verticesLowLink vertex $ min successorIndex
  vertexIndex <- readL verticesIndex vertex
  vertexLowLink <- readL verticesLowLink vertex
  -- | If the current vertex is the root, then we have found a
  -- strongly connected component. Constructs if by popping the
  -- stack.
  when (vertexLowLink == vertexIndex) $ do
    scc <- buildCurrentSCC vertex
    sccs %= (scc:)

-- | Builds the current strongly connected component
-- from its root by popping the stack.
buildCurrentSCC :: Vertex -> SCC s [Vertex]
buildCurrentSCC root = do
  vertex <- pop
  if vertex == root 
    then do
      return [vertex]
    else do
      rest <- buildCurrentSCC root
      return (vertex:rest)

-- | Pops the stack of visited nodes.
pop :: SCC s Vertex
pop = do
  stack <- use visitedStack
  visitedStack %= Prelude.tail
  let poppedVertex = Prelude.head stack
  writeL verticesInStack poppedVertex False
  return poppedVertex
