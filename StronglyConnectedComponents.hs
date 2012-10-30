{-|
This module implements Tarjan's algorithm to determine the strongly
connected components of a directed graph.
-}
module StronglyConnectedComponents
       (
         tarjan
       ) where

import Data.Array as Array
import Data.Vector.Mutable as Vector
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

-- | Data associated to a single vertex in Tarjan's algorithm.
data VertexData = VertexData {
  _index :: Int, -- | Pre-ordering index of the vertex
  _lowLink :: Int, -- | Index of the root of the connected component
  _isInStack :: Bool -- | True iff the vertex is in the stack of the visited vertex, as an optimisation to avoid O(n) lookup in the stack.
  }
makeLenses ''VertexData

-- | State threaded throughout tarjan's algorithm.
data SCCState s = SCCState {
  _globalIndex :: Int, -- | Current global index, indicates the pre-ordering of a newly visited vertex.
  _verticesData :: STVector s (Maybe VertexData), -- | Map storing informations about the vertices. If it is undefined for a vertex, then this vertex has not been visited yet.
  _visitedStack :: [Vertex], -- | Stack storing the visited vertex. Vertices are pushed to the stack when first visited, then popped once the root of the strongly connected component has been reached.
  _sccs :: [[Vertex]]
  }
makeLenses ''SCCState

-- | The SCC monad for Tarjan's algorithm, is a reader on the input graph, a writer on the output strongly connected components, and threads the necessary state.
type SCC s = RWST Graph () (SCCState s) (ST s)

-- | Runs a computation in the SCC monad.
runSCC :: Graph -> (forall s . SCC s a) -> [[Vertex]]
runSCC graph sccAction = runST $ do
  initialVerticesData <- replicate (order graph) Nothing
  let initialState = SCCState 0 initialVerticesData [] []
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
    mVertexData <- readL verticesData vertex
    when (isNothing mVertexData) $ do
      strongConnect vertex

-- | Visits the current vertices and its unvisited successors
-- recursively to determine the stongly connected component of
-- a vertex.
strongConnect :: Vertex -> SCC s ()
strongConnect vertex = do
  -- Marks the vertex as visited and pushes it to the stack.
  currentIndex <- use globalIndex
  writeL verticesData vertex $ Just $ VertexData currentIndex currentIndex True
  globalIndex += 1
  visitedStack %= (vertex:)
  graph <- ask
  -- Iterates over each successor of the current vertex. If the
  -- successor has not been visited, visit it recursively. Determines
  -- the new root of the strongly connected component accordingly.
  forM_ (successors graph vertex) $ \successor -> do
    mSuccessorData <- readL verticesData successor
    case mSuccessorData of
      Nothing -> do
        strongConnect successor
        newSuccessorData <- fmap fromJust $ readL verticesData successor
        modifyL verticesData vertex $ 
          fmap $ lowLink %~ (min $ newSuccessorData ^. lowLink)
      Just successorData -> when (successorData ^. isInStack) $ do
        modifyL verticesData vertex $
          fmap $ lowLink %~ (min $ successorData ^. index)
  vertexData <- fmap fromJust $ readL verticesData vertex
  -- | If the current vertex is the root, then we have found a
  -- strongly connected component. Constructs if by popping the
  -- stack.
  when (vertexData ^. lowLink == vertexData ^. index) $ do
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
  modifyL verticesData poppedVertex $
    fmap $ isInStack .~ False
  return poppedVertex
