{-|
This module implements Tarjan's algorithm to determine the strongly
connected components of a directed graph.
-}
module StronglyConnectedComponents
       (
         tarjan
       ) where

import Data.Array as Array
import Data.IntMap as IntMap
import Prelude as Prelude hiding ((.))
import Control.Category
import Control.Monad.RWS.Strict
import Control.Lens
import Control.Lens.IndexedLens
import Control.Lens.TH
import Control.Monad
import Control.Applicative
import Data.Maybe

import GraphUtils

-- | Data associated to a single vertex in Tarjan's algorithm.
data VertexData = VertexData {
  _index :: Int, -- | Pre-ordering index of the vertex
  _lowLink :: Int, -- | Index of the root of the connected component
  _isInStack :: Bool -- | True iff the vertex is in the stack of the visited vertex, as an optimisation to avoid O(n) lookup in the stack.
  }
makeLenses ''VertexData

-- | State threaded throughout tarjan's algorithm.
data SCCState = SCCState {
  _globalIndex :: Int, -- | Current global index, indicates the pre-ordering of a newly visited vertex.
  _verticesData :: IntMap VertexData, -- | Map storing informations about the vertices. If it is undefined for a vertex, then this vertex has not been visited yet.
  _visitedStack :: [Vertex], -- | Stack storing the visited vertex. Vertices are pushed to the stack when first visited, then popped once the root of the strongly connected component has been reached.
  _sccs :: [[Vertex]]
  }
makeLenses ''SCCState

-- | The SCC monad for Tarjan's algorithm, is a reader on the input graph, a writer on the output strongly connected components, and threads the necessary state.
type SCC = RWS Graph () SCCState

-- | Runs a computation in the SCC monad.
runSCC :: Graph -> SCC a -> [[Vertex]]
runSCC graph sccAction = do
  let (min,max) = bounds graph
      numberOfVertices = max - min + 1
      initialVertexData = IntMap.empty
      initialState = SCCState 0 initialVertexData [] []
  (fst $ execRWS sccAction graph initialState) ^. sccs

-- | Returns the strongly connected components of the graph,
-- computed using Tarjan's algorithm.
tarjan :: Graph -> [[Vertex]]
tarjan graph = runSCC graph tarjanSCC

-- | Main loop procedure for Tarjan's algorithm, iterates
-- over all vertices in the graph which have not yet been
-- associated to a strongly connected component.
tarjanSCC :: SCC ()
tarjanSCC = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    mVertexData <- use $ verticesData . at vertex
    when (isNothing mVertexData) $ do
      strongConnect vertex

-- | Visits the current vertices and its unvisited successors
-- recursively to determine the stongly connected component of
-- a vertex.
strongConnect :: Vertex -> SCC ()
strongConnect vertex = do
  -- Marks the vertex as visited and pushes it to the stack.
  currentIndex <- use globalIndex
  verticesData . at vertex .= (Just $ VertexData currentIndex currentIndex True)
  globalIndex += 1
  visitedStack %= (vertex:)
  graph <- ask
  -- Iterates over each successor of the current vertex. If the
  -- successor has not been visited, visit it recursively. Determines
  -- the new root of the strongly connected component accordingly.
  forM_ (successors graph vertex) $ \successor -> do
    mSuccessorData <- use $ verticesData . at successor
    case mSuccessorData of
      Nothing -> do
        strongConnect successor
        newSuccessorData <- fmap fromJust $ use $ verticesData . at successor
        verticesData . at vertex %= 
          (fmap $ lowLink %~ (min $ newSuccessorData ^. lowLink))
      Just successorData -> when (successorData ^. isInStack) $ do
        verticesData . at vertex %=
          (fmap $ lowLink %~ (min $ successorData ^. index))
  vertexData <- fmap fromJust $ use $ verticesData . at vertex
  -- | If the current vertex is the root, then we have found a
  -- strongly connected component. Constructs if by popping the
  -- stack.
  when (vertexData ^. lowLink == vertexData ^. index) $ do
    scc <- buildCurrentSCC vertex
    sccs %= (scc:)

-- | Builds the current strongly connected component
-- from its root by popping the stack.
buildCurrentSCC :: Vertex -> SCC [Vertex]
buildCurrentSCC root = do
  vertex <- pop
  if vertex == root 
    then do
      return [vertex]
    else do
      rest <- buildCurrentSCC root
      return (vertex:rest)

-- | Pops the stack of visited nodes.
pop :: SCC Vertex
pop = do
  stack <- use visitedStack
  visitedStack %= Prelude.tail
  let poppedVertex = Prelude.head stack
  verticesData . at poppedVertex %=
    (fmap $ isInStack .~ False)
  return poppedVertex
