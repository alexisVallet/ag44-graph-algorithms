{-|
Contains utility functions to manipulate graphs.
-}
module GraphUtils
       (
         Graph(..),
         Vertex,
         vertices,
         edges,
         successors,
         adjacencyMatrix,
         adjacencyList,
         hasACycle
       ) where

import Data.Graph (Graph(..),Vertex)
import Data.List
import Data.Array
import Control.Monad.RWS.Lazy
import Control.Monad
import Control.Monad.Cont
import Control.Lens
import Control.Lens.IndexedLens
import Control.Lens.TH
import Data.IntMap hiding ((!))

-- | Returns the successors of a vertex in a graph.
successors :: Graph -> Vertex -> [Vertex]
successors = (!)

-- | Returns the vertices of the graph.
vertices :: Graph -> [Vertex]
vertices graph =
  let (min,max) = bounds graph in
  [min..max]

edges :: Graph -> [(Vertex,Vertex)]
edges graph = 
  concatMap 
  (\vertex -> zip (repeat vertex) (successors graph vertex)) 
  $ vertices graph

-- | Returns the adjacency matrix of the graph.
adjacencyMatrix :: Graph -> Array (Vertex,Vertex) Bool
adjacencyMatrix graph =
  let vertices' = vertices graph
      (min,max) = bounds graph in
  array ((min,min),(max,max)) 
  $ [((i,j),j `elem` (graph!i)) | i <- vertices', j <- vertices']

-- | Converts from adjacency matrix to adjacency list.
adjacencyList :: Array (Vertex,Vertex) Bool -> Graph
adjacencyList adjMatrix =
  let ((min,_),(max,_)) = bounds adjMatrix
      vertices' = [min..max]
      successors' vertex = 
        findIndices id 
        $ [adjMatrix!(vertex,col) | col <- vertices'] in
  listArray (min,max) $ [successors' vertex | vertex <- vertices']

-- | Vertex color for the cycle detection algorithm.
-- Gray if it has been visited but not all its successors have,
-- Black if it has been visited and so have all its successors.
data CDVertexColor = Gray | Black

-- | State for the cycle detection algorithm. Keeps track of which
-- vertex were visited, and whether or not all their respective
-- successors have been visited.
data CDState = CDState {
  _verticesColor :: IntMap CDVertexColor
  }
makeLenses ''CDState

-- | Monad for cycle detection. Threads the color of vertices, with
-- an escape continuation to exit as soon as a cycle is found.
type CDMonad = ContT Bool (RWS Graph () CDState)

runCycleDetection :: Graph 
                     -> CDMonad Bool
                     -> Bool
runCycleDetection graph cdAction =
  fst 
  $ evalRWS
  (runContT cdAction return)
  graph
  (CDState empty)

-- | Checks whether the graph has a cycle.
hasACycle :: Graph -> Bool
hasACycle graph = runCycleDetection graph hasACycle'

-- | Checks whether the graph has a cycle within the cycle detection
-- monad.
hasACycle' :: CDMonad Bool
hasACycle' = callCC $ \exit -> do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    mVertexColor <- use $ verticesColor . at vertex
    case mVertexColor of
      Nothing -> cdVisit exit vertex
      Just _ -> return ()
  return False

cdVisit :: (Bool -> CDMonad ()) -> Vertex -> CDMonad ()
cdVisit exit vertex = do
  verticesColor . at vertex .= Just Gray
  graph <- ask
  forM_ (successors graph vertex) $ \successor -> do
    mSuccessorData <- use $ verticesColor . at successor
    case mSuccessorData of
      Nothing -> do
        cdVisit exit successor
      Just Gray -> do
        exit True
      Just Black -> do
        return ()
  verticesColor . at vertex .= Just Black
