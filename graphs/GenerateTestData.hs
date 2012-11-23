module GenerateTestData (main) where

import Control.Monad.Random
import System.Random
import System.Environment
import Data.List
import Data.Graph
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import Control.Monad.Trans.Class
import System.Random
import Data.Array.ST
import Data.Array
import GHC.Arr

-- | Shuffles a list randomly. Taken from http://www.haskell.org/haskellwiki/Random_shuffle .
shuffle :: RandomGen g => [(Vertex,Vertex)] -> Rand g [(Vertex,Vertex)]
shuffle xs = do
  generator <- getSplit
  return $ runST $ flip evalRandT generator $ do
    let numberOfElements = length xs
    ar <- lift $ (newListArray (0,numberOfElements-1) xs :: ST s (STArray s Int (Vertex,Vertex)))
    forM_ [numberOfElements-1..1] $ \i -> do
      j <- getRandomR (0,i)
      vi <- lift $ readArray ar i
      vj <- lift $ readArray ar j
      lift $ writeArray ar i vj
      lift $ writeArray ar j vi
    lift $ getElems ar
    
-- | Generates a random graph with a given number of edges and
-- vertices. For benchmarking purposes, be careful to always
-- use the same starting seed for the random number generator for
-- repeatable results.
randomGraphOfSize :: (RandomGen g) => Int -> Int -> Rand g Graph
randomGraphOfSize numberOfVertices numberOfEdges = do
  let vertices = [0..numberOfVertices-1]
  edges <- fmap (take numberOfEdges) 
           $ shuffle [(i,j) | i <- vertices, j <- vertices]
  return
    $ adjacencyList
    $ array
    ((0,0),(numberOfVertices-1,numberOfVertices-1))
    [((i,j),(i,j) `elem` edges) | i <- vertices, j <- vertices]

-- | Converts from adjacency matrix to adjacency list.
adjacencyList :: Array (Vertex,Vertex) Bool -> Graph
adjacencyList adjMatrix =
  let ((min,_),(max,_)) = bounds adjMatrix
      vertices' = [min..max]
      successors' vertex = 
        findIndices id 
        $ [adjMatrix!(vertex,col) | col <- vertices'] in
  listArray (min,max) $ [successors' vertex | vertex <- vertices']


-- | Prints the adjacency matrix of the graph.
showAdjacencyMatrix :: Graph -> IO ()
showAdjacencyMatrix graph = do
  let adjMatrix = adjacencyMatrix graph
      vertices' = vertices graph
      (min,max) = bounds graph
      order = max - min + 1
  putStrLn $ show order
  forM_ vertices' $ \i -> do
    forM_ vertices' $ \j -> do
      putStr $ (if adjMatrix!(i,j) then "1" else "0") ++ " "
    putStrLn ""

-- | Returns the adjacency matrix of the graph.
adjacencyMatrix :: Graph -> Array (Vertex,Vertex) Bool
adjacencyMatrix graph =
  let vertices' = vertices graph
      (min,max) = bounds graph in
  array ((min,min),(max,max)) 
  $ [((i,j),j `elem` (graph!i)) | i <- vertices', j <- vertices']

main :: IO ()
main = do
  [numberOfVertices, numberOfEdges] <- fmap (map read) getArgs
  graph <- evalRandIO 
           $ randomGraphOfSize numberOfVertices numberOfEdges
  showAdjacencyMatrix graph
