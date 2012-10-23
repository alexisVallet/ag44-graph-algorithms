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
         adjacencyMatrix
       ) where

import Data.Graph (Graph(..),Vertex)
import Data.Array

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
  array ((min,max),(min,max)) 
  $ [((i,j),j `elem` (graph!i)) | i <- vertices', j <- vertices']
