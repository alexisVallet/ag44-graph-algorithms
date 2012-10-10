{-|
This module defines a simple Graph type class for interfacing with
graphs, along with an adjacency matrix implementation and an
adjacency list implementation.
-}
module Graph (
  Graph(..)
  ) where

import Prelude hiding (all)
import Data.Set (Set, findMin, findMax, fromList, member)
import Data.Array (Array, bounds, assocs, Ix, listArray)
import Data.Foldable (all)
import Control.Arrow ((***))

{-|
Most general finite graph type class. Based on the following 
set-theoretic definition for finite graphs:

G = (V, E) is a graph iff:

* V is a finite set of vertices

* E is a subset of VxV called the set of edges

This definition is generalized to account for specific types of
graphs in which E is more than a set of pair of vertices:

* Non simple graphs, in which there can be more than one
edge between each pair of vertices.

* Labelled graphs, in which edges are associated a label.

Consequently, the edge is an associated datatype to the graph.

The following laws should hold true for any implementation:

* The vertex set specified on construction should be returned
by 'vertices':

@
vertices (fromVerticesAndEdges v e) == v
@

* The edges returned by 'edges' should be all the edges specified
on construction, minus the ones which had any of their vertices not
in the vertex set specified on construction:

@
edges (fromVerticesAndEdges v e) == e ``difference`` fromList [(i,j) | (i,j) <- e, not (member i v && member j v)]
@

Minimum complete definition: fromVerticesAndEdges, vertices,
edges, edgeVertices
-}
class Graph gr where
  -- | Datatype for vertices of the graph.
  type Vertex gr :: *
  -- | Datatype for edges of the graph.
  type Edge gr :: *
  -- | Constructs a graph from a vertex set and an edge set.
  -- If an edge in the edge set has either of its vertices outside
  -- of the vertex set, it should be ignored.
  fromVerticesAndEdges :: (Ord (Vertex gr), Ord (Edge gr))
                          => Set (Vertex gr) -> Set (Edge gr) -> gr
  -- | The set of vertices of the graph.
  vertices :: (Ord (Vertex gr)) => gr -> Set (Vertex gr)
  -- | The set of edges of the graph
  edges :: (Ord (Edge gr)) => gr -> Set (Edge gr)
  -- | The two vertices for an edge.
  edgeVertices :: Edge gr -> (Vertex gr, Vertex gr)

-- | Adjacency matrix, with 'a' parameter specifying the type of the
-- elements of the matrix, eg. Bool for simple graphs, Int for 
-- multigraphs. 'v' is the type of vertices of the graph, has to be
-- an instance of 'Ix'.
data AdjacencyMatrix v a where
     AdjacencyMatrix :: (Ix v, Enum v) => Array (v, v) a -> AdjacencyMatrix v a

-- | Adjacency list, with the same parameters as 'AdjacencyMatrix'.
data AdjacencyList v a where 
  AdjacencyList :: (Ix v, Enum v) => Array v [a] -> AdjacencyList v a

newtype SimpleGraph impl v = SimpleGraph (impl v Bool)

newtype MultiGraph impl v = MultiGraph (impl v Int)

type SimpleGraphAL v = SimpleGraph AdjacencyList v

type SimpleGraphAM v = SimpleGraph AdjacencyMatrix v

type MultiGraphAL v = MultiGraph AdjacencyList v

type MultiGraphAM v = MultiGraph AdjacencyMatrix v

instance (Ix v, Enum v) => Graph (SimpleGraph AdjacencyMatrix v) where
  type Vertex (SimpleGraph AdjacencyMatrix v) = v
  type Edge (SimpleGraph AdjacencyMatrix v) = (v, v)
  fromVerticesAndEdges vertices edges =
    let (min,max) = (findMin vertices, findMax vertices) in
    SimpleGraph
    $ AdjacencyMatrix
    $ listArray ((min,max), (min,max))
    [member (i,j) edges | i <- [min..max], j <- [min..max]]
  vertices (SimpleGraph (AdjacencyMatrix matrix)) =
    let (min,max) = fst $ bounds $ matrix in
    fromList [min..max]
  edges (SimpleGraph (AdjacencyMatrix matrix)) =
    fromList $ map fst $ filter snd $ assocs matrix
  edgeVertices = id
