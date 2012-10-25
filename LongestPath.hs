{-|
This module provides a function to compute the longest path
between two vertices in a connected directed acyclic graph.
The first vertex must be a vertex with no incoming edges,
the second must have no outgoing edges.
-}
module LongestPath (longestPath) where

import GraphUtils
import TopologicalSort

import Control.Monad.RWS
import Control.Monad
import Control.Lens
import Control.Lens.TH

data LPVertexData = LPVertexData {
  _lengthTo :: Int,
  _predecessor :: Vertex
  }
makeLenses ''LPVertexData

data LPState = LPState {
  _vertexData :: IntMap LPVertexData
  }
makeLenses ''LPState

type LP = RWS Graph () LPState

runLongestPath :: Graph -> LP a -> [Vertex]

absoluteLongestPath :: Graph -> [Vertex]
absoluteLongestPath graph = undefined