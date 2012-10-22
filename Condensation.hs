{-|
This module provides a function to compute the condensation of a graph,
ie. the original graph with its strongly connected components
contracted to a single vertex.
-}
module Condensation
       (
         condensation
       ) where

import StronglyConnectedComponents

import GraphUtils
import Data.Array as Array

-- | The condensation of a graph, contracting every
-- strongly connected component in the graph to a
-- single vertex. The result is a directed acyclic graph.
condensation :: Graph -> Graph
condensation graph = contraction graph (tarjan graph)

-- | Condensate a graph given its list of strongly connected
-- components.
contraction :: Graph -> [[Vertex]] -> Graph
contraction graph sccs = undefined
  -- let
  --   compList = [0..]
  --   size = length sccs
  --   bounds' = bounds graph
  --   vertToCompArray = 
  --     array (bounds',bounds')
  --     $ concat
  --     $ zipWith (\scc comp -> map ((,) comp) scc) sccs compList in
  -- array (0, size-1) $ map 