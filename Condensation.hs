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
import Data.List
import Control.Exception (assert)

-- | The condensation of a graph, contracting every
-- strongly connected component in the graph to a
-- single vertex. The result is a directed acyclic graph.
condensation :: Graph -> Graph
condensation graph = contraction graph (tarjan graph)

-- | Condensates a graph given its list of strongly connected
-- components.
contraction :: Graph -> [[Vertex]] -> Graph
contraction graph sccs =
  let successorsOf scc =
        nub $ map vertToComp $ foldr union [] (map (successors graph) scc)
      vertToComp vertex = 
        case findIndex (elem vertex) sccs of
          Nothing -> error 
                     $ "The vertex " 
                     ++ show vertex 
                     ++ " does not appear in any of the SCCS: "
                     ++ show sccs
          Just componentIndex -> componentIndex - 1 in
  listArray (0,length sccs - 1) $ map successorsOf sccs
