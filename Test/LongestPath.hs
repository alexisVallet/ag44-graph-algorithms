{-|
Unit tests module for the longest path algorithm. Checks that the
following properties hold true for randomly generated DAGs:

* The only DAG in in which the longest path is empty is the DAG
of order 0.

* If the longest path is not empty, then the first vertex has
no predecessors and the last no successors in the graph.

-}
module Test.LongestPath (testLongestPath) where

import Test.ArbitraryInstances
import LongestPath
import GraphUtils

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- | Test group aggregating all tests in this module.
testLongestPath :: Test
testLongestPath = testGroup "LongestPath.longestPath" 
  [testProperty 
   "The longest path is only empty when given an empty DAG"
   pEmptyPath,
   testProperty 
   "The first and last vertex of the longest path are a source and sink respectively" 
   pFirstLastSourceSink]

-- | Checks that the algorithm returns an empty path iff the graph
-- is of order 0 (ie it has no vertices and no edges).
pEmptyPath :: DAG -> Bool
pEmptyPath (DAG graph) =
  let 
    actualLongestPath = longestPath graph 
    isGraphEmpty = order graph == 0
    isLongestPathEmpty = actualLongestPath == [] in
  (if isGraphEmpty then isLongestPathEmpty else True) &&
  (if isLongestPathEmpty then isGraphEmpty else True)

-- | Checks that the first vertex in the longest path is
-- a source vertex (it has no incoming edges) and that the
-- last vertex in the longest path is a sink vertex (it has
-- no outgoing edges).
pFirstLastSourceSink :: DAG -> Bool
pFirstLastSourceSink (DAG graph) =
  let actualLongestPath = longestPath graph in
  if null actualLongestPath
  then True
  else
    isSource graph (head actualLongestPath)
    && isSink graph (last actualLongestPath)

-- | A vertex is a source iff it has no predecessors.
isSource :: Graph -> Vertex -> Bool
isSource graph vertex =
  null $ predecessors graph vertex

-- | A vertex is a sink iff it has no successors
isSink :: Graph -> Vertex -> Bool
isSink graph vertex =
  null $ successors graph vertex
