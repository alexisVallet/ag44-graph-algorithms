module Test.LongestPath (testLongestPath) where

import Test.ArbitraryInstances
import LongestPath
import GraphUtils

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testLongestPath :: Test
testLongestPath = testGroup "LongestPath.longestPath" 
  [testProperty 
   "The longest path is only empty when given an empty DAG"
   pEmptyPath,
   testProperty 
   "The first and last vertex of the longest path are a source and sink respectively" 
   pFirstLastSourceSink]

pEmptyPath :: DAG -> Bool
pEmptyPath (DAG graph) =
  let 
    actualLongestPath = longestPath graph 
    isGraphEmpty = order graph == 0
    isLongestPathEmpty = actualLongestPath == [] in
  (if isGraphEmpty then isLongestPathEmpty else True) &&
  (if isLongestPathEmpty then isGraphEmpty else True)

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
