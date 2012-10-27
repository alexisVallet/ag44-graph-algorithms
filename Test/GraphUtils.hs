{-|
Unit tests module for the GraphUtils module. Checks non-obvious
properties of the functions defined. Notably:

* Checks that conversions between adjacency matrices and adjacency
lists yields isomorphic graphs.

-}
module Test.GraphUtils (testGraphUtils) where

import Test.ArbitraryInstances
import GraphUtils

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.List
import Data.Array

-- | Test group aggregating all properties of this module.
testGraphUtils :: Test
testGraphUtils =
  testGroup "GraphUtils"
  [testProperty 
   "adjacencyList is the inverse of adjacencyMatrix"
   pAdjListAdjMatrixInverse,
   testProperty
   "adjacencyMatrix is the inverse of adjacencyList"
   pAdjMatrixAdjListInverse,
   testProperty
   "hasACycle returns False when given a DAG"
   pHasACycleFalseOnDAG]

-- | Tests whether two graph are equals, ignoring the order
-- of vertices in the adjacency lists.
graphEq :: Graph -> Graph -> Bool
graphEq graph1 graph2 =
  all 
  (\(l1,l2) -> sort l1 == sort l2) 
  $ zip (elems graph1) (elems graph2)

-- | Checks that adjacencyList is the inverse of adjacencyMatrix,
-- up to isomorphism.
pAdjListAdjMatrixInverse :: Graph -> Property
pAdjListAdjMatrixInverse graph =
  let adjMatrix = adjacencyMatrix graph
      adjList' = adjacencyList adjMatrix
      showResults = do
        putStrLn $ "Initial graph: " ++ show graph
        putStrLn $ "Adjacency matrix: " ++ show adjMatrix
        putStrLn $ "New graph: " ++ show adjList' in
  whenFail showResults $ adjList' `graphEq` graph

-- | Checks that adjacencyMatrix is the inverse of adjacencyList.
pAdjMatrixAdjListInverse :: Array (Vertex,Vertex) Bool -> Bool
pAdjMatrixAdjListInverse adjMatrix =
  adjacencyMatrix (adjacencyList adjMatrix) == adjMatrix

-- | Checks that the cycle detection algorithm returns false
-- on directed ayclic graphs.
pHasACycleFalseOnDAG :: DAG -> Bool
pHasACycleFalseOnDAG (DAG graph) =
  hasACycle graph == False
