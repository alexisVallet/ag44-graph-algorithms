{-|
Unit tests for the graph condensation algorithm. Tests the algorithm
against randomly generated graphs, verifying the following properties
hold true:

* The condensation must have as many vertices as the original graph
has strongly connected components.

* The condensation must be acyclic.

-}
module Test.Condensation (testCondensation) where

import Condensation
import StronglyConnectedComponents
import GraphUtils

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.ArbitraryInstances

-- | Test group for the Condensation module regrouping the other
-- properties.
testCondensation :: Test
testCondensation = 
  testGroup "Condensation.condensation" 
  [testProperty "Condensation has the right size" pCondensationSize,
   testProperty "Condensation returns a DAG" pCondensationIsDAG]

-- | Property holding true iff the condensation of a graph G
-- has exactly as many vertices as G has strongly connected
-- components.
pCondensationSize :: Graph -> Bool
pCondensationSize graph =
  let sccs = tarjan graph      
      graphCondensation = condensation graph in
  length sccs == length (vertices graphCondensation)

-- | Property holding true iff the condensation of a graph is
-- acyclic. Uses a cycle detection algorithm itself tested
-- in the 'Test.GraphUtils' module.
pCondensationIsDAG :: Graph -> Property
pCondensationIsDAG graph =
  let graphCondensation = condensation graph
      showResults = do
        putStrLn $ "Graph condensation: " ++ show graphCondensation in
  whenFail showResults $ not $ hasACycle graphCondensation
