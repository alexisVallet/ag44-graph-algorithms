module Test.Condensation (testCondensation) where

import Condensation
import StronglyConnectedComponents
import GraphUtils

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.ArbitraryInstances

testCondensation :: Test
testCondensation = 
  testGroup "Condensation.condensation" 
  [testProperty "Condensation has the right size" pCondensationSize,
   testProperty "Condensation returns a DAG" pCondensationIsDAG]

pCondensationSize :: Graph -> Bool
pCondensationSize graph =
  let sccs = tarjan graph      
      graphCondensation = condensation graph in
  length sccs == length (vertices graphCondensation)

pCondensationIsDAG :: Graph -> Property
pCondensationIsDAG graph =
  let graphCondensation = condensation graph
      showResults = do
        putStrLn $ "Graph condensation: " ++ show graphCondensation in
  whenFail showResults $ not $ hasACycle graphCondensation
