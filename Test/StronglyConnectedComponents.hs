{-|
Unit test module for the strongly connected components determination
algorithm. Verifies that the algorithm returns the same result as
the algorithm implemented in the standard Data.Graph module from
the containers library.
-}
module Test.StronglyConnectedComponents (testSCC) where

import StronglyConnectedComponents
import GraphIO
import Test.ArbitraryInstances

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Graph
import Data.Tree
import Data.List
import Control.Concurrent

testSCC :: Test
testSCC = testProperty "Equality with Data.Graph.scc" pTarjanEqualToScc

-- | Checks that the algorithm returns the same strongly connected
-- components as Data.Graph.scc .
pTarjanEqualToScc :: Graph -> Property
pTarjanEqualToScc graph =
  let actual = sort $ map sort $ tarjan graph
      expected = sort $ map sort $ map flatten $ scc graph
      showResults = do
        forkIO $ showGraphAndSCC graph expected
        forkIO $ showGraphAndSCC graph actual
        return () in
  whenFail showResults $ actual == expected
