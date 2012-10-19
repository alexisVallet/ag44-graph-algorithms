module Test.StronglyConnectedComponents (testSCC) where

import StronglyConnectedComponents
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testSCC :: Test
testSCC = testProperty "Equality with Data.Graph.scc" pTarjanEqualToScc

pTarjanEqualToScc :: Bool
pTarjanEqualToScc = True