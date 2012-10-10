module Test.Graph (testGraph) where

import Distribution.TestSuite
import Test.QuickCheck

import Graph

testGraph :: IO ()
testGraph = do
  testSimpleGraphAM
  return ()

testSimpleGraphAM :: IO ()
testSimpleGraphAM = do
  return ()