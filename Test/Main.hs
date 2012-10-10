module Main where

import Distribution.TestSuite
import Test.QuickCheck
import Test.Graph

main :: IO ()
main = do
  testGraph
  return ()
