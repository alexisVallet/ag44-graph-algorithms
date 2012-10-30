module Bench.Graphs (
  randomGraphOfSize
  ) where

import GraphUtils

import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import System.Random
import Data.Array.ST
import Data.Array
import GHC.Arr

-- | Shuffles a list randomly. Taken from http://www.haskell.org/haskellwiki/Random_shuffle .
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

-- | Generates a random graph with a given number of edges and
-- vertices. For benchmarking purposes, be careful to always
-- use the same starting seed for the random number generator for
-- repeatable results.
randomGraphOfSize :: (RandomGen g) => Int -> Int -> Rand g Graph
randomGraphOfSize numberOfVertices numberOfEdges = do
  let vertices = [0..numberOfVertices-1]
  edges <- fmap (take numberOfEdges) 
           $ shuffle [(i,j) | i <- vertices, j <- vertices]
  return
    $ adjacencyList
    $ array
    ((0,0),(numberOfVertices-1,numberOfVertices-1))
    [((i,j),(i,j) `elem` edges) | i <- vertices, j <- vertices]
