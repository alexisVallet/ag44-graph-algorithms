{-|
This module provides a command line program to compute graph
algorithms. Includes the following:

* Computing the strongly connected components in a directed
  graph.

* Computing the condensation of a directed graph.

* Computing the longest path in the condensation of a directed graph.

* Displaying the results in the command line and with Graphviz.

-}
module Main (main) where

import GraphIO
import StronglyConnectedComponents as SCC
import Condensation as Condensation
import LongestPath as LongestPath
import ShortestPath as ShortestPath

import System.Console.CmdArgs
import Control.Monad

-- | Data type specifying command line parameters.
data Graph_algorithms = SCCs { file :: FilePath, enable_graphviz :: Bool }
                          | Condensation { file :: FilePath, enable_graphviz :: Bool }
                          | LongestPath { file :: FilePath, enable_graphviz :: Bool }
                            deriving (Show, Data, Typeable)

-- | Default values for command line parameters.
sccsMode = SCCs {file = def, enable_graphviz = False }
condensationMode = Condensation {file = def, enable_graphviz = False }
longestPathMode = LongestPath {file = def, enable_graphviz = False }

-- | Entry point of the program, parses the parameters and executes
-- the corresponding actions.
main :: IO ()
main = do
  args <- cmdArgs (modes [sccsMode, condensationMode, longestPathMode])
  graph <- fromFile $ file args
  case args of
    SCCs _ graphviz -> do
      let sccs = tarjan graph
      putStrLn $ show sccs
      when graphviz $ showGraphAndSCC graph sccs
    Condensation _ graphviz -> do
      let graphCondensation = condensation graph
      showAdjacencyMatrix graphCondensation
      when graphviz $ showGraph graphCondensation
    LongestPath _ graphviz -> do
      let 
        graphCondensation = condensation graph
        graphLongestPath = longestPath graphCondensation
      putStrLn $ show graphLongestPath
      when graphviz $ showGraphAndPath graphCondensation graphLongestPath
