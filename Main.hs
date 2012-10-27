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

import System.Console.CmdArgs

-- | Data type specifying command line parameters.
data Graph_algorithms = SCCs { file :: FilePath }
                          | Condensation { file :: FilePath }
                          | LongestPath { file :: FilePath }
                            deriving (Show, Data, Typeable)

-- | Default values for command line parameters.
sccsMode = SCCs {file = def}
condensationMode = Condensation {file = def}
longestPathMode = LongestPath {file = def}

-- | Entry point of the program, parses the parameters and executes
-- the corresponding actions.
main :: IO ()
main = do
  args <- cmdArgs (modes [sccsMode, condensationMode, longestPathMode])
  graph <- fromFile $ file args
  case args of
    SCCs _ -> do
      let sccs = tarjan graph
      putStrLn $ show sccs
      showGraphAndSCC graph sccs
    Condensation _ -> do
      let graphCondensation = condensation graph
      showAdjacencyMatrix graphCondensation
      showGraph graphCondensation
    LongestPath _ -> do
      let 
        graphCondensation = condensation graph
        graphLongestPath = longestPath graphCondensation
      putStrLn $ show graphLongestPath
      showGraphAndPath graphCondensation graphLongestPath
