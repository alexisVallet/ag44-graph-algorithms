{-|
This module defines functions to create a graph from a file and
to display a graph and his strongly connected components using
graphviz.
-}
module GraphIO 
       (
         fromFile,
         showGraphAndSCC
       ) where

import System.IO as IO
import qualified Data.ByteString as ByteString
import Data.Array as Array hiding (index)
import Data.Char (ord)
import Data.Word
import Data.Graph
import Data.GraphViz
import Data.List
import Data.Maybe

-- | Create a directed graph from a file, where the file is contains
-- the adjacency matrix of the graph. This function assumes the file
-- is well formed, and behaves in undefined ways otherwise.
fromFile :: FilePath -> IO Graph
fromFile filePath = do
  rawData <- ByteString.readFile filePath
  return $ rawDataToGraph rawData

-- | Utitlity function to work with bytestrings.
toWord :: Char -> Word8
toWord = fromIntegral . ord

-- | Parses the raw bytestring data from the adjacency matrix file
-- into a graph.
rawDataToGraph :: ByteString.ByteString -> Graph
rawDataToGraph rawData =
  let lines = ByteString.split (toWord '\n') rawData
      adjacencyLists = Prelude.map lineToAdjList lines
      numberOfVertices = Prelude.length lines in
  listArray
  (0,numberOfVertices-1) 
  adjacencyLists

-- | Converts a line of the adjacency matrix file into
-- an adjacency list for the corresponding vertex.
lineToAdjList :: ByteString.ByteString -> [Vertex]
lineToAdjList line =
  snd 
  $ ByteString.foldr
  (\word (index,vertices) -> 
    let isZero = word == toWord '0'
        isOne = word == toWord '1'
        newIndex = if isZero || isOne then index + 1 else index
        newVertices = if isOne then index:vertices else vertices in
    (newIndex, newVertices))
  (0,[])
  line

-- | Converts a graph and its strongly connected components
-- into a format suitable for use with Graphviz.
graphAndSCCToDot :: Graph -> [[Vertex]] -> DotGraph Vertex
graphAndSCCToDot graph sccs =
  let graphVizParams = Params {
        isDirected = True,
        globalAttributes = [],
        clusterBy = \(node,scc) -> C scc $ N (node,node),
        clusterID = id,
        fmtCluster = const [],
        fmtNode = const [],
        fmtEdge = const []
        }
      sccContaining node = 
        case findIndex (elem node) sccs of
          Just i -> i
          Nothing -> error $ "Node " ++ show node ++ " does not belong to any SCC!"
      nodeList =
         map (\node -> (node, Int $ sccContaining node)) 
         $ vertices graph
      edgeList = map (\(i,j) -> (i,j,())) $ edges graph in
   graphElemsToDot graphVizParams nodeList edgeList

-- | Displays a graph and its strongly connected components
-- in a new window, blocking the thread it is used in in the
-- meanwhile.
showGraphAndSCC :: Graph -> [[Vertex]] -> IO ()
showGraphAndSCC graph sccs = do
  graphvizInstalled <- isGraphvizInstalled
  if graphvizInstalled 
    then do
      let dotGraph = graphAndSCCToDot graph sccs
      putStrLn $ "The dot graph is: " ++ show dotGraph
      runGraphvizCanvas' dotGraph Xlib
    else do
      putStrLn "Cannot show the graph, please install graphviz."
