{-|
This module defines functions to create a graph from a file and
to display a graph and his strongly connected components using
graphviz.
-}
module GraphIO
       (
         fromFile,
         showGraph,
         showAdjacencyMatrix,
         showGraphAndSCC,
         showGraphAndPath
       ) where

import System.IO as IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BSC8
import Data.Array as Array hiding (index)
import Data.Char (ord)
import Data.Word
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List
import Data.Maybe
import Control.Monad

import GraphUtils

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

-- | Parse lines out of a bytestring text.
parseLines :: ByteString.ByteString -> [ByteString.ByteString]
parseLines text =
  let slashn = toWord '\n'
      slashr = toWord '\r' in
  filter (not . ByteString.null)
  $ ByteString.splitWith 
  (\word -> word == slashn || word == slashr)
  text

-- | Parses the raw bytestring data from the adjacency matrix file
-- into a graph.
rawDataToGraph :: ByteString.ByteString -> Graph
rawDataToGraph rawData =
  let (first:lines) = parseLines rawData
      numberOfVertices = read $ BSC8.unpack first :: Int
      adjacencyLists = Prelude.map lineToAdjList lines in
  listArray
  (0,numberOfVertices-1) 
  adjacencyLists

-- | Converts a line of the adjacency matrix file into
-- an adjacency list for the corresponding vertex.
lineToAdjList :: ByteString.ByteString -> [Vertex]
lineToAdjList line =
  snd 
  $ ByteString.foldl'
  (\(index,vertices) word -> 
    let isZero = word == toWord '0'
        isOne = word == toWord '1'
        newIndex = if isZero || isOne then index + 1 else index
        newVertices = if isOne then index:vertices else vertices in
    (newIndex, newVertices))
  (0,[])
  line

-- | Converts a graph to a graph representation suitable for
-- graphviz.
graphToDot' :: Graph -> DotGraph Vertex
graphToDot' graph = do
  let graphVizParams = Params {
        isDirected = True,
        globalAttributes = [],
        clusterBy = \(node,scc) -> N (node,node),
        clusterID = id,
        fmtCluster = const [],
        fmtNode = const [],
        fmtEdge = const []
        }
      nodeList =
         map (\node -> (node, Int node)) 
         $ vertices graph
      edgeList = map (\(i,j) -> (i,j,())) $ edges graph in
   graphElemsToDot graphVizParams nodeList edgeList

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

-- | Checks whether graphviz is available, and execute the action if
-- it is the case. Prints a message otherwise.
withGraphviz :: IO () -> IO ()
withGraphviz action = do
  graphvizInstalled <- isGraphvizInstalled
  if graphvizInstalled 
    then do
      action
    else do
      putStrLn "Cannot show the graph, please install graphviz."

-- | Converts a graph and a path in the graph into a format
-- suitable for use with Graphviz.
graphAndPathToDot :: Graph -> [Vertex] -> DotGraph Vertex
graphAndPathToDot graph path =
  let pathEdges =
        fst
        $ foldl
        (\(edges,mV2) v1 ->
          case mV2 of
            Nothing -> (edges, Just v1)
            Just v2 -> ((v2,v1):edges,Just v1))
        ([],Nothing)
        path
      graphVizParams = Params {
        isDirected = True,
        globalAttributes = [],
        clusterBy = \(node,_) -> N (node,node),
        clusterID = id,
        fmtCluster = const [],
        fmtNode = const [],
        fmtEdge = 
          \(i,j,_) -> if (i,j) `elem` pathEdges
                      then [Color $ [X11Color Red]]
                      else [Color $ [X11Color Black]]
        } 
      nodeList =
         map (\node -> (node, Int node)) 
         $ vertices graph
      edgeList = map (\(i,j) -> (i,j,())) $ edges graph in
  graphElemsToDot graphVizParams nodeList edgeList

-- | Displays a graph and its strongly connected components
-- in a new window, blocking the thread it is used in in the
-- meanwhile.
showGraphAndSCC :: Graph -> [[Vertex]] -> IO ()
showGraphAndSCC graph sccs = withGraphviz $ do
  let dotGraph = graphAndSCCToDot graph sccs
  runGraphvizCanvas' dotGraph Xlib

-- | Displays a graph in a new window, blocking the thread
-- it is used in in the meanwhile.
showGraph :: Graph -> IO ()
showGraph graph = withGraphviz $ do
  let dotGraph = graphToDot' graph
  runGraphvizCanvas' dotGraph Xlib

-- | Prints the adjacency matrix of the graph.
showAdjacencyMatrix :: Graph -> IO ()
showAdjacencyMatrix graph = do
  let adjMatrix = adjacencyMatrix graph
      vertices' = vertices graph
  forM_ vertices' $ \i -> do
    forM_ vertices' $ \j -> do
      putStr $ (if adjMatrix!(i,j) then "1" else "0") ++ " "
    putStrLn ""

-- | Displays a graph and path in the graph in a new window,
-- blocking the thread in the meanwhile.
showGraphAndPath :: Graph -> [Vertex] -> IO ()
showGraphAndPath graph path = withGraphviz $ do
    let dotGraph = graphAndPathToDot graph path
    runGraphvizCanvas' dotGraph Xlib