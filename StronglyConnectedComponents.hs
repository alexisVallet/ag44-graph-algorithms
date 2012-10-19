module StronglyConnectedComponents
       (
         fromFile,
         tarjan
       ) where

import Data.Graph (
  Graph(..),
  vertices,
  edges,
  Vertex(..), 
  Edge(..), 
  Table(..), 
  Bounds(..))
import Data.IntMap as IntMap hiding ((!))
import Prelude as Prelude hiding ((.))
import Control.Category
import System.IO as IO
import Data.ByteString as ByteString
import Data.Array as Array hiding (index)
import Data.Char (ord)
import Data.Word
import Control.Monad.ST
import Control.Monad.RWS.Strict
import Control.Lens
import Control.Monad.Primitive
import Control.Monad
import Control.Applicative
import Data.Maybe

fromFile :: FilePath -> IO Graph
fromFile filePath = do
  rawData <- ByteString.readFile filePath
  return $ rawDataToGraph rawData

toWord :: Char -> Word8
toWord = fromIntegral . ord

rawDataToGraph :: ByteString -> Graph
rawDataToGraph rawData =
  let lines = ByteString.split (toWord '\n') rawData
      adjacencyLists = Prelude.map lineToAdjList lines
      numberOfVertices = Prelude.length lines in
  listArray
  (0,numberOfVertices-1) 
  adjacencyLists

lineToAdjList :: ByteString -> [Vertex]
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

data VertexData = VertexData {
  _index :: Int,
  _lowLink :: Int,
  _isInStack :: Bool
  }
makeLenses ''VertexData

data SCCState = SCCState {
  _globalIndex :: Int,
  _verticesData :: IntMap VertexData,
  _visitedStack :: [Vertex]
  }
makeLenses ''SCCState


type SCC = RWS Graph [[Vertex]] SCCState

runSCC :: Graph -> SCC a -> [[Vertex]]
runSCC graph sccAction = do
  let (min,max) = bounds graph
      numberOfVertices = max - min + 1
      initialVertexData = IntMap.empty
      initialState = SCCState 0 initialVertexData []
  snd $ evalRWS sccAction graph initialState

tarjan :: Graph -> [[Vertex]]
tarjan graph = runSCC graph tarjanSCC

tarjanSCC :: SCC ()
tarjanSCC = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    mVertexData <- use $ verticesData . at vertex
    when (isNothing mVertexData) $ do
      strongConnect vertex

successors :: Graph -> Vertex -> [Vertex]
successors = (!)

strongConnect :: Vertex -> SCC ()
strongConnect vertex = do
  currentIndex <- use globalIndex
  let vertexDataL = verticesData . at vertex
  vertexDataL .= (Just $ VertexData currentIndex currentIndex True)
  globalIndex += 1
  visitedStack %= (vertex:)
  graph <- ask
  forM_ (successors graph vertex) $ \successor -> do
    let successorDataL = verticesData . at successor
    mSuccessorData <- use successorDataL
    case mSuccessorData of
      Nothing -> do
        strongConnect successor
        newSuccessorData <- fmap fromJust $ use successorDataL
        vertexDataL %= fmap (\vertData -> vertData {
                           _lowLink = min (_lowLink vertData) (_lowLink newSuccessorData)
                           })
      Just successorData -> do
        when (_isInStack successorData) $ do
          vertexDataL %= fmap (\vertData -> vertData {
                             _lowLink = min (_index successorData) (_lowLink vertData)
                             })
  let vertexDataL' = verticesData . at vertex
  mVertexData <- use vertexDataL'
  case mVertexData of
    Nothing -> error "The vertex data should be defined at this point in the algorithm. This shouldn't happen."
    Just vertexData -> do
      when (_lowLink vertexData == _index vertexData) $ do
        scc <- buildCurrentSCC vertex
        tell [scc]

buildCurrentSCC :: Vertex -> SCC [Vertex]
buildCurrentSCC root = do
  vertex <- pop
  if vertex == root 
    then do
      return [vertex]
    else do
      rest <- buildCurrentSCC root
      return (vertex:rest)

pop :: SCC Vertex
pop = do
  stack <- use visitedStack
  visitedStack %= Prelude.tail
  return $ Prelude.head stack

