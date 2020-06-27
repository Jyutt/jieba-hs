module Graph where

import Data.Array
import Data.List (maximumBy)
import Dictionary
import Data.Maybe

type Sentence = String
type Vertex = Int
type Weight = LogFrequency

-- For use in AdjacencyList where source vertex is implicit
data Edge = Edge Vertex Weight deriving (Show)

type AdjacencyList = [Edge]
type SentenceDAG = Array Vertex AdjacencyList

buildDAG :: Sentence -> Dict -> SentenceDAG
buildDAG str dict = sntDAG
  where
    adj = map (suffixAdjList dict) (suffixes str)
    adjIndexed = zip [0..] adj
    sntDAG = array (0, length adjIndexed - 1) adjIndexed 

-- 1. 从句子建無環圖
-- 2. 採用動態規劃查找最大概率路徑
optimalPath :: SentenceDAG -> Dict -> Array Vertex (LogFrequency, Vertex)
optimalPath sntDAG dict = costArray
  where
    (_, n) = bounds sntDAG
    logTF = logTotalFrequency dict
    normalize (freq, v) = (freq - logTF, v)
    costArray = array (0, n+1) $ (n+1, (0,0)):[(v, optimalPathFrom v) | v <- [n,n-1..0]]
    optimalPathFrom v =
      (normalize . maximum) [(w + remainder v',v')| (Edge v' w) <- sntDAG ! v]
        where
          remainder v' = fst $ costArray ! (v' + 1)

-- |Returns list of suffixes with corresponding indices of the first character
suffixes :: String -> [(Vertex, String)]
suffixes str = [(n, drop n str) | n <- [0..length str - 1]]

-- |Returns list of prefixes with corresponding indicies of the last character
-- offset by the input index. Meant to be chained with output of 'suffixes'.
prefixes :: (Vertex, String) -> [(Vertex, String)]
prefixes (idx, str) = [(idx + n - 1, take n str) | n <- [1..length str]]

-- |Returns adjacency list for an input prefixes list and Dictionary.
-- The node from which edges are directed out of are implicit
-- based on the input prefixes list. See 'prefixes'.
findEdges :: Dict -> [(Vertex, String)] -> AdjacencyList
findEdges dict pfxs = catMaybes $ map pf2edge pfxs
  where
    pf2edge (v, pf) =
      (\freq -> Edge v ((log . fromIntegral) freq)) <$> termFreq pf dict

suffixAdjList :: Dict -> (Vertex, String) -> AdjacencyList
suffixAdjList dict = (findEdges dict) . prefixes