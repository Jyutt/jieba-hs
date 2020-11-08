module Jieba.Graph where

--import Jieba.Dictionary
import Jieba.Dictionary.FreqDict as FD
import Jieba.Types.Units (Weight)
import Data.Array
import Data.Maybe

type Sentence = String
type Vertex = Int
type PathWeights = Array Vertex (Weight, Vertex)
type AdjacencyList = [Edge]
type SentenceDAG = Array Vertex AdjacencyList

-- For use in AdjacencyList where source vertex is implicit
data Edge = Edge { vertex :: Vertex , weight :: Weight } deriving (Show)

buildDAG :: FreqDict -> Sentence -> SentenceDAG
buildDAG dict snt = sntDAG
  where
    adj = map (suffixAdjList dict) (suffixes snt)
    adjIndexed = zip [0..] adj
    sntDAG = array (0, length adjIndexed - 1) adjIndexed

-- 1. 从句子建無環圖
-- 2. 採用動態規劃查找最大概率路徑
-- (Edge v 0) is for smoothing purposes in the case that no matches in dictionary
-- were found.
optimalPath :: FreqDict -> SentenceDAG -> PathWeights
optimalPath dict sntDAG = costArray
  where
    (_, n) = bounds sntDAG
    normalize (f, v) = (f - (logTotalFrequency . metadata) dict, v)
    costArray = array (0, n+1) $ (n+1, (0,-1)):[(v, optimalPathFrom v) | v <- [n,n-1..0]]
    optimalPathFrom v =
      (normalize . maximum) [(w + remainder v',v') | (Edge v' w) <- Edge v 0 : sntDAG ! v]
        where
          remainder v' = fst $ costArray ! (v' + 1)

followPath :: PathWeights -> [Vertex]
followPath path = f 0 []
  where
    f v vs
      | v' == -1  = vs
      | otherwise   = f (v' + 1) (vs ++ [v' - v + 1])
      where v' = snd $ path ! v

segmentSentence :: Sentence -> [Vertex] -> [String]
segmentSentence snt seglens = f snt seglens []
  where
    f str lens xs
      | null lens = xs
      | otherwise = f (drop len str) (drop 1 lens) (xs ++ [take len str])
      where
        len = head lens

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
findEdges :: FreqDict -> [(Vertex, String)] -> AdjacencyList
findEdges dict = mapMaybe pf2edge
  where
    pf2edge (v, pf) = Edge v . (log . fromIntegral) <$> lookupFreq dict pf

suffixAdjList :: FreqDict -> (Vertex, String) -> AdjacencyList
suffixAdjList dict = findEdges dict . prefixes
