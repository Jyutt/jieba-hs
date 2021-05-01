module Jieba.Graph.STGraph where

import Jieba.Dictionary.FreqDict as FD
import Jieba.Types.Units (Weight)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Text as T ( Text, drop, length, take, unpack, splitAt)
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.List as List
import Data.Ord

type Vertex = Int
type Sentence = Text

-- Adjacency List Representation
type SentenceDAG = V.Vector (V.Vector Edge)

-- Edge (Vertex i -> Vertex j+1) represents snt[i:j], index j non-inclusive
data Edge = Edge { vertex :: Vertex , weight :: Weight } deriving (Show)

buildDAG :: FreqDict -> Sentence -> SentenceDAG
buildDAG dict snt = V.create $ do
    vec <- VM.unsafeNew m
    forM_ [0..m-1] $ \i -> do
        let sfx = sfxs V.! i
            -- Enumerate substrs
            pfxs = V.imap (,) . prefixes $ sfx

            edges :: V.Vector Edge
            edges = V.catMaybes $
                V.map (\(j, pfx) -> calcEdge (i+j+1) <$> lookup pfx) pfxs

        VM.unsafeWrite vec i edges

    return vec
    where
        m = T.length snt
        sfxs = suffixes snt
        lookup = lookupFreq dict . T.unpack
        calcEdge v = Edge v . (log . fromIntegral)

calcDP :: FreqDict -> SentenceDAG -> V.Vector Edge
calcDP fd dag = V.create $ do
    dp <- VM.unsafeNew (m+1)
    -- DP base case, the end node of the DAG
    VM.unsafeWrite dp m (Edge m 0)

    forM_ [m-1, m-2..0] $ \i -> do
        let paths = dag V.! i

        dp' <- V.unsafeFreeze dp
        let costs :: V.Vector Edge
            costs = if V.null paths
                -- weights are log probabilities, log(1) = 0
                then V.singleton $ Edge (i+1) 0
                else (\(Edge v w) -> Edge v (w + weight (dp' V.! v))) <$> paths
        V.unsafeThaw dp'

        let optimalPath :: Edge
            optimalPath = maximumBy (comparing weight) . V.toList $ costs
        
        VM.unsafeWrite dp i (normalize optimalPath)
    
    -- Trim the base case node, which does not actually correspond to
    -- any section of the sentence
    return $ VM.slice 0 m dp
    where
        m = V.length dag
        ltf = logTotalFrequency . metadata $ fd
        normalize (Edge v w) = Edge v (w - ltf)

-- TODO: Verify that this is O(n) and not O(n^2)
-- Returns list of indicies to splice the sentence at
-- [a, b, c, d] -> snt[a:b], snt[b:c], snt[c:d]
calcPath :: V.Vector Edge -> [Vertex]
calcPath edges = f 0 []
    where
        next ix = vertex (edges V.! ix)
        k = V.length edges
        f ix xs
            | ix < k = f (next ix) (xs ++ [ix])
            | otherwise = xs ++ [ix]

calcSegmentLengths :: V.Vector Edge -> [Int]
calcSegmentLengths edges = zipWith (-) (List.drop 1 idxs) idxs
    where idxs = calcPath edges

-- Segemnt sentence based on lengths of slices
segmentSentence :: Sentence -> [Int] -> [Text]
segmentSentence snt = snd . foldl f (snt, [])
    where
        f (snt, xs) sliceLength =
            let (slice, remainder) = T.splitAt sliceLength snt
            in (remainder, xs ++ [slice])

suffixes :: Sentence -> V.Vector Sentence
suffixes snt = V.iterateN (T.length snt) (T.drop 1) snt

prefixes :: Sentence -> V.Vector Sentence
prefixes snt = V.create $ do
    vec <- VM.unsafeNew m
    forM_ [0..m-1] $ \i -> VM.unsafeWrite vec i (T.take (i+1) snt)
    return vec
    where
        m = T.length snt