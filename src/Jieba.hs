module Jieba where

import Dictionary
import Graph
import Control.Monad (liftM2)
import Data.Array
import Data.Maybe (fromMaybe)

data CutMode = All | HMM | NoHMM

cut :: CutMode -> Dict -> String -> [String]
cut mode = case mode of
  All -> cut_All
  HMM -> undefined
  NoHMM -> cut_NoHMM

cut_NoHMM :: Dict -> String -> [String]
cut_NoHMM dict snt = seg . followPath . path $ (dict, snt)
  where
    path = uncurry $ liftM2 (.) optimalPath buildDAG
    seg = segmentSentence snt

cut_All :: Dict -> String -> [String]
cut_All dict snt = concat $ map f idxs
  where
    dag = buildDAG dict snt
    (_, n) = bounds dag -- first idx is always 0
    idxs = [0..n]
    substr = \x y -> take (y - x + 1) . drop x $ snt
    f = \ix -> map (substr ix) $ map vertex $ dag ! ix

tokenize :: Dict -> [String] -> [(String, POSTag)]
tokenize dict xs = zip xs $ map (fromMaybe Untagged . termPOS dict) xs
