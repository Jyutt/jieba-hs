module Jieba where

import Jieba.Graph
import Jieba.Types.PosTag
import Jieba.Dictionary.FreqDict

import Control.Applicative (liftA2)
import Data.Array
import Data.Maybe (fromMaybe)

data CutMode = All | HMM | NoHMM

cut :: CutMode -> FreqDict -> String -> [String]
cut mode = case mode of
  All -> cutAll
  HMM -> undefined
  NoHMM -> cutNoHMM

cutNoHMM :: FreqDict -> String -> [String]
cutNoHMM dict snt = seg . followPath . path $ (dict, snt)
  where
    path = uncurry $ liftA2 (.) optimalPath buildDAG
    seg = segmentSentence snt

cutAll :: FreqDict -> String -> [String]
cutAll dict snt = concatMap f idxs
  where
    dag = buildDAG dict snt
    (_, n) = bounds dag -- first idx is always 0
    idxs = [0..n]
    substr x y = take (y - x + 1) . drop x $ snt
    f ix = map (substr ix . vertex) (dag ! ix)

tokenize :: FreqDict -> [String] -> [(String, PosTag)]
tokenize dict xs = zip xs $ map (fromMaybe Untagged . lookupPOS dict) xs
