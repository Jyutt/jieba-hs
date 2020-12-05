{-# LANGUAGE MultiWayIf #-}

module Jieba.Cut where

import Jieba.Graph as Graph
import Jieba.Types.PosTag
import Jieba.Dictionary.FreqDict as FD
import Jieba.Dictionary.HmmDict
import Control.Monad.State.Lazy
import Jieba.FinalSeg as FS

import Data.Array -- cutAll

cutNoHMM :: FreqDict -> String -> [String]
cutNoHMM dict snt = seg . followPath . calcOptimalPath dict $ dag
  where
    dag = buildDAG dict snt
    seg = segmentSentence snt

cutHMM :: FreqDict -> HmmDict -> String -> [String]
cutHMM fd hd snt = cutHMM' firstCut []
  where firstCut = cutNoHMM fd snt
        cutHMM' rem acc
          | null rem = acc
          | length buff == 0 = cutHMM' rem (acc ++ seg)
          | length buff == 1 = cutHMM' rem'' (acc ++ buff ++ seg)
          | FD.entryExists fd (concat buff) = cutHMM' rem'' (acc ++ buff ++ seg)
          | otherwise = cutHMM' rem'' (acc ++ fs ++ seg)
          where buff = takeWhile ((==1) . length) rem
                rem' = dropWhile ((==1) . length) rem
                rem'' = drop 1 rem'
                seg = take 1 rem'
                fs = FS.finalseg hd (concat buff)

cutAll :: FreqDict -> String -> [String]
cutAll dict snt = concatMap f idxs
  where
    dag = buildDAG dict snt
    (_, n) = bounds dag -- first idx is always 0
    idxs = [0..n]
    substr x y = take (y - x + 1) . drop x $ snt
    f ix = map (substr ix . vertex) (dag ! ix)
