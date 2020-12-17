module Jieba.Cut where

import qualified Jieba.Graph as G
import Jieba.Types.PosTag
import Jieba.Dictionary.FreqDict as FD
import Jieba.Dictionary.HmmDict as HD
import Data.Array ((!), bounds)
import qualified Jieba.FinalSeg as FS
import qualified Data.DList as DL

cutNoHMM :: FreqDict -> String -> [String]
cutNoHMM dict snt = seg . G.followPath . G.calcOptimalPath dict $ dag
  where
    dag = G.buildDAG dict snt
    seg = G.segmentSentence snt

cutHMM :: FreqDict -> HmmDict -> String -> [String]
cutHMM fd hd snt = DL.toList . cutHMM' firstCut $ mempty
  where firstCut = cutNoHMM fd snt
        cutHMM' rem acc
          | null rem = acc
          | length buff == 0 = cutHMM' rem (acc <> seg)
          | length buff == 1 = cutHMM' rem'' (acc <> dlBuff <> seg)
          | FD.entryExists fd buff' = cutHMM' rem'' (acc <> dlBuff <> seg)
          | otherwise = cutHMM' rem'' (acc <> fs <> seg)
          where (buff, rem') = span ((== 1) . length) rem
                buff' = concat buff
                dlBuff = DL.fromList buff
                rem'' = drop 1 rem'
                seg = DL.fromList . take 1 $ rem'
                fs = DL.fromList . FS.finalseg hd $ buff'

cutAll :: FreqDict -> String -> [String]
cutAll dict snt = concatMap f idxs
  where
    dag = G.buildDAG dict snt
    (_, n) = bounds dag -- first idx is always 0
    idxs = [0..n]
    substr x y = take (y - x + 1) . drop x $ snt
    f ix = map (substr ix . G.vertex) (dag ! ix)
