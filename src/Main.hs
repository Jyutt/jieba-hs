{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment (getArgs)
--import Jieba.Parsers.FreqDict (dictFromContents)
import Jieba
import Jieba.Parsec.FreqDict
import Jieba.Parsec.HmmWeights
import Jieba.FinalSeg
import Jieba.Types.HmmState
import Data.List (intercalate)

import Jieba.Graph.STGraph as STG
import qualified Data.Text as T

-- For benchmarking purposes
main :: IO ()
main = do
  dict <- readFreqDict "data/dict.txt.small"
  let snt = "因为你所以我所以我不退缩"
      dag = STG.buildDAG dict snt
      edges = STG.calcDP dict dag
      path = STG.calcPath edges
      segmentLengths = STG.calcSegmentLengths edges
      segmentedSnt = map T.unpack $ STG.segmentSentence snt segmentLengths

  putStrLn "Segmentations: "
  putStrLn . show $ path
  putStrLn . show $ segmentLengths
  mapM_ putStrLn segmentedSnt

    -- hmmd <- readHmmDict "data/hmm.model"
    -- args <- getArgs
    -- if null args then error "Expected filepath, no arguments found" else return ()

    -- -- Open contents
    -- handle <- openFile (args !! 0) ReadMode
    -- sentences <- lines <$> hGetContents handle

    -- seq sentences (putStrLn ("Opening file: " ++ args !! 0))
    -- mapM_ putStrLn sentences

    -- -- Segment each sentences and pretty-print
    -- -- Serious performance problems with cutHMM
    -- let cut = cutNoHMM dict
    --   in mapM_ (putStrLn . intercalate "/" . cut) sentences
