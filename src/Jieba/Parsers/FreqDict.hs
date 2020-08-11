module Jieba.Parsers.FreqDict where

import Jieba.Parsers.Dictionary (dictInputParser)
import Jieba.Dictionary.Types.PosTag (parsePOS)
import qualified Jieba.Dictionary.FreqDict as FD

words2entry :: [String] -> FD.DictEntryPair
words2entry xs = (term, FD.Entry frequency posTag)
  where
    term = head xs
    frequency = read (xs !! 1) :: FD.Frequency
    posTag = parsePOS (xs !! 2)

entries2meta :: [FD.DictEntryPair] -> FD.Metadata
entries2meta entryPairs = FD.Metadata totalF totalLF
  where
    totalF = sum $ map (FD.freq . snd) entryPairs
    totalLF = (log . fromIntegral) totalF

dictFromContents :: String -> FD.FreqDict
dictFromContents = dictInputParser words2entry entries2meta
