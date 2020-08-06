module Jieba.Dictionary.FreqDict where

import Jieba.Dictionary as D
import Jieba.Dictionary.Types.PosTag
-- import qualified Data.Map.Strict as Map

type FreqDict = Dictionary Entry Metadata
type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type DictEntryPair = EntryPair Entry

data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
data Metadata = Metadata
  { totalFrequency :: Frequency
  , logTotalFrequency :: LogFrequency
  } deriving (Show)

words2entry :: [String] -> DictEntryPair
words2entry xs = (term, Entry frequency posTag)
  where
    term = head xs
    frequency = read (xs !! 1) :: Frequency
    posTag = parsePOS (xs !! 2)

entries2meta :: [DictEntryPair] -> Metadata
entries2meta entryPairs = Metadata totalF totalLF
  where
    totalF = sum $ map (freq . snd) entryPairs
    totalLF = (log . fromIntegral) totalF

dictFromContents :: String -> FreqDict
dictFromContents = dictInputParser words2entry entries2meta

lookupFreq :: FreqDict -> String -> Maybe Frequency
lookupFreq = D.lookupWith freq

lookupPOS :: FreqDict -> String -> Maybe PosTag
lookupPOS = D.lookupWith pos
