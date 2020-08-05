module Jieba.Dictionary.FreqDict where

import Jieba.Dictionary as D
import Jieba.Dictionary.Types.PosTag
import qualified Data.Map.Strict as Map

type FreqDict = Dictionary Entry Metadata
type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type DictEntryPair = (String, Entry)

data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
data Metadata = Metadata
  { totalFrequency :: Frequency
  , logTotalFrequency :: LogFrequency
  } deriving (Show)


entryPairsFromContents :: String -> [DictEntryPair]
entryPairsFromContents input = map f $ lines input
    where
      f = words2entry . words
      words2entry xs = (term, Entry frequency posTag)
        where
          term = head xs
          frequency = read (xs !! 1) :: Frequency
          posTag = parsePOS (xs !! 2)

dictFromEntryPairs :: [DictEntryPair] -> FreqDict
dictFromEntryPairs entryPairs = Dictionary dM md
    where
      dM = Map.fromList entryPairs
      totalF = sum [ f | (_, Entry f _) <- entryPairs]
      totalLF = (log . fromIntegral) totalF
      md = Metadata totalF totalLF

dictFromContents :: String -> FreqDict
dictFromContents = dictFromEntryPairs . entryPairsFromContents

lookupFreq :: FreqDict -> String -> Maybe Frequency
lookupFreq = D.lookupWith freq

lookupPOS :: FreqDict -> String -> Maybe PosTag
lookupPOS = D.lookupWith pos
