module Jieba.Dictionary.FreqDict where

import Jieba.Dictionary as D
import Jieba.Dictionary.Types.PosTag

type FreqDict = Dictionary Entry Metadata
type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type DictEntryPair = EntryPair Entry

data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
data Metadata = Metadata
  { totalFrequency :: Frequency
  , logTotalFrequency :: LogFrequency
  } deriving (Show)

lookupFreq :: FreqDict -> String -> Maybe Frequency
lookupFreq = D.lookupWith freq

lookupPOS :: FreqDict -> String -> Maybe PosTag
lookupPOS = D.lookupWith pos
