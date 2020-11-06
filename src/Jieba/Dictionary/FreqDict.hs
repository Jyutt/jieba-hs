module Jieba.Dictionary.FreqDict where

import Jieba.Dictionary as D
import qualified Jieba.Types.Precisions as P
import Jieba.Types.PosTag

type FreqDict = Dictionary Entry Metadata
type Frequency = P.Frequency
type LogFrequency = P.LogFrequency
type DictEntryPair = EntryPair Entry

data Entry = Entry { freq :: Frequency, pos :: PosTagNamed } deriving (Show)
data Metadata = Metadata
  { totalFrequency :: Frequency
  , logTotalFrequency :: LogFrequency
  } deriving (Show)

lookupFreq :: FreqDict -> String -> Maybe Frequency
lookupFreq = D.lookupWith freq

lookupPOS :: FreqDict -> String -> Maybe PosTagNamed
lookupPOS = D.lookupWith pos
