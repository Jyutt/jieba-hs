module Jieba.Dictionary.FreqDict where

-- import Jieba.Dictionary as D
import Jieba.Types.PosTag
import Jieba.Types.Units (Frequency, LogFrequency)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

data FreqDict = FreqDict { dictMap :: Map.Map String Entry
                         , metadata :: Metadata
                         } deriving (Show)
data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
data Metadata = Metadata
  { totalFrequency :: Frequency
  , logTotalFrequency :: LogFrequency
  } deriving (Show)

entriesToDict :: [(String, Entry)] -> FreqDict
entriesToDict xs = FreqDict dm m
  where dm = Map.fromList xs
        tf = sum $ map (freq . snd) xs
        logTf = (log . fromIntegral)  tf
        m = Metadata tf logTf

entryExists :: FreqDict -> String -> Bool
entryExists d k = isJust . Map.lookup k $ dictMap d

lookupFreq :: FreqDict -> String -> Maybe Frequency
lookupFreq d k = freq <$> Map.lookup k (dictMap d)

lookupPOS :: FreqDict -> String -> Maybe PosTag
lookupPOS d k = pos <$> Map.lookup k (dictMap d)
