module Dictionary where

import qualified Data.Map.Strict as Map

type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type Term = String
data PosTag = VERB | ADJ | OTHER deriving (Show)
data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
data Dict = Dict
    { dictMap :: Map.Map Term Entry
    , totalFrequency :: Frequency
    , logTotalFrequency :: LogFrequency
--    , longestTermLength :: Int
    } deriving (Show)

entryPairsFromContents :: String -> [(Term, Entry)]
entryPairsFromContents input = map f $ lines input
    where
      asTuple x = ((x !! 0), Entry (read (x !! 1) :: Frequency) OTHER)
      f = asTuple . words

dictFromEntryPairs :: [(Term, Entry)] -> Dict
dictFromEntryPairs entryPairs = Dict dM totalF totalLF
    where
      dM = Map.fromList entryPairs
      totalF = sum [ f | (_, Entry f _) <- entryPairs]
      totalLF = (log . fromIntegral) totalF

dictFromContents :: String -> Dict
dictFromContents = dictFromEntryPairs . entryPairsFromContents

termFreq :: Term -> Dict -> Maybe Frequency
termFreq t dict = freq <$> Map.lookup t (dictMap dict)
