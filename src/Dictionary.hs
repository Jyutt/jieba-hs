module Dictionary where

import qualified Data.Map.Strict as Map

type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type Term = String

-- POS Tags as defined by ICTCLAS
data POSTag
  = N | F | S | T
  | NR | NS | NT | NW
  | NZ | V | VD | VN
  | A | AD | AN | D
  | M | Q | R | P
  | C | U | XC | W
  | PER | LOC | ORG | TIME
  | Untagged deriving (Show)

data Entry = Entry { freq :: Frequency, pos :: POSTag } deriving (Show)
type DictEntryPair = (Term, Entry)
data Dict = Dict
    { dictMap :: Map.Map Term Entry
    , totalFrequency :: Frequency
    , logTotalFrequency :: LogFrequency
--    , longestTermLength :: Int
    } deriving (Show)

-- TODO: Error detection in parsers? Refactor into a parser module?
parsePOS :: String -> POSTag
parsePOS posTag = case posTag of
  "n" -> N
  "f" -> F
  "s" -> S
  "t" -> T
  "nr" -> NR
  "ns" -> NS
  "nt" -> NT
  "nw" -> NW
  "nz" -> NZ
  "v"  -> V
  "vd" -> VD
  "vn" -> VN
  "a"  -> A
  "ad" -> AD
  "an" -> AN
  "d"  -> D
  "m"  -> M
  "q"  -> Q
  "r"  -> R
  "p"  -> P
  "c"  -> C
  "u"  -> U
  "xc" -> XC
  "w" -> W
  "PER" -> PER
  "LOC" -> LOC
  "ORG" -> ORG
  "TIME" -> TIME
  _ -> Untagged

entryPairsFromContents :: String -> [DictEntryPair]
entryPairsFromContents input = map f $ lines input
    where
      f = words2entry . words
      words2entry xs = (term, Entry freq posTag)
        where
          term = xs !! 0
          freq = read (xs !! 1) :: Frequency
          posTag = parsePOS (xs !! 2)

dictFromEntryPairs :: [DictEntryPair] -> Dict
dictFromEntryPairs entryPairs = Dict dM totalF totalLF
    where
      dM = Map.fromList entryPairs
      totalF = sum [ f | (_, Entry f _) <- entryPairs]
      totalLF = (log . fromIntegral) totalF

dictFromContents :: String -> Dict
dictFromContents = dictFromEntryPairs . entryPairsFromContents

termFreq :: Term -> Dict -> Maybe Frequency
termFreq t dict = freq <$> Map.lookup t (dictMap dict)

-- TF-IDF dictionary
data IdfDict = IdfDict { idfDict :: Map.Map Term Frequency }
