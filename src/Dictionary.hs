module Dictionary where

import qualified Data.Map.Strict as Map

type Frequency = Integer -- Integer vs. Int?
type LogFrequency = Double
type Term = String

-- POS Tags as defined by ICTCLAS
data PosTag
  = N | F | S | T
  | NR | NS | NT | NW
  | NZ | V | VD | VN
  | A | AD | AN | D
  | M | Q | R | P
  | C | U | XC | W
  | PER | LOC | ORG | TIME
  | Untagged

instance Show PosTag where
  show posTag = case posTag of
    N -> "n 普通名詞"
    F -> "f 方位名詞"
    S -> "s 處所名詞"
    T -> "t 時間"
    NR -> "nr 人名"
    NS -> "ns 地名"
    NT -> "nt 機構名"
    NW -> "nw 作品名"
    NZ -> "nz 其他專名"
    V -> "v 普通動詞"
    VD -> "vd 動副詞"
    VN -> "vn 名動詞"
    A -> "a 形容詞"
    AD -> "ad 副形詞"
    AN -> "an 名形詞"
    D -> "d 副詞"
    M -> "m 數量詞"
    Q -> "q 量詞"
    R -> "r 代詞"
    P -> "p 介詞"
    C -> "c 連詞"
    U -> "u 助詞"
    XC -> "xc 其他虛詞"
    W -> "w 標點符號"
    PER -> "PER 人名"
    LOC -> "LOC 地名"
    ORG -> "ORG 機構名"
    TIME -> "TIME 時間"
    Untagged -> "無標籤"

data Entry = Entry { freq :: Frequency, pos :: PosTag } deriving (Show)
type DictEntryPair = (Term, Entry)
data Dict = Dict
    { dictMap :: Map.Map Term Entry
    , totalFrequency :: Frequency
    , logTotalFrequency :: LogFrequency
--    , longestTermLength :: Int
    } deriving (Show)

-- TODO: Error detection in parsers? Refactor into a parser module?
parsePOS :: String -> PosTag
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
      words2entry xs = (term, Entry frequency posTag)
        where
          term = head xs
          frequency = read (xs !! 1) :: Frequency
          posTag = parsePOS (xs !! 2)

dictFromEntryPairs :: [DictEntryPair] -> Dict
dictFromEntryPairs entryPairs = Dict dM totalF totalLF
    where
      dM = Map.fromList entryPairs
      totalF = sum [ f | (_, Entry f _) <- entryPairs]
      totalLF = (log . fromIntegral) totalF

dictFromContents :: String -> Dict
dictFromContents = dictFromEntryPairs . entryPairsFromContents

termFreq :: Dict -> Term -> Maybe Frequency
termFreq dict t = freq <$> Map.lookup t (dictMap dict)

termPOS :: Dict -> Term -> Maybe PosTag
termPOS dict t = pos <$> Map.lookup t (dictMap dict)

-- TF-IDF dictionary
-- TODO: Move to generic Dict type, with backing Trie
-- data IdfDict = IdfDict { idfDict :: Map.Map Term Frequency }
