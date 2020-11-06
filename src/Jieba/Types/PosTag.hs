{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
-- Ensure we haven't left out any PosTags

module Jieba.Types.PosTag where

-- POS Tags as defined by ICTCLAS
data PosTagNamed
  = N | F | S | T
  | NR | NS | NT | NW
  | NZ | V | VD | VN
  | A | AD | AN | D
  | M | Q | R | P
  | C | U | XC | W
  | PER | LOC | ORG | TIME
  | Untagged -- TODO: Deprecate, keeping to suppress GHC
  deriving (Eq, Ord, Enum, Bounded)

data PosTag = Known PosTagNamed | Unknown String

instance Show PosTag where
  show posTag = case posTag of
    Known tag -> show tag
    Unknown str -> str ++ " 未知"

instance Show PosTagNamed where
  show posTagNamed = case posTagNamed of
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
    Untagged -> "Untagged" -- TODO: Deprecate

parsePOSNamed :: String -> PosTagNamed
parsePOSNamed posTag = case posTag of
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
  _ -> undefined
