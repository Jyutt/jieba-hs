{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
-- Ensure we haven't left out any PosTags

module Jieba.Types.PosTag where

-- POS Tags as defined by ICTCLAS
data PosTagKnown
  = N | F | S | T
  | NR | NS | NT | NW
  | NZ | V | VD | VN
  | A | AD | AN | D
  | M | Q | R | P
  | C | U | XC | W
  | PER | LOC | ORG | TIME
  deriving (Eq, Ord, Enum, Bounded)

data PosTag = Known PosTagKnown | Unknown String

instance Show PosTag where
  show posTag = case posTag of
    Known tag -> show tag
    Unknown str -> str ++ " 未知"

instance Show PosTagKnown where
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

posToString :: PosTagKnown-> String
posToString pos = case pos of
  N -> "n"
  F -> "f"
  S -> "s"
  T -> "t"
  NR -> "nr"
  NS -> "ns"
  NT -> "nt"
  NW -> "nw"
  NZ -> "nz"
  V ->  "v"
  VD -> "vd"
  VN -> "vn"
  A ->  "a"
  AD -> "ad"
  AN -> "an"
  D ->  "d"
  M ->  "m"
  Q ->  "q"
  R ->  "r"
  P ->  "p"
  C ->  "c"
  U ->  "u"
  XC -> "xc"
  W ->  "w"
  PER -> "PER"
  LOC -> "LOC"
  ORG -> "ORG"
  TIME ->"TIME"
