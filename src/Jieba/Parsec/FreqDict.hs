{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
-- Ensure we haven't left out any PosTags

module Jieba.Parsec.FreqDict where

import Jieba.Types.PosTag
import Text.Parsec

data FreqEntry = FreqEntry String Int PosTag deriving (Show)
type Parsec' = Parsec String ()
type Frequency = Int
type ParsecFreqEntry = Parsec' FreqEntry

parseFreqEntry :: Parsec' FreqEntry
parseFreqEntry = do
  word <- many1 letter
  skipMany1 space
  freq <- read <$> many1 digit
  skipMany1 space
  pos <- parsePos
  return $ FreqEntry word freq pos

parsePosNamed :: PosTagNamed -> Parsec' PosTagNamed
parsePosNamed pos = try (string str >> eof) >> return pos
  where str = case pos of
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

parseAnyPosNamed :: Parsec' PosTagNamed
parseAnyPosNamed = foldl1 (<|>) $ fmap parsePosNamed [minBound..maxBound]

parsePos :: Parsec' PosTag
parsePos = (Known <$> parseAnyPosNamed) <|> (Unknown <$> many1 letter)
