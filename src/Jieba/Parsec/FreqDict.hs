{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
-- Ensure we haven't left out any PosTags

module Jieba.Parsec.FreqDict where

import Jieba.Types.PosTag
import Jieba.Types.Units (Frequency)
import qualified Jieba.Dictionary.FreqDict as FD

import Text.Parsec
import qualified Data.Map.Strict as Map
import System.IO
import Data.Either (partitionEithers)

type FreqEntry = (String, FD.Entry)
type Parsec' = Parsec String ()
type ParsecFreqEntry = Parsec' FreqEntry

parsePosNamed :: PosTagKnown -> Parsec' PosTagKnown
parsePosNamed pos = try (string (posToString pos) >> eof) >> return pos

parseAnyPosNamed :: Parsec' PosTagKnown
parseAnyPosNamed = foldl1 (<|>) $ fmap parsePosNamed [minBound..maxBound]

parsePos :: Parsec' PosTag
parsePos = (Known <$> parseAnyPosNamed) <|> (Unknown <$> many1 letter)

parseFreqEntry :: Parsec' FreqEntry
parseFreqEntry = do
  word <- many1 letter
  skipMany1 space
  freq <- read <$> many1 digit
  skipMany1 space
  pos <- parsePos
  return $ (word, FD.Entry freq pos)

readFreqDictEntries :: String -> IO [FreqEntry]
readFreqDictEntries filePath = do
  contents <- hGetContents =<< openFile filePath ReadMode
  let entries = map (runParser parseFreqEntry () "") (lines contents)
  let (lefts, rights) = partitionEithers entries
  mapM_ ((hPutStrLn stderr) . show) lefts
  return rights

readFreqDict :: String -> IO FD.FreqDict
readFreqDict p = FD.entriesToDict <$> readFreqDictEntries p
