module Jieba.Parsec.HmmWeights where

import Jieba.Types.HmmState
import Jieba.Types.Units (LogProbability)
import Jieba.Dictionary.HmmDict

import qualified Data.Map.Strict as Map
import Text.Parsec
import Control.Monad (liftM4, replicateM)
import Data.Either (partitionEithers)
import System.IO

newtype WeightPair = WeightPair (Char, Double)

type Parsec' = Parsec String ()

parseProb :: Parsec' LogProbability
parseProb = read <$> many1 (alphaNum <|> oneOf "+-.")

parseStateProbRow :: Parsec' StateProbRow
parseStateProbRow = liftM4 StateProbRow p p p p
  where p = skipMany space >> parseProb

directive :: String -> Parsec' ()
directive str = string str >> skipMany1 endOfLine

comments :: Parsec' ()
comments = char '#' >> manyTill anyChar endOfLine >> return ()

skippable :: Parsec' ()
skippable = skipMany $ comments <|> skipMany1 space -- space incl's newline

parseEmissionProbPair :: Parsec' (Char, LogProbability)
parseEmissionProbPair = do
  ch <- anyChar
  char ':'
  prob <- parseProb
  return (ch, prob)

parseEmissions :: HmmState -> Parsec' EmissionProbs
parseEmissions st = do
  directive $ "!emit_prob " ++ st'
  pairs <- parseEmissionProbPair `sepBy` (char ',')
  endOfLine
  return $ Map.fromList pairs
  where st' = case st of
          B -> "B"
          E -> "E"
          M -> "M"
          S -> "S"

parseHmmDict :: Parsec' HmmDict
parseHmmDict = do
  -- Initial State Probabilities
  skippable
  directive "!init_prob"
  init <- parseStateProbRow
  -- State Transitions
  skippable
  directive "!state_trans"
  trans <- replicateM 4 (skippable >> parseStateProbRow)
  -- Each of the Emission Probabilities
  emits <- mapM (\x -> skippable >> parseEmissions x) [B, E, M, S]
  return $ HmmDict init trans emits

readHmmDict :: String -> IO HmmDict
readHmmDict filePath = do
  contents <- hGetContents =<< openFile filePath ReadMode
  let res = runParser parseHmmDict () "" contents
  return $ case res of
    Left e -> error $ "HMM weights not in the correct format, file: " ++ filePath
      ++ "\n Parser Error:\n" ++ show e
    Right hd -> hd
