module Jieba.Dictionary.HmmDict where

import Jieba.Types.HmmState as HS
import Jieba.Types.Units (Probability, LogProbability)
import qualified Data.Map.Strict as Map

type EmissionProbs = Map.Map Char LogProbability

-- lists of length 4, 1 for each state
data HmmDict = HmmDict { initProb :: StateProbRow
                       , stateTrans :: [StateProbRow]
                       , emitProb :: [EmissionProbs]
                       } deriving (Show)

lookupStateTrans :: HmmDict -> Transition -> Probability
lookupStateTrans d (Transition a b) = stateProb a (t !! HS.idx b)
  where t = stateTrans d

lookupEmit :: HmmDict -> Emission -> Maybe Probability
lookupEmit d (Emission ch st) = Map.lookup ch dm
  where dm = (emitProb d) !! (HS.idx st)
