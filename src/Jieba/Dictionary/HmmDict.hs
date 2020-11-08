module Jieba.Dictionary.HmmDict where

import Jieba.Types.HmmState as HS
import Jieba.Types.Units (Probability, LogProbability)

import qualified Data.Map.Strict as Map

type EmissionProbs = Map.Map Char LogProbability

-- lists of length 4, 1 for each state
data HmmDict = HmmDict { init_prob :: StateProbRow
                       , state_trans :: [StateProbRow]
                       , emit_prob :: [EmissionProbs]
                       } deriving (Show)

initProb :: HmmDict -> HmmState -> Probability
initProb d st = (HS.stateProb st) $ init_prob d

stateTransProb :: HmmDict -> Transition -> Probability
stateTransProb d (Transition a b) = stateProb a (t !! HS.index b)
  where t = state_trans d

emitProb :: HmmDict -> Emission -> Maybe Probability
emitProb d (Emission ch st) = Map.lookup ch dm
  where dm = (emit_prob d) !! (HS.index st)
