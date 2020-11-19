module Jieba.Types.HmmState where

import Jieba.Types.Units (Probability)
import Data.Ix

data HmmState = B | E | M | S deriving (Eq, Ord, Enum, Bounded)
-- Posterior and Prior respectively
-- P(A|B) -> Transition A B
data Transition = Transition HmmState HmmState

-- P(char|state)
data Emission = Emission Char HmmState
data StateProbRow = StateProbRow { b_prob :: Probability
                                 , e_prob :: Probability
                                 , m_prob :: Probability
                                 , s_prob :: Probability
                                 } deriving Show

instance Show HmmState where
  show s = case s of
    B -> "Beginning"
    E -> "End"
    M -> "Middle"
    S -> "Single"

stateProb :: HmmState -> StateProbRow -> Probability
stateProb hs = case hs of
  B -> b_prob
  E -> e_prob
  M -> m_prob
  S -> s_prob

idx :: HmmState -> Int
idx st = case st of
  B -> 0
  E -> 1
  M -> 2
  S -> 3
