module Jieba.FinalSeg where

import Jieba.Dictionary.HmmDict
import Jieba.Types.Units (LogProbability)
import Jieba.Types.HmmState as HS
import Data.Array
import Data.Maybe (fromMaybe)
import Data.List (foldl', foldl1')
import Control.Monad.State.Lazy

-- TODO: Major ergonomics of the [B,E,M,S] transition arrays
-- perhaps define a new type with an indexing operator
-- for ex, b:e:m:s:[] is super ugly

minFloat :: LogProbability
minFloat = -3.14e100

fm :: Maybe LogProbability -> LogProbability
fm = fromMaybe minFloat

viterbi :: HmmDict -> String -> [HmmState]
viterbi hd = backward . forward hd

segment :: String -> [HmmState] -> [String]
segment str = fst' . foldl f ([], [], str)
  where f (acc, c, x:[]) _ = (acc ++ [c ++ [x]], [], "")
        f (acc, c, x:xs) s = case s of
                               B -> (acc, c ++ [x], xs)
                               M -> (acc, c ++ [x], xs)
                               E -> (acc ++ [c ++ [x]], [], xs)
                               S -> (acc ++ [c ++ [x]], [], xs)
        fst' (a, _, _) = a

finalseg :: HmmDict -> String -> [String]
finalseg hd str = segment str states
  where states = viterbi hd str

forward :: HmmDict -> String -> [[(LogProbability, HmmState)]]
forward hd (x:xs) = foldl' f [a] xs
  where a = zip (initial hd x) [B,E,M,S]
        f v ch = let lp = map fst $ head v
                     k = transProbAll hd ch lp
                 in k:v

backward :: [[(LogProbability, HmmState)]] -> [HmmState]
backward (x:xs) = drop 1 $ foldl' f [a, start] xs
  where b:e:m:s:[] = x
        start = snd . maximum . zip [e,s] $ [E,S]
        a     = snd . maximum $ [e,s]
        f s c = let prev = head s
                    nxt = snd $ c !! fromEnum prev
                 in nxt:s

initial :: HmmDict -> Char -> [LogProbability]
initial hd ch = zipWith (+) fstTrans [b,e,m,s]
  where StateProbRow b e m s = initProb hd
        em st = fm $ lookupEmit hd (Emission ch st)
        fstTrans = map em [B,E,M,S]

-- viterbi[s,t] = viterbi[s', t-1] * (trans s' -> s) * obs(ch|s)
-- e is technically constant, could be done after finding maximum instead
transProb :: HmmDict -> Char -> [LogProbability] -> HmmState -> [LogProbability]
transProb hd ch lp n = foldl1' (zipWith (+)) [lp, t, e]
  where trans p = lookupStateTrans hd $ Transition n p
        t = map trans [B,E,M,S]
        e = replicate 4 $ emiss hd ch n

-- TODO: Use a maximumBy instead, easier to read and reason about
transProbAll :: HmmDict -> Char -> [LogProbability] -> [(LogProbability, HmmState)]
transProbAll hd ch lp = map t [B,E,M,S]
  where t = max' . (transProb hd ch lp)

emiss :: HmmDict -> Char -> HmmState -> LogProbability
emiss hd ch n = fm $ lookupEmit hd $ Emission ch n

max' :: [LogProbability] -> (LogProbability, HmmState)
max' lp = maximum $ zip lp [B,E,M,S]
