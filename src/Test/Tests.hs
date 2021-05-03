import Jieba
import Jieba.Parsec.FreqDict
import Jieba.Parsec.HmmWeights
import Jieba.Dictionary.FreqDict
import Jieba.Dictionary.HmmDict
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Control.DeepSeq (deepseq)

import Test.QuickCheck.Jieba.Instances

freqDict :: FreqDict
freqDict = unsafePerformIO $ readFreqDict "data/dict.txt.small"

hmmDict :: HmmDict
hmmDict = unsafePerformIO $ readHmmDict "data/hmm.model"

fileLoads :: a -> Bool
fileLoads file = file `seq` True

cutHmmHalts :: String -> Bool
cutHmmHalts s = cutHMM freqDict hmmDict s `deepseq` True

cutNoHmmHalts :: String -> Bool
cutNoHmmHalts s = cutNoHMM freqDict s `deepseq` True

cutAllHalts :: String -> Bool
cutAllHalts s = cutAll freqDict s `deepseq` True

stEquivalentToPure :: String -> Bool
stEquivalentToPure s = pureRes == stRes
    where
        pureRes = cutNoHMM freqDict s
        stRes = cutNoHMM' freqDict s

stEquivalentToPure' :: [ZhChar] -> Bool
stEquivalentToPure' s' = pureRes == stRes
    where
        s = toString s'
        pureRes = cutNoHMM freqDict s
        stRes = cutNoHMM' freqDict s

-- Run all the tests
main :: IO ()
main = do
    pureGraphTests
    stGraphEquivalenceTests

pureGraphTests :: IO ()
pureGraphTests = do
    putStrLn "Performing PureGraph tests..."
    quickCheck (fileLoads freqDict)
    quickCheck (fileLoads hmmDict)
    quickCheck cutHmmHalts
    quickCheck cutNoHmmHalts
    quickCheck cutAllHalts

stGraphEquivalenceTests :: IO ()
stGraphEquivalenceTests = do
    putStrLn "Testing ST Graph equivalence wrt. Pure Graph implementation"
    quickCheck stEquivalentToPure
    quickCheck stEquivalentToPure'