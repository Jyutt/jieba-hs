import Jieba
import Jieba.Parsec.FreqDict
import Jieba.Parsec.HmmWeights
import Jieba.Dictionary.FreqDict
import Jieba.Dictionary.HmmDict
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Control.DeepSeq (deepseq)

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

-- Run all the tests
main :: IO ()
main = do
    quickCheck (fileLoads freqDict)
    quickCheck (fileLoads hmmDict)
    quickCheck cutHmmHalts
    quickCheck cutNoHmmHalts
    quickCheck cutAllHalts