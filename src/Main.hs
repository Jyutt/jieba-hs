import System.IO
--import Jieba.Parsers.FreqDict (dictFromContents)
import Jieba
import Jieba.Parsec.FreqDict
import Jieba.Parsec.HmmWeights
import Jieba.FinalSeg
import Jieba.Types.HmmState
import Data.List (intercalate)

main :: IO ()
main = do
    --contents <- hGetContents =<< openFile "data/dict.txt.small" ReadMode
    --let dict = dictFromContents contents
    dict <- readFreqDict "data/dict.txt.small"
    let snt = "他来到了网易杭研大厦"
    let result = cutNoHMM dict snt
    putStrLn $ intercalate "/" result
    hmmd <- readHmmDict "data/hmm.model"
    let snt = "这是一个HMM的测试"
    let states = viterbi hmmd snt
    let split = segment snt states
    putStrLn . intercalate "/" $ split
