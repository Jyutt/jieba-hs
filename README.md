# jieba-hs

「結巴-hs」是「[結巴](https://github.com/fxsjy/jieba)」中文分詞的Haskell版本。

## 使用
jieba-hs的字典格式與jieba的一模一樣 ([例子](https://github.com/fxsjy/jieba/tree/master/extra_dict))。

```haskell
import System.IO
import Dictionary
import Jieba
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- hGetContents =<< openFile "dict.txt.small" ReadMode
    let dict = dictFromContents contents
    let snt = "就算失望不能绝望"
    let result = cut NoHMM dict snt
    putStrLn $ intercalate "/" result
```
```
*Main> main
就算/失望/不能/绝望
```

## TODO
- [ ] Hidden Markov Model 的 cut 模式
- [ ] TF-IDF 的字典
- [ ] POS Tagging
