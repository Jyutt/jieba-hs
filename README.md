# jieba-hs
![Haskell CI](https://github.com/zyklotomic/jieba-hs/workflows/Haskell%20CI/badge.svg)

# WORK IN PROGRESS, 還缺HMM功能!

「結巴-hs」是「[結巴](https://github.com/fxsjy/jieba)」中文分詞的Haskell版本。

## 使用
jieba-hs的字典格式與jieba的一模一樣 ([例子](https://github.com/fxsjy/jieba/tree/master/extra_dict))。
字典存進 `data/*` 裏。

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
- [x] POS Tagging
- [ ] 查看`Data.Map.Strict` 是否已經使用了 Trie樹。~~字典後備數據結構從 Map 換成 Trie樹~~
- [ ] 解析器例外處理/應不應該使用Parsec？
- [ ] Github Actions 裏加hlint和cabal tests
- [ ] 英語版本的 README
