module Test.QuickCheck.Jieba.Instances where

import Test.QuickCheck
import Data.Char
import Data.Coerce

-- TODO: Split into Simplified and Traditional?
-- newtype ZhSimpChar = ZhSimpChar Char
-- newtype ZhTradChar = ZhTradChar Char
newtype ZhChar = ZhChar Char deriving Show

toString :: [ZhChar] -> String
toString = coerce

-- Beginning and end ranges for CJK Unified Ideographs
-- Sources: https://jrgraphix.net/research/unicode_blocks.php
-- https://stackoverflow.com/questions/1366068/whats-the-complete-range-for-chinese-characters-in-unicode
zh_begin :: Int
zh_begin = 0x4e00

zh_end :: Int
zh_end = 0x9fff

instance Arbitrary ZhChar where
    arbitrary = ZhChar . chr <$> chooseInt (zh_begin, zh_end)