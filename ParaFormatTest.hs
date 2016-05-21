module ParaFormatTest where

import Test.QuickCheck

import ParaFormat

eqf :: [[Int]] -> [[Int]] -> Bool
xss `eqf` yss = f xss == f yss

optpart_correct :: Int -> Int -> Property
optpart_correct wordWth wordNum  =
  forAll (inpGen wordWth wordNum) $
    \xs -> optpart_spec xs `eqf` opt xs

inpGen :: Int -> Int -> Gen [Int]
inpGen wordWth wordNum = sized (\n -> genList (wordNum `min` n))
  where genList 0 = return []
        genList n = do a <- genElem
                       x <- genList (n - 1)
                       return (a : x)
        genElem :: Gen Int
        genElem = do a <- choose (1, wordWth)
                     return a
