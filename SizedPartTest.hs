module SizedPartTest where

import Test.QuickCheck

import SizedPart

eqf :: Int -> [[Int]] -> [[Int]] -> Bool
eqf optw xss yss = f optw xss == f optw yss

sizedpart_correct :: Int -> Int -> Int -> Property
sizedpart_correct optw wordWth wordNum  =
  forAll (inpGen wordWth wordNum) $
    \xs -> eqf optw (sizedpart_spec optw xs) (sizedpart optw xs)

inpGen :: Int -> Int -> Gen [Int]
inpGen wordWth wordNum = sized (\n -> genList (wordNum `min` n))
  where genList 0 = return []
        genList n = do a <- genElem
                       x <- genList (n - 1)
                       return (a : x)
        genElem :: Gen Int
        genElem = do a <- choose (1, wordWth)
                     return a
