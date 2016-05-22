module BatchTest where

import Batch
import Test.QuickCheck

-- usage: verboseCheck (batch_correct startOverhead maxWeight maxSpan maxLen)

eqf :: Int -> [[(Int,Int)]] -> [[(Int,Int)]] -> Bool
eqf s xss yss = f s xss == f s yss

batch_correct :: Int -> Int -> Int -> Int -> Property
batch_correct s maxWeight maxSpan maxLen  =
  forAll (inpGen maxWeight maxSpan maxLen) $
    \xs -> eqf s (batch_spec s xs) (batch s xs)

inpGen :: Int -> Int -> Int -> Gen [(Int,Int)]
inpGen maxWeight maxSpan maxLen = sized (\n -> genList (maxLen `min` n))
  where genList 0 = return []
        genList n = do a <- genElem
                       x <- genList (n - 1)
                       return (a : x)
        genElem :: Gen (Int,Int)
        genElem = do a <- choose (1, maxWeight)
                     b <- choose (1, maxSpan)
                     return (a,b)
