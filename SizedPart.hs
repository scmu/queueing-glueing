{-# LANGUAGE ViewPatterns #-}
module SizedPart where

import Data.Array
import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))

import Spec
import QueueSeg
import OptPart

f :: Int -> [[Int]] -> Int
f optw = sum . map (\xs -> (optw - sum xs - length xs + 1)^2)

w :: Int -> ([Int], [Int]) -> Int
w optw (xs, _) = (optw - sum xs - length xs + 1)^2

sizedpart_spec :: Int -> [Int] -> [[Int]]
sizedpart_spec optw = opt_spec (w optw)

---------------------------------------------

type Length = Int
type Width = Int
type Elem = Int

sizeDict :: Width -> SQDict Elem Width () Int
sizeDict optw =
  SQDict () pMemoCons 0 pCostCons
         memoSing memoCat memoDeQL memoDeQR
 where pMemoCons _ _  = ()
       pCostCons w l () r = (optw - w - l + 1)^2 + r
       memoSing w = w
       memoCat  w1 w2 = w1 + w2
       memoDeQL w1 w2 = w2 - w1
       memoDeQR w1 w2 = w1 - w2

-- Main algorithm

sizedpart :: Width -> [Elem] -> [[Elem]]
sizedpart optw inp = opt (sizeDict optw) delta inp
  where delta :: (Array Int (Part Elem Width () Int)) ->
                 Int -> Seg Elem Width -> Double
        delta optArr n xs =
           fromIntegral (sj^2 - sk^2 + cost (optArr ! n) - cost (optArr ! m)) /
           fromIntegral (sj - sk)
          where sj = sums ! n
                sk = sums ! m
                m = n - sLength xs
        sums = array (0, length inp)
                (scanr (\x (i, s) -> (i+1, x+s+1)) (0,0) inp)
