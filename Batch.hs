{-# LANGUAGE ViewPatterns #-}
module Batch where

import Data.Array

import Spec
import QueueSeg
import OptPart

type Span = Int
type Weight = Int
type Job = (Span, Weight)

tm :: Job -> Span
tm = fst

wt :: Job -> Weight
wt = snd

bspan :: Span -> [Job] -> Span
bspan s = (s+) . sum . map tm

ftime :: Span -> [[Job]] -> Span
ftime s = sum . map (bspan s)

weights :: [[Job]] -> Weight
weights = sum . map wt . concat

w :: Int -> ([Job], [Job]) -> Int
w s (xs, ys) = bspan s xs * sum (map wt (xs ++ ys))

f :: Int -> [[Job]] -> Int
f s [] = 0
f s (xs:xss) = bspan s xs * weights (xs:xss) + f s xss

batch_spec :: Int -> [Job] -> [[Job]]
batch_spec s = opt_spec (w s)

---

batchDict :: Span -> SQDict Job (Span, Weight) Weight Int
batchDict s =
  SQDict 0 pMemoCons 0 pCostCons
         memoSing memoCat memoDeQL memoDeQR
 where pMemoCons (sp, w) w'   = w + w'
       pCostCons (sp, w) _ w' r = (s + sp) * (w + w') + r
       memoSing (sp, w) = (sp, w)
       memoCat  (s1,w1) (s2,w2) = (s1+s2, w1+w2)
       memoDeQL (s1,w1) (s2,w2) = (s2-s1, w2-w1)
       memoDeQR (s1,w1) (s2,w2) = (s1-s2, w1-w2)

batch :: Int -> [Job] -> [[Job]]
batch s = opt (batchDict s) delta
  where delta :: (Array Int (Part Job (Span, Weight) Weight Int)) ->
                 Int -> Seg Job (Span, Weight) -> Double
        delta optArr n xs =
           fromIntegral (cost (optArr ! n) - cost (optArr ! m)) /
           fromIntegral (spanSeg xs)
          where m = n - sLength xs
                spanSeg = fst . sMemo
