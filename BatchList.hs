module BatchList where

import Data.Array
import OptPartList

type Job = (Int, Int)

tm :: Job -> Int
tm = fst

wt :: Job -> Int
wt = snd

bspan :: Int -> [Job] -> Int
bspan s = (s+) . sum . map tm

ftime :: Int -> [[Job]] -> Int
ftime s = sum . map (bspan s)

weights :: [[Job]] -> Int
weights = sum . map wt . concat

w :: Int -> ([Job], [Job]) -> Int
w s (xs, ys) = bspan s xs * sum (map wt (xs ++ ys))

f :: Int -> [[Job]] -> Int
f s = w2f (w s)

d :: Int -> (Array Nat [[[Job]]]) -> Nat -> [Job] -> Double
d s optArr n xs = fromIntegral (cost (optArr ! n) - cost (optArr ! m)) /
                  fromIntegral (bspan s xs - s)
    where m = n - length xs
          cost = f s . map concat


---

batch_spec :: Int -> [Job] -> [[Job]]
batch_spec s = opt_spec (w s)

batch :: Int -> [Job] -> [[Job]]
batch s = opt (w s) (d s)

batchArr :: Int -> [Job] -> Array Int [[[Job]]]
batchArr s = optArray (w s) (d s)
