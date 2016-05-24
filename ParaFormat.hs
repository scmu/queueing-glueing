{-# LANGUAGE ViewPatterns #-}
module ParaFormat where

import Data.Array
import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))

import Spec
import QueueSeg hiding (pcons)

type Length = Int
type Width = Int
type Elem = Int
type Nat = Int

valid :: Width -> [Elem] -> Bool
valid optw xs = (sum xs + length xs - 1) <= optw

w :: Width -> [Elem] -> Int
w optw xs = (optw - sum xs - length xs + 1)^2

f :: Width -> [[Elem]] -> Int
f _ [] = 0
f _ [xs] = 0
f optw (xs:xss) = w optw xs + f optw xss

parafmt_spec :: Width -> [Elem] -> [[Elem]]
parafmt_spec optw = minBy (f optw) . filter (all (valid optw)) . parts

---------------------------------------------

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

  -- use this in place of the standard pcons.

pcons' :: SQDict a b () Int ->
          Queue a b -> Part a b () Int -> Part a b () Int
pcons' d xs (Part [] () _) = Part [xs] () 0
pcons' d xs (Part xss c r) =
  Part (xs:xss) (pMemoCons d (qMemo xs) c)
                (pCostCons d (qMemo xs) (qLength xs) c r)

-- Main algorithm

parafmt :: Width -> [Elem] -> [[Elem]]
parafmt optw inp = pflatten (optArr ! (length inp))
 where
  inpLength = length inp

  dt :: SQDict Elem Width () Int
  dt = sizeDict optw

  g :: Nat -> Queue Elem Width -> Int
  g n xs = cost (pcons' dt xs (optArr ! (n - qLength xs)))

  sums = array (0, length inp) (scanr (\x (i, s) -> (i+1, x+s+1)) (0,0) inp)

  ws = listArray (0, inpLength) (scanl next (next 0 0) [1.. inpLength])
    where next i j | i >= inpLength = inpLength
                   | sums ! i - sums ! j - 1 <= optw = next (i+1) j
                   | otherwise = i

  optArr :: Array Nat (Part Elem Width () Int)
  optArr = array (0, inpLength) $
              map (\(n, xs) -> (n, optpart n xs))
                  (scanr (\x (i,xs) -> (i+1, x:xs)) (0, []) inp)

  optpart :: Nat -> [Elem] -> Part Elem Width () Int
  optpart n [] = emptyPart dt
  optpart n xs = pcons' dt ys (optArr ! (n - qLength ys))
       where ys = optpref n xs

  optpref :: Nat -> [Elem] -> Queue Elem Width
  optpref n [x] = singleQueue (singleSeg dt x)
  optpref n (x:xs) =
    minchop n (qcons dt (singleSeg dt x)
                (prepend (n-1) (phead (optArr ! (n-1)))))

  delta :: Nat -> Seg Elem Width -> Double
  delta n xs =
     fromIntegral (sj^2 - sk^2 + cost (optArr ! n) - cost (optArr ! m)) / fromIntegral (sj - sk)
    where sj = sums ! n
          sk = sums ! m
          m = n - sLength xs

  prepend :: Nat -> Queue Elem Width -> Queue Elem Width
  prepend n (qviewl2 dt -> Left xs) = singleQueue xs
  prepend n (qviewl2 dt -> Right (xs, ys, yss))
        | delta n xs <= delta m ys &&
          delta n xs <= fromIntegral (2 * (sums ! k - optw - 1)) =
               prepend n ((xs |++ ys) |>> yss)
        | otherwise = xs |>> (ys |>> yss)
       where m = n - sLength xs
             k = ws ! (m - sLength ys)
             (|>>) = qcons dt
             (|++) = sapp dt

  minchop :: Nat -> Queue Elem Width -> Queue Elem Width
  minchop n (qviewr2 dt -> Left xs) = singleQueue xs
  minchop n (qviewr2 dt -> Right (yss', ys, xs))
       | wl > optw          = minchop n yss
       | g n yss <= g n xss = minchop n yss
       | otherwise          = xss
      where (<<|) = qsnoc dt
            yss = yss' <<| ys
            xss = yss  <<| xs
            wl = qMemo xss + qLength xss -1
