{-# LANGUAGE RankNTypes, ScopedTypeVariables, ViewPatterns #-}
module OptPart where

import Data.Array

import QueueSeg

type Nat = Int

opt :: forall e b c r t . (Num r, Ord r, Ord t) =>
       SQDict e b c r ->
       -- (([e],[e]) -> r) ->
       ((Array Nat (Part e b c r)) -> Nat -> Seg e b -> t) ->
       [e] -> [[e]]
opt dt delta inp = pflatten (optArr ! (length inp))
 where
  g :: Nat -> Queue e b -> r
  g n xs = cost (pcons dt xs (optArr ! (n - lengthQueue xs)))

  optArr :: Array Nat (Part e b c r)
  optArr = array (0, length inp) $
                   map (\(n, xs) -> (n, optpart n xs)) (scanr (\x (i,xs) -> (i+1, x:xs)) (0, []) inp)

  optpart :: Nat -> [e] -> Part e b c r
  optpart n [] = emptyPart dt
  optpart n xs = pcons dt ys (optArr ! (n - lengthQueue ys))
    where ys = optpref n xs

  optpref :: Nat -> [e] -> Queue e b
  optpref n [x] = singleQueue (singleSeg dt x)
  optpref n (x:xs) =
    minchop n (qcons dt (singleSeg dt x)
                (prepend (n-1) (phead (optArr ! (n-1)))))

  minchop :: Nat -> Queue e b -> Queue e b
  minchop n (qviewr2 dt -> Left xs) = singleQueue xs
  minchop n (qviewr2 dt -> Right (yss, ys, xs))
    | g n (yss <<| ys) <= g n ((yss <<| ys) <<| xs) =
           minchop n (yss <<| ys)
    | otherwise = (yss <<| ys) <<| xs
   where (<<|) = qsnoc dt

  prepend :: Nat -> Queue e b -> Queue e b
  prepend n (qviewl2 dt -> Left xs) = singleQueue xs
  prepend n (qviewl2 dt -> Right (xs, ys, yss))
    | delta optArr n xs <= delta optArr (n - lengthSeg xs) ys =
         prepend n ((xs |++ ys) |>> yss)
    | otherwise = xs |>> (ys |>> yss)
   where (|>>) = qcons dt
         (|++) = sapp dt
