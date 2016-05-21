{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module OptPartList where

import Data.Array

prefs :: [a] -> [[a]]
prefs [] = []
prefs (x:xs) = [x] : map (x:) (prefs xs)

suffs :: [a] -> [[a]]
suffs [] = []
suffs (x:xs) = (x : xs) : suffs xs

splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = ([x], xs) : map (\(ys, zs) -> (x:ys, zs)) (splits xs)

parts :: [a] -> [[[a]]]
parts [] = [[]]
parts xs = concat . map extparts . splits $ xs
  where extparts (xs, ys) = map (xs:) (parts ys)

minBy :: Ord r => (a -> r) -> [a] -> a
minBy c [x] = x
minBy c (x:xs)
  | c x <= c y = x
  | otherwise  = y
 where y = minBy c xs

w2f :: (Num r, Ord r) => (([e],[e]) -> r) -> [[e]] -> r
w2f w [] = 0
w2f w (xs : xss) = w (xs, concat xss) + w2f w xss

opt_spec :: forall e r . (Num r, Ord r) =>
            (([e],[e]) -> r) ->
            [e] -> [[e]]
opt_spec w = minBy (w2f w) . parts

type Nat = Int

opt :: forall e r t . (Num r, Ord r, Ord t) =>
       (([e],[e]) -> r) ->
       ((Array Nat [[[e]]]) -> Nat -> [e] -> t) ->
       [e] -> [[e]]
opt w d inp = map concat (optArr ! (length inp))
 where
   f = w2f w

   g :: Nat -> [e] -> r
   g n xs = let zss = map concat (optArr ! (n - length xs))
            in w (xs, concat zss) + f zss

   gc :: Nat -> [[e]] -> r
   gc n = g n . concat

   prepend :: Nat -> [[e]] -> [[e]]
   prepend n [xs] = [xs]
   prepend n (xs:ys:xss)
     | d optArr n xs <= d optArr (n - length xs) ys
                  = prepend n ((xs++ys):xss)
     | otherwise  = xs:ys:xss

   minchop :: Nat -> [[e]] -> [[e]]
   minchop n [xs] = [xs]
   minchop n xss
     | gc n yss <= gc n xss = minchop n yss
     | otherwise            = xss
    where yss = init xss

   optArr :: Array Nat [[[e]]]
   optArr = array (0, length inp)
             [ (length ys, optpart ys) | ys <- ([] : suffs inp) ]

   optpart :: [e] -> [[[e]]]
   optpart [] = []
   optpart xs = ys : optArr ! (length xs - length (concat ys))
     where ys = optpref xs

   optpref :: [e] -> [[e]]
   optpref [x] = [[x]]
   optpref (x:xs) = minchop (1+n) ([x] : prepend n (head (optArr ! n)))
       where n = length xs

optArray :: forall e r t . (Num r, Ord r, Ord t) =>
       (([e],[e]) -> r) ->
       ((Array Nat [[[e]]]) -> Nat -> [e] -> t) ->
       [e] -> Array Nat [[[e]]]
optArray w d inp = optArr
 where
   f = w2f w

   g :: Nat -> [e] -> r
   g n xs = let zss = map concat (optArr ! (n - length xs))
            in w (xs, concat zss) + f zss

   gc :: Nat -> [[e]] -> r
   gc n = g n . concat

   prepend :: Nat -> [[e]] -> [[e]]
   prepend n [xs] = [xs]
   prepend n (xs:ys:xss)
     | d optArr n xs <= d optArr (n - length xs) ys
                  = prepend n ((xs++ys):xss)
     | otherwise  = xs:ys:xss

   minchop :: Nat -> [[e]] -> [[e]]
   minchop n [xs] = [xs]
   minchop n xss
     | gc n yss <= gc n xss = minchop n yss
     | otherwise            = xss
    where yss = init xss

   optArr :: Array Nat [[[e]]]
   optArr = array (0, length inp)
             [ (length ys, optpart ys) | ys <- ([] : suffs inp) ]

   optpart :: [e] -> [[[e]]]
   optpart [] = []
   optpart xs = ys : optArr ! (length xs - length (concat ys))
     where ys = optpref xs

   optpref :: [e] -> [[e]]
   optpref [x] = [[x]]
   optpref (x:xs) = minchop (1+n) ([x] : prepend n (head (optArr ! n)))
       where n = length xs
