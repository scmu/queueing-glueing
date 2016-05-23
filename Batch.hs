{-# LANGUAGE ViewPatterns #-}
module Batch where

import Data.Array
import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))

import Spec

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

-- f :: Int -> [[Job]] -> Int
-- f s [] = 0
-- f s (xs:xss) = bspan s xs * weights (xs:xss) + f s xss

batch_spec :: Int -> [Job] -> [[Job]]
batch_spec s = opt_spec (w s)

---------------------------------------------
data JList a = Sing a | Join (JList a) (JList a) deriving Show
data Memo = Mem !Int !Int !Int                   deriving Show
data Seg' = Seg' (JList Job) !Memo               deriving Show
data Seg = Seg (Seq Seg') !Memo                  deriving Show
data Parts = Parts [Seg] !Int !Int               deriving Show

singleMemo :: Job -> Memo
singleMemo x = Mem (tm x) (wt x) 1

bspanMemo :: Int -> Memo -> Int
bspanMemo s (Mem pt _ _) = s + pt

weightMemo :: Memo -> Int
weightMemo (Mem _ wt _) = wt

lengthMemo :: Memo -> Int
lengthMemo (Mem _ _ l) = l

catMemo :: Memo -> Memo -> Memo
catMemo (Mem t1 w1 l1) (Mem t2 w2 l2) = Mem (t1+t2) (w1+w2) (l1+l2)

decatMemoL :: Memo -> Memo -> Memo
decatMemoL (Mem t1 w1 l1) (Mem t2 w2 l2) = Mem (t2-t1) (w2-w1) (l2-l1)

decatMemoR :: Memo -> Memo -> Memo
decatMemoR (Mem t1 w1 l1) (Mem t2 w2 l2) = Mem (t1-t2) (w1-w2) (l1-l2)


bspanSeg' :: Int -> Seg' -> Int
bspanSeg' s (Seg' _ m) = bspanMemo s m

weightSeg' :: Seg' -> Int
weightSeg' (Seg' _ m) = weightMemo m

lengthSeg' :: Seg' -> Int
lengthSeg' (Seg' _ m) = lengthMemo m

bspanSeg :: Int -> Seg -> Int
bspanSeg s (Seg _ m) = bspanMemo s m

weightSeg :: Seg -> Int
weightSeg (Seg _ m) = weightMemo m

lengthSeg :: Seg -> Int
lengthSeg (Seg _ m) = lengthMemo m

scat :: Seg' -> Seg' -> Seg'
scat (Seg' xs m) (Seg' ys n) = Seg' (xs `Join` ys) (catMemo m n)

scons :: Seg' -> Seg -> Seg
scons (Seg' x m) (Seg xs n) = Seg ((Seg' x m) <| xs) (catMemo m n)

ssnoc :: Seg -> Seg' -> Seg
ssnoc (Seg xs m) (Seg' x n) = Seg (xs |> (Seg' x n)) (catMemo m n)

singleSeg' :: Job -> Seg'
singleSeg' x = Seg' (Sing x) (singleMemo x)

singleSeg :: Seg' -> Seg
singleSeg (Seg' xs m) = Seg (singleton (Seg' xs m)) m

emptyParts :: Parts
emptyParts = Parts [] 0 0

pcons :: Int -> Seg -> Parts -> Parts
pcons s xs (Parts xss w c) = Parts (xs:xss) w' (bspanSeg s xs * w' + c)
  where w' = weightSeg xs + w

phead :: Parts -> Seg
phead (Parts xss _ _) = head xss

cost :: Parts -> Int
cost (Parts _ _ c) = c

jflatten :: JList Job -> [Job]
jflatten (Sing x) = [x]
jflatten (Join xs ys) = jflatten xs ++ jflatten ys

sflatten' :: Seq Seg' -> [Job]
sflatten' (viewl -> EmptyL) = []
sflatten' (viewl -> (Seg' xs _) :< yss) = jflatten xs ++ sflatten' yss

sflatten :: Seg -> [Job]
sflatten (Seg xss _) = sflatten' xss

pflatten :: Parts -> [[Job]]
pflatten (Parts xss _ _) = map sflatten xss

sviewl :: Seg -> Maybe (Seg', Seg)
sviewl (Seg xs m)
  | EmptyL <- viewl xs           = Nothing
  | (Seg' y n) :< ys <- viewl xs = Just (Seg' y n, Seg ys (decatMemoL n m))

sviewl2 :: Seg -> Either Seg' (Seg', Seg', Seg)
sviewl2 (sviewl -> Just (x, xs'))
  | Nothing <- sviewl xs'      = Left x
  | Just (y, ys) <- sviewl xs' = Right (x, y, ys)

sviewr :: Seg -> Maybe (Seg, Seg')
sviewr (Seg xs m)
  | EmptyR <- viewr xs           = Nothing
  | ys :> (Seg' y n) <- viewr xs = Just (Seg ys (decatMemoR m n), Seg' y n)

sviewr2 :: Seg -> Either Seg' (Seg, Seg', Seg')
sviewr2 (sviewr -> Just (xs', x))
  | Nothing <- sviewr xs'      = Left x
  | Just (ys, y) <- sviewr xs' = Right (ys, y, x)


-- Main algorithm

batch :: Int -> [Job] -> [[Job]]
batch s inp = pflatten (optArr ! (length inp))
  where
     g :: Int -> Seg -> Int
     g n xs = cost (pcons s xs (optArr ! (n - lengthSeg xs)))

     optArr = array (0, length inp) $
                   map (\(n, xs) -> (n, optpart n xs)) (scanr (\x (i,xs) -> (i+1, x:xs)) (0, []) inp)

     optpart n [] = emptyParts
     optpart n xs = pcons s ys (optArr ! (n - lengthSeg ys))
       where ys = optpref n xs

     optpref n [x] = singleSeg (singleSeg' x)
     optpref n (x:xs) = minchop n (singleSeg' x `scons` prepend (n-1) (phead         (optArr ! (n-1))))

     delta :: Int -> Seg' -> Double
     delta n xs = fromIntegral (cost (optArr ! n) - cost (optArr ! m)) /
                  fromIntegral (bspanSeg' s xs - s)
       where m = n - lengthSeg' xs

     prepend :: Int -> Seg -> Seg
     prepend n (sviewl2 -> Left xs) = singleSeg xs
     prepend n (sviewl2 -> Right (xs, ys, yss))
       | delta n xs <= delta (n - lengthSeg' xs) ys = prepend n ((xs `scat` ys) `scons` yss)
       | otherwise                                  = xs `scons` (ys `scons` yss)

     minchop :: Int -> Seg -> Seg
     minchop n (sviewr2 -> Left xs) = singleSeg xs
     minchop n (sviewr2 -> Right (yss, ys, xs))
       | g n (yss `ssnoc` ys) <= g n ((yss `ssnoc` ys) `ssnoc` xs) = minchop n (yss `ssnoc` ys)
       | otherwise                                                 = (yss `ssnoc` ys) `ssnoc` xs
