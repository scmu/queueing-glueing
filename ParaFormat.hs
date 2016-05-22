{-# LANGUAGE ViewPatterns #-}
module ParaFormat where

import Data.Array
import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))


splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = ([x], xs) : map (\(ys, zs) -> (x:ys, zs)) (splits xs)

parts :: [a] -> [[[a]]]
parts [] = [[]]
parts xs = concat . map extparts . splits $ xs
  where extparts (xs, ys) = map (xs:) (parts ys)

minBy :: (a -> Int) -> [a] -> a
minBy c [x] = x
minBy c (x:xs)
  | c x <= c y = x
  | otherwise  = y
 where y = minBy c xs

--------------------------------------------

-- optw :: Int
-- optw = 70

valid :: Int -> [Elem] -> Bool
valid optw xs = (sum xs + length xs - 1) <= optw

w :: Int -> [Elem] -> Int
w optw xs = (optw - sum xs - length xs + 1)^2

f :: Int -> [[Elem]] -> Int
f _ [] = 0
f _ [xs] = 0
f optw (xs:xss) = w optw xs + f optw xss

parafmt_spec :: Int -> [Int] -> [[Int]]
parafmt_spec optw = minBy (f optw) . filter (all (valid optw)) . parts

---------------------------------------------

type Length = Int
type Width = Int
type Elem = Int

data JList a = Sing a | Join (JList a) (JList a)
data Memo = Mem !Width !Length
data Seg' = Seg' (JList Elem) !Memo
data Seg = Seg (Seq Seg') !Memo
data Parts = Parts [Seg] !Int

emptyMemo :: Memo
emptyMemo = Mem 0 0

singleMemo :: Elem -> Memo
singleMemo x = Mem x 1

widthMemo :: Memo -> Width
widthMemo (Mem w _) = w

lengthMemo :: Memo -> Length
lengthMemo (Mem _ l) = l

wlMemo :: Memo -> Int
wlMemo (Mem w l) = w + l - 1

weightMemo :: Int -> Memo -> Int
weightMemo optw (Mem w l) = (optw - w - l + 1)^2

catMemo :: Memo -> Memo -> Memo
catMemo (Mem w1 l1) (Mem w2 l2) = Mem (w1+w2) (l1+l2)

decatMemoL :: Memo -> Memo -> Memo
decatMemoL (Mem w1 l1) (Mem w2 l2) = Mem (w2-w1) (l2-l1)

decatMemoR :: Memo -> Memo -> Memo
decatMemoR (Mem w1 l1) (Mem w2 l2) = Mem (w1-w2) (l1-l2)


widthSeg' :: Seg' -> Int
widthSeg' (Seg' _ m) = widthMemo m

lengthSeg' :: Seg' -> Int
lengthSeg' (Seg' _ m) = lengthMemo m

wlSeg' :: Seg' -> Int
wlSeg' (Seg' _ m) = wlMemo m

weightSeg' :: Int -> Seg' -> Int
weightSeg' optw (Seg' _ m) = weightMemo optw m

widthSeg :: Seg -> Int
widthSeg (Seg _ m) = widthMemo m

lengthSeg :: Seg -> Int
lengthSeg (Seg _ m) = lengthMemo m

wlSeg :: Seg -> Int
wlSeg (Seg _ m) = wlMemo m

weightSeg :: Int -> Seg -> Int
weightSeg optw (Seg _ m) = weightMemo optw m

scat :: Seg' -> Seg' -> Seg'
scat (Seg' xs m) (Seg' ys n) = Seg' (xs `Join` ys) (catMemo m n)

scons :: Seg' -> Seg -> Seg
scons (Seg' x m) (Seg xs n) = Seg ((Seg' x m) <| xs) (catMemo m n)

ssnoc :: Seg -> Seg' -> Seg
ssnoc (Seg xs m) (Seg' x n) = Seg (xs |> (Seg' x n)) (catMemo m n)

singleSeg' :: Elem -> Seg'
singleSeg' x = Seg' (Sing x) (singleMemo x)

singleSeg :: Seg' -> Seg
singleSeg (Seg' xs m) = Seg (singleton (Seg' xs m)) m

emptyParts :: Parts
emptyParts = Parts [] 0

pcons :: Int -> Seg -> Parts -> Parts
pcons optw xs (Parts [] _) = Parts [xs] 0
pcons optw xs (Parts xss c) = Parts (xs:xss) (weightSeg optw xs + c)

phead :: Parts -> Seg
phead (Parts xss _) = head xss

cost :: Parts -> Int
cost (Parts _ c) = c

jflatten :: JList Int -> [Int]
jflatten (Sing x) = [x]
jflatten (Join xs ys) = jflatten xs ++ jflatten ys

sflatten' :: Seq Seg' -> [Int]
sflatten' (viewl -> EmptyL) = []
sflatten' (viewl -> (Seg' xs _) :< yss) = jflatten xs ++ sflatten' yss

sflatten :: Seg -> [Int]
sflatten (Seg xss _) = sflatten' xss

pflatten :: Parts -> [[Int]]
pflatten (Parts xss _) = map sflatten xss

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

parafmt :: Int -> [Elem] -> [[Elem]]
parafmt optw inp = pflatten (optparts ! (length inp))
  where
     g n xs = weightSeg optw xs + cost (optparts ! (n - lengthSeg xs))

     sums = array (0, length inp) (scanr (\x (i, s) -> (i+1, x+s+1)) (0,0) inp)

     ws = listArray (0, n) (scanl next (next 0 0) [1..n])
       where
         next i j
           | i >= n = n
           | sums ! i - sums ! j - 1 <= optw = next (i+1) j
           | otherwise = i
         n = length inp

     optparts = array (0, length inp) $
                   map (\(n, xs) -> (n, optpart n xs)) (scanr (\x (i,xs) -> (i+1, x:xs)) (0, []) inp)

     optpart n [] = emptyParts
     optpart n xs = pcons optw ys (optparts ! (n - lengthSeg ys))
       where ys = optpref n xs

     optpref n [x] = singleSeg (singleSeg' x)
     optpref n (x:xs) = minchop n (singleSeg' x `scons` prepend (n-1) (phead (optparts ! (n-1))))

     delta n xs = fromIntegral (sj^2 - sk^2 + cost (optparts ! n) - cost (optparts ! m)) / fromIntegral (sj - sk)
       where
         sj = sums ! n
         sk = sums ! m
         m = n - lengthSeg' xs

     prepend :: Int -> Seg -> Seg
     prepend n (sviewl2 -> Left xs) = singleSeg xs
     prepend n (sviewl2 -> Right (xs, ys, yss))
        | delta n xs <= delta m ys && delta n xs <= fromIntegral (2 * (sums ! k - optw - 1)) = prepend n ((xs `scat` ys) `scons` yss)
        | otherwise                                                                          = xs `scons` (ys `scons` yss)
       where
         m = n - lengthSeg' xs
         k = ws ! (m - lengthSeg' ys)

     minchop :: Int -> Seg -> Seg
     minchop n (sviewr2 -> Left xs) = singleSeg xs
     minchop n (sviewr2 -> Right (yss', ys, xs))
       | wlSeg xss > optw   = minchop n yss
       | g n yss <= g n xss = minchop n yss
       | otherwise          = xss
      where
        yss = yss' `ssnoc` ys
        xss = yss `ssnoc` xs
