{-# LANGUAGE ViewPatterns #-}
module QueueSeg where

import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))

data JList a = Sing a | Join (JList a) (JList a)  deriving Show
-- newtype Memo b = Mem b                            deriving Show
data Seg a b = Seg (JList a) !b                   deriving Show
data Queue a b = Queue (Seq (Seg a b)) !b         deriving Show
data Parts a b c = Parts [Queue a b] !c           deriving Show

data SQDict a b c = SQDict {
    emptyP :: c
  , pConsMemo :: b -> c -> c
  , singleMemo :: a -> b
  , catMemo :: b -> b -> b
  , decatMemoL :: b -> b -> b
  , decatMemoR :: b -> b -> b
  }

qGetMemo :: Queue a b -> b
qGetMemo (Queue _ m) = m

pGetMemo :: Parts a b c -> c
pGetMemo (Parts _ c) = c

scat :: SQDict a b c -> Seg a b -> Seg a b -> Seg a b
scat d (Seg xs m) (Seg ys n) = Seg (xs `Join` ys) (catMemo d m n)

qcons :: SQDict a b c -> Seg a b -> Queue a b -> Queue a b
qcons d (Seg x m) (Queue xs n) = Queue ((Seg x m) <| xs) (catMemo d m n)

qsnoc :: SQDict a b c -> Queue a b -> Seg a b -> Queue a b
qsnoc d (Queue xs m) (Seg x n) = Queue (xs |> (Seg x n)) (catMemo d m n)

singleSeg :: SQDict a b c -> a -> Seg a b
singleSeg d x = Seg (Sing x) (singleMemo d x)

singleQueue :: Seg a b -> Queue a b
singleQueue (Seg xs m) = Queue (singleton (Seg xs m)) m

emptyParts :: SQDict a b c -> Parts a b c
emptyParts d = Parts [] (emptyP d)

pcons :: SQDict a b c -> Queue a b -> Parts a b c -> Parts a b c
pcons d xs (Parts xss c) = Parts (xs:xss) (pConsMemo d (qGetMemo xs) c)

phead :: Parts a b c -> Queue a b
phead (Parts xss _) = head xss

jflatten :: JList a -> [a]
jflatten (Sing x) = [x]
jflatten (Join xs ys) = jflatten xs ++ jflatten ys

ssflatten :: Seq (Seg a b) -> [a]
ssflatten (viewl -> EmptyL) = []
ssflatten (viewl -> (Seg xs _) :< yss) = jflatten xs ++ ssflatten yss

qflatten :: Queue a b -> [a]
qflatten (Queue xss _) = ssflatten xss

pflatten :: Parts a b c -> [[a]]
pflatten (Parts xss _) = map qflatten xss

qviewl :: SQDict a b c -> Queue a b -> Maybe (Seg a b, Queue a b)
qviewl d (Queue xs m)
  | EmptyL          <- viewl xs = Nothing
  | (Seg y n) :< ys <- viewl xs = Just (Seg y n, Queue ys (decatMemoL d n m))

qviewl2 :: SQDict a b c -> Queue a b ->
           Either (Seg a b) (Seg a b, Seg a b, Queue a b)
qviewl2 d (qviewl d -> Just (x, xs'))
  | Nothing      <- qviewl d xs' = Left x
  | Just (y, ys) <- qviewl d xs' = Right (x, y, ys)

qviewr :: SQDict a b c -> Queue a b -> Maybe (Queue a b, Seg a b)
qviewr d (Queue xs m)
  | EmptyR          <- viewr xs = Nothing
  | ys :> (Seg y n) <- viewr xs = Just (Queue ys (decatMemoR d m n), Seg y n)

qviewr2 :: SQDict a b c -> Queue a b ->
           Either (Seg a b) (Queue a b, Seg a b, Seg a b)
qviewr2 d (qviewr d -> Just (xs', x))
  | Nothing      <- qviewr d xs' = Left x
  | Just (ys, y) <- qviewr d xs' = Right (ys, y, x)
