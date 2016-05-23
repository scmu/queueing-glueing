{-# LANGUAGE ViewPatterns #-}
module QueueSeg where

import Data.Sequence (Seq(..), singleton, empty,
                      ViewL(..), viewl, (<|),
                      ViewR(..), viewr, (|>))

data JList a = Sing a | Join (JList a) (JList a)  deriving Show
-- newtype Memo b = Mem b                            deriving Show
data Seg a b = Seg (JList a) !b !Int              deriving Show
data Queue a b = Queue (Seq (Seg a b)) !b !Int    deriving Show
data Part a b c r = Part [Queue a b] !c !r        deriving Show

data SQDict a b c r = SQDict {
    pMemoEmpty :: c
  , pMemoCons  :: b -> c -> c
  , pCostZero  :: r
  , pCostCons  :: b -> c -> r -> r
  , memoSing   :: a -> b
  , memoCat    :: b -> b -> b
  , memoDeQL   :: b -> b -> b
  , memoDeQR   :: b -> b -> b
  }

cost :: Part a b c r -> r
cost (Part _ _ r) = r

sMemo :: Seg a b -> b
sMemo (Seg _ m _) = m

qMemo :: Queue a b -> b
qMemo (Queue _ m _) = m

pMemo :: Part a b c r -> c
pMemo (Part _ c _) = c

lengthSeg :: Seg a b -> Int
lengthSeg (Seg _ _ n) = n

lengthQueue :: Queue a b -> Int
lengthQueue (Queue _ _ n) = n

sapp :: SQDict a b c r -> Seg a b -> Seg a b -> Seg a b
sapp d (Seg xs u m) (Seg ys v n) =
  Seg (xs `Join` ys) (memoCat d u v) (m + n)

qcons :: SQDict a b c r -> Seg a b -> Queue a b -> Queue a b
qcons d (Seg x u m) (Queue xs v n) =
  Queue ((Seg x u m) <| xs) (memoCat d u v) (m + n)

qsnoc :: SQDict a b c r -> Queue a b -> Seg a b -> Queue a b
qsnoc d (Queue xs u m) (Seg x v n) =
  Queue (xs |> (Seg x v n)) (memoCat d u v) (m + n)

singleSeg :: SQDict a b c r -> a -> Seg a b
singleSeg d x = Seg (Sing x) (memoSing d x) 1

singleQueue :: Seg a b -> Queue a b
singleQueue (Seg xs u m) = Queue (singleton (Seg xs u m)) u m

emptyPart :: SQDict a b c r -> Part a b c r
emptyPart d = Part [] (pMemoEmpty d) (pCostZero d)

pcons :: SQDict a b c r -> Queue a b -> Part a b c r -> Part a b c r
pcons d xs (Part xss c r) =
  Part (xs:xss) (pMemoCons d (qMemo xs) c)
                (pCostCons d (qMemo xs) c r)

phead :: Part a b c r -> Queue a b
phead (Part xss _ _) = head xss

jflatten :: JList a -> [a]
jflatten (Sing x) = [x]
jflatten (Join xs ys) = jflatten xs ++ jflatten ys

ssflatten :: Seq (Seg a b) -> [a]
ssflatten (viewl -> EmptyL) = []
ssflatten (viewl -> (Seg xs _ _) :< yss) = jflatten xs ++ ssflatten yss

qflatten :: Queue a b -> [a]
qflatten (Queue xss _ _) = ssflatten xss

pflatten :: Part a b c r -> [[a]]
pflatten (Part xss _ _) = map qflatten xss

qviewl :: SQDict a b c r -> Queue a b -> Maybe (Seg a b, Queue a b)
qviewl d (Queue xs v m)
  | EmptyL          <- viewl xs = Nothing
  | (Seg y u n) :< ys <- viewl xs =
      Just (Seg y u n, Queue ys (memoDeQL d u v) (m - n))

qviewl2 :: SQDict a b c r -> Queue a b ->
           Either (Seg a b) (Seg a b, Seg a b, Queue a b)
qviewl2 d (qviewl d -> Just (x, xs'))
  | Nothing      <- qviewl d xs' = Left x
  | Just (y, ys) <- qviewl d xs' = Right (x, y, ys)

qviewr :: SQDict a b c r -> Queue a b -> Maybe (Queue a b, Seg a b)
qviewr d (Queue xs u m)
  | EmptyR          <- viewr xs = Nothing
  | ys :> (Seg y v n) <- viewr xs =
      Just (Queue ys (memoDeQR d u v) (m - n), Seg y v n)

qviewr2 :: SQDict a b c r -> Queue a b ->
           Either (Seg a b) (Queue a b, Seg a b, Seg a b)
qviewr2 d (qviewr d -> Just (xs', x))
  | Nothing      <- qviewr d xs' = Left x
  | Just (ys, y) <- qviewr d xs' = Right (ys, y, x)
