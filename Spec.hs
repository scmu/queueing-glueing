module Spec where

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
minBy c (x:xs) | c x <= c y = x
               | otherwise  = y
 where y = minBy c xs

w2f :: (Num r, Ord r) => (([e],[e]) -> r) -> [[e]] -> r
w2f w [] = 0
w2f w (xs : xss) = w (xs, concat xss) + w2f w xss

-- taking w as parameter.

opt_spec :: (Num r, Ord r) => (([e],[e]) -> r) -> [e] -> [[e]]
opt_spec w = minBy (w2f w) . parts

-- taking f as parameter.

opt_spec' :: (Num r, Ord r) => ([[e]] -> r) -> [e] -> [[e]]
opt_spec' f = minBy f . parts
