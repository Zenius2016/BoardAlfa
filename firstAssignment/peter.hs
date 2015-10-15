module Peter where

import Data.List

-- 1.a

null1 x = x == []

null2 [] = True
null2 _ = False

null3 x
  | x == [] = True
  | otherwise = False

--pembatas

mapPeter _ [] = []
mapPeter f (x:xs) = (f x) : (mapPeter f xs)

--pembatas

foldlPeter _ x [] = x
foldlPeter f x (y:ys) = foldlPeter f next ys
  where
    next = (f x y)

--pembatas

takeWhile' _ [] = []
takeWhile' f (x:xs)
  | (f x) = x : (takeWhile' f xs)
  | otherwise = []

--pembatas

deleteAllPeter _ [] = []
deleteAllPeter x (y:ys)
  | x == y = deleteAllPeter x ys
  | otherwise = y : (deleteAllPeter x ys)

-- 1.b

nubPeter [] = []
nubPeter (x:xs) = x : (nubPeter (deleteAllPeter x xs))

--pembatas

union' xs ys = nubPeter (xs++ys)

--pembatas
