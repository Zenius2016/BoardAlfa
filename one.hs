module One where

import Data.List

f m a = m * a

gaya m a = a * m

gayaGravitasi g m1 m2 r = (g*m1*m2) / (r^2)

pitagor a b = sqrt (a^2 + b^2)

mutlak x =
    if (x >= 0) && (x /= 5)
    then x
    else (- x)

mutlak' x
    | x >= 0 = x
    | x < 0 = (- x)

mutlak'' x
    | x >= 0 = x
    | otherwise = (- x)

mutlak''' x
    | x >= 0 = x
    | True = (- x)

ngasal x y
    | x == y = "sama"
    | x <= y = "keciiiil"
    | x >= y = "gueeddeee"
    | otherwise = "ngasal"

-- 2 < x < 5 => (2<x) && (x<5)

ngasal2 x
    | (0 < x) && (x < 5) = "0 sampe 5"
    | ((- 2) < x) && (x < 3) = "gitu deeeh"
    | (1 < x) && (x < 10) = "nagini"
    | otherwise = "woooii"

-- | - 3 | = 3
-- n! = n * n-1 * n-2 .. 1
-- n! => kalo (n <= 1) maka 1 else (n * (n-1)!)
-- apa pun

faktorial n
    | n <= 1 = 1
    | otherwise = n * (faktorial (n - 1))

rekurRekuran x
    | x == 0 = "yeeeiii"
    | otherwise = rekurRekuran (x - 1)

rekurLengkap x
    | x == 0 = "yeeeiii"
    | x < (- 10) = "yeeooo"
    | otherwise = rekurLengkap (x-1)

fst' (a,b) = a

snd' (a,b) = b

fak 0 = 1
fak 1 = 1
fak n = n * fak (n-1)

length' [] = 0
length' (x:xs) = 1 + (length' xs)

sum' [] = 0
sum' (x:xs) = x + sum' xs

koko = [1..4]

drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- drop' 3 [1,2,3,4,5,6,7,8] = drop' 3 [2..8]



-- sum' [1,2,3] => 1 + sum' [2,3] => 1+5
-- sum' [2,3] => 2 + sum' [3]  => 2+3
-- sum' [3] = 3 + sum' []

-- length' [1,2,4] -> 3
-- 1 + length' [2,4] -> 1 + 2
-- 1+ length' [4] -> 1+1
-- 1 + 0


-- r -1
-- r -2
-- r -3


-- r 3
-- r 2
-- r 1
-- r 0
-- "yeeei"

-- 3! => 3*2 => 6
-- 2! => 2*1
-- 1! => 1
















































-- save
