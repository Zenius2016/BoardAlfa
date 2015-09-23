module Three where

import Data.List

sol1 lim = iter 1 0
    where iter i result
            | i > lim = result
            | rem i 3 == 0 || rem i 5 == 0 = iter (i+1) (result + i)
            | otherwise = iter (i+1) result

sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' lst = helper lst 0
    where helper [] res = res
          helper (x:xs) res = helper xs (x+res)

-- sum [2,3,4] = helper [2,3,4] 0
-- helper [2,3,4] 0 = helper [3,4] (2+0)
-- helper [3,4] 2 = helper [4] (3+2)
-- helper [4] 5 = helper [] (4+5)

-- sum [3,5,5,8] = 3 + sum [5,5,8]
-- sum [5,5,8] = 5 + sum [5,8]
-- .. sum [] = 0

-- iter 1 0 => iter 2 0
-- iter 2 0 => iter 3 0
-- iter 3 0 => iter 4 3
-- iter 4 3 => iter 5 3
-- iter 5 3 => iter 6 8
-- iter 6 8 => iter 7 14


-- p => q
-- ~ q => ~ p

eulerNo1a = sum [x | x <- [1..999], rem x 3 == 0 || rem x 5 == 0]
eulerNo1b = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
pitagoras = [(x,y,z) | x <- [1..100], y <- [x..100], z <- [y..100], z^2 == x^2 + y^2]
daduGanjil = [(a,b) | a <- [1..6], b <- [1..6], let c = a*b, odd c]


















































-- save
