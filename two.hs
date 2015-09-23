module Two where

import Data.List

square x = x * x

cube x = x * x * x

nama_orang = "pake p"
umur = "uzur"

kecap a b c = [((- b) + akarD) / (2*a), ((- b) - akarD)/ (2*a)]
    where akarD = sqrt ((square b) - 4 * a * c)
          akarC = a*b*c

fungsiBerkondisi x = (f1 x) * (f2 x)
    where f1 i = i*i
          f2 i = i*i - i

fLain x = (f1 x) * (f2 x)
    where f1 n
            | n > 0 = n
            | otherwise = (- n)
          f2 (- 1) = 10
          f2 1 = 12
          f2 2 = 13
          f2 _ = 0

-- review berfollownya

faktorial 0 = 1
faktorial 1 = 1
faktorial 2 = 2

f5 x = if x > 0 then x else (- x)

fLainLain x
    | x > 0 = x
    | otherwise = (- x)

asal [] = 0
asal (x:[]) = x+1
asal (x:y:[]) = x+3+y+3
asal [a,b,c] = a+2+b+2+c+2
asal xs = sum xs

tambah1 a b = a + b
tambah2 [a,b] = a + b
tambah3 (a,b) = a + b

-- rekursif

null' [] = True
null' _ = False

drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

kuadratinSemua [] = []
kuadratinSemua (x:xs) = (x*x) : (kuadratinSemua xs)

pangkatTigainSemua [] = []
pangkatTigainSemua (x:xs) = x*x*x : pangkatTigainSemua xs

semuain f [] = []
semuain f (x:xs) = f x : semuain f xs


-- kenapa intercalate nggak kebaca di haskell?

-- drop' 2 [1,2,3,4,5]
-- 1> drop' 1 [2,3,4,5]
-- 1> drop' 0 [3,4,5]
-- 1> drop' -1 [4,5]
-- 1> drop' -2 [5]
-- 1> drop' -3 []
-- 3>







-- kenapa nggak koma?
-- nggak papa beda da? satunya tuple satunya List
































































-- save
