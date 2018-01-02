module Chapter_06 where

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n-1)

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop (n-1) xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)