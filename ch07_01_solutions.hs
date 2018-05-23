module Chapter_07 where

-- Ex. 1
-- [f x | x <- xs, p x]
ex1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
ex1 p f = map f . filter p

-- Ex. 2

-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x y -> y && p x) True

-- b.
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x y -> y || p x) False

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : (takeWhile' p xs)
                    | otherwise = []

-- d.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- Ex. 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Ex. 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- dec2int [2,4,3,1] == 2431
-- f = (\x y -> 10 * x + y)
-- foldl f 0 [2,4,3,1]
-- foldl f (f 0 2) [4,3,1]
-- foldl f (f (f 0 2) 4) [3,1]
-- foldl f (f (f (f 0 2) 4) 3) [1]
-- foldl f (f (f (f (f 0 2) 4) 3) 1) []
-- (f (f (f (f 0 2) 4) 3) 1)
-- (f (f (f (10 * 0 + 2) 4) 3) 1)
-- (f (f (f 2 4) 3) 1)
-- (f (f (10 * 2 + 4) 3) 1)
-- (f (f 24 3) 1)
-- (f (10 * 24 + 3) 1)
-- (f 243 1)
-- (10 * 243 + 1)
-- 2431


 -- Ex. 5
add :: Num a => (a,a) -> a
add (x,y) = x + y 

add' :: Num a => a -> a -> a
add' x y = x + y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (a,b) = f a b