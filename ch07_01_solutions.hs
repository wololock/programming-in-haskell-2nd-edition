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