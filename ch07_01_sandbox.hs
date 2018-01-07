module Chapter_07 where

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x:xs) | p x = x : filter p xs
                  | otherwise = filter p xs

sumsqreven :: [Int] -> Int
sumsqreven xs = sum (map (^2) (filter even xs))

-- Defining functions with foldr

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- Defining recursive foldr'
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = x `f` foldr' f v xs -- f x (foldr' f v xs)

-- foldr' (+) 0 [1,2]
-- (+ 1 (foldr' (+) 0 [2]))
-- (+ 1 (+ 2 + (foldr' (+) 0 [])))
-- (+ 1 (+ 2 + (0)))

length' :: [a] -> Int
length' = foldr (\_ n -> n+1) 0

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x:xs) = foldl f (f v x) xs

-- foldl' (+) 0 [1,2]
-- (+ (foldl' (+) 0 [1]) 2)
-- (+ (+ (foldl' (+) 0 []) 1) 2)
-- (+ (+ 0 1) 2)