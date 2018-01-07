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

