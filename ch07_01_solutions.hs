module Chapter_07 where

-- Ex. 1
-- [f x | x <- xs, p x]
ex1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
ex1 = map . filter

