module Chapter_04 where

-- Ex. 1
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where
             half = (length xs) `div` 2

-- Ex. 2
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x

-- Ex. 3
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs = []
              | otherwise = tail xs

safetail''' :: [a] -> [a]
safetail''' [] = []
safetail''' xs = tail xs

-- Ex. 8
luhnDouble :: Int -> Int
luhnDouble x = if x > 4 then (x * 2) - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0