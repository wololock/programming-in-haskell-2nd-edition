module Chapter_06 where

-- Ex. 1
fac'' :: Int -> Int
fac'' n | n == 0 = 1
        | n > 0 = n * fac'' (n-1)
        | otherwise = 0

-- Ex. 2
sumdown :: Int -> Int
sumdown n | n > 0 = n + sumdown (n-1)
          | otherwise = 0

