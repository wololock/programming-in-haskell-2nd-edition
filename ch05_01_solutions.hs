module Chapter_05 where

import Data.Char

-- Ex. 1
first_hundred_squares = sum [x^2 | x <- [1..100]]

-- Ex. 2
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Ex. 3
-- > square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Ex. 4
replicate' :: Int -> a -> [a]
replicate' n v = [v | _ <- [1..n]]

-- Ex. 5
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], 
                     y <- [1..n],
                     z <- [1..n], 
                     x^2 + y^2 == z^2]

-- Ex. 6
-- > perfects 500
-- [6,28,496]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | (x,y) <- pairs, x == y]
             where
               pairs = [(x, sum (factors x) - x) | x <- [1..n]]

-- Ex. 7
list = concat [[(x,3),(x,4)] | x <- [1,2]]

-- Ex. 8
-- > find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)]
-- [2,4]
find :: Eq a => a -> [(a,b)] -> [b]
find k ps = [v | (k',v) <- ps, k == k']

-- positions False [True, False, True, False]
-- [1,3]
positions :: Eq a => a -> [a] -> [Int]
positions v vs = find v (zip vs [0..])

-- Ex. 9
-- > scalarproduct [1,2,3] [4,5,6]
-- 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
