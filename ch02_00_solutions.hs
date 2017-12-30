module Chapter_02 where

-- Fixes: lower case n instead of N, fixed spacing of local definitions
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

-- recursive alternative for `last` function
last' [x] = x
last' (x:xs) = last' xs

-- another approach
last'' xs = head (reverse xs)

-- an alternative for `init` function 
init' [x] = []
init' (x:xs) = [x] ++ init' xs

init'' xs = reverse (tail (reverse xs))