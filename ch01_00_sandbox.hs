module Chapter_01 where

-- sum function definition
my_sum [] = 0
my_sum (n:ns) = n + my_sum ns


test xs x = left ++ [0,0,x,0,0] ++ right
            where
              left = [a | a <- xs, a < x]
              right = [b | b <- xs, b > x]

-- filtering function example
-- e.g. 
-- my_filter (\x -> x > 0) [-1,2,3,0] == [2,3]
-- my_filter (> 0) [-1,2,3,0] == [2,3] 
my_filter f xs = list
                 where
                   list = [a | a <- xs, f(a) == True]

-- local definition `list` is obsolate
my_filter' f xs = [a | a <- xs, f(a) == True]

-- quicksort algorithm
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger 
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

-- Computation step by step:
--
-- qsort [1,4,3,2]
-- qsort [] ++ [1] ++ qsort [4,3,2]
-- [] ++ [1] ++ (qsort [3,2] ++ [4] ++ qsort [])
-- [1] ++ ((qsort [2] ++ [3] ++ qsort []) ++ [4] ++ [])
-- [1] ++ (((qsort [] ++ [2] ++ qsort []) ++ [3] ++ []) ++ [4])
-- [1] ++ ((([] ++ [2] ++ []) ++ [3]) ++ [4])
-- [1] ++ (([2] ++ [3]) ++ [4])
-- [1] ++ ([2,3] ++ [4])
-- [1] ++ [2,3,4]
-- [1,2,3,4]
-- 

-- seqn :: Monad m => [m t] -> m [t]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

