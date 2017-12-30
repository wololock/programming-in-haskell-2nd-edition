-- Solutions to exercises from Chapter 01
module Chapter_01 where

product' [] = 1
product' (x:xs) = x * product xs

-- reversed quicksort
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where
                  larger = [a | a <- xs, a > x]
                  smaller = [b | b <- xs, b <= x]

-- This variation of qsort removes duplicated values from final list
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]