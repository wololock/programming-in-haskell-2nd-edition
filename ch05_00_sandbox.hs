module Chapter_05 where

import Data.Char

-- [1,4,9,16,25]
list1 = [x^2 | x <- [1..5]]

-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
list2 = [(x,y) | x <- [1,2,3], y <- [4,5]]

firsts :: [(a,b)] -> [a] 
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

list3 = [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- > find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)]
-- [2,4]
find :: Eq a => a -> [(a,b)] -> [b]
find k ps = [v | (k',v) <- ps, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- positions False [True, False, True, False]
-- [1,3]
positions :: Eq a => a -> [a] -> [Int]
positions v vs = [p | (v',p) <- zip vs [0..], v' == v]

lowers :: String -> Int
lowers str = length [c | c <- str, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c str = length [c' | c' <- str, c' == c]

-- --------------------------------
-- Caesar cipher
-- --------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n str = [shift n c | c <- str]



table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
                 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
                 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where
             n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs