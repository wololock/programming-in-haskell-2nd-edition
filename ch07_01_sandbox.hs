module Chapter_07 where

import Data.Char
import Data.List

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

-- Function composition

odd' :: Int -> Bool
odd' x = not (even x)

odd'' :: Int -> Bool
odd'' = not . even

twice :: (a -> a) -> a -> a
twice f x = f (f x)

twice' :: (a -> a) -> a -> a
twice' f = f . f

sumsqreven' :: [Int] -> Int
sumsqreven' = sum . map (^2) . filter even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- Binary String Transmitter example

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin n | divider > 0 = renainder : (int2bin divider)
          | otherwise = [renainder]
          where
            divider = div n 2
            renainder = mod n 2

int2bin' :: Int -> [Bit]
int2bin' 0 = []
int2bin' n = n `mod` 2 : int2bin' (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- Voting alghorithms

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups $ filter (/= x) xs)

votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

