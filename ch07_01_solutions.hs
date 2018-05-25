module Chapter_07 where

-- Ex. 1
-- [f x | x <- xs, p x]
ex1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
ex1 p f = map f . filter p

-- Ex. 2

-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x y -> y && p x) True

-- b.
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x y -> y || p x) False

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : (takeWhile' p xs)
                    | otherwise = []

-- d.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- Ex. 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Ex. 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- dec2int [2,4,3,1] == 2431
-- f = (\x y -> 10 * x + y)
-- foldl f 0 [2,4,3,1]
-- foldl f (f 0 2) [4,3,1]
-- foldl f (f (f 0 2) 4) [3,1]
-- foldl f (f (f (f 0 2) 4) 3) [1]
-- foldl f (f (f (f (f 0 2) 4) 3) 1) []
-- (f (f (f (f 0 2) 4) 3) 1)
-- (f (f (f (10 * 0 + 2) 4) 3) 1)
-- (f (f (f 2 4) 3) 1)
-- (f (f (10 * 2 + 4) 3) 1)
-- (f (f 24 3) 1)
-- (f (10 * 24 + 3) 1)
-- (f 243 1)
-- (10 * 243 + 1)
-- 2431


 -- Ex. 5
add :: Num a => (a,a) -> a
add (x,y) = x + y 

add' :: Num a => a -> a -> a
add' x y = x + y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (a,b) = f a b

-- Ex. 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- map'' (*2) [1,2,3,4] ==> [2,4,6,8]
map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

-- take 5 $ iterate'' (+1) 0 ==> [0,1,2,3,4]
iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfold (\x -> False) id f

-- Ex. 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f h = map (\x -> if even (fst x) then f (snd x) else h (snd x)) . zip (iterate (+1) 0) 

-- > altMap (+10) (+100) [5,4,3,2,1]
-- > [15,104,13,102,11]

-- Ex. 10

luhnDouble :: Int -> Int
luhnDouble x = if x > 4 then (x * 2) - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

luhn' :: [Int] -> Bool
luhn' xs = (mod (sum $ altMap luhnDouble id xs) 10) == 0
