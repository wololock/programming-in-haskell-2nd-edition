module Chapter_04 where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1/n

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
                     if n == 0 then 0 else 1

-- Guarded equations examples

abs'' n | n >= 0 = n
        | otherwise = -n

signum'' n | n > 0 = 1
           | n == 0 = 0
           | otherwise = -1

-- Pattern matching

not' :: Bool -> Bool
not' False = True
not' True = False

fst' :: (a,b) -> a
fst' (x,_) = x

snd':: (a,b) -> b
snd' (_,y) = y
