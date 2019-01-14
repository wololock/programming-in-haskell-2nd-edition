-- Ex. 4: Using a list comprehension, define an expression fibs
-- the infinite sequence of Fibonacci numbers

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

-- Ex. 6 Newton’s method for computing the square root of a (non-negative) floating-point
-- number n can be expressed as follows:
--
--  * start with an initial approximation to the result;
--  * given the current approximation a , the next approximation is defined by the
--    function next a = (a + n/a) / 2 ;
--  * repeat the second step until the two most recent approximations are within some
--    desired distance of one another, at which point the most recent value is returned as
--    the result.

sqroot :: Double -> Double
sqroot = snd . head . dropWhile (\(x,y) -> abs (x - y) >= dx) . seq
    where
        dx        = 0.00001
        next n a  = (a + n/a) / 2
        approxs n = iterate (next n) 1.0
        seq n     = zip (approxs n) (drop 1 $ approxs n)
