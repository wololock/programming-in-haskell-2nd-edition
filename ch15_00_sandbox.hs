-- Ex. 4: Using a list comprehension, define an expression fibs
-- the infinite sequence of Fibonacci numbers

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]
