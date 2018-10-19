module Chapter_09_Sandbox where

-- The coundown problem

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


-- Check if application of an operator to two positive natural numbers
-- gives another positive natural number
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y



