module Chapter_08_Sandbox where

-- Creating a new type as an alias for an existing type
type String = [Char]

-- A new type may pair existing types
type Pos = (Int,Int)

-- This is an example of transformation type that transforms 
-- one position to another
type Trans = Pos -> Pos

-- Types can be parameterized, e.g.
type Pair a = (a,a)

-- And we can use more than one paramter, e.g.
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']


-- 8.2 Data declarations

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West


-- The constructors in a data declaration can also have an arguments
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `mod` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3 New type declarations

newtype Nat = N Int 

-- 8.4 Recursive types

data Nat' = Zero | Succ Nat'

nat2int :: Nat' -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat'
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

addNat :: Nat' -> Nat' -> Nat'
addNat n m = int2nat ((nat2int n) + (nat2int m))

addNat' :: Nat' -> Nat' -> Nat'
addNat' Zero n     = n
addNat' (Succ m) n = Succ (addNat' m n)

-- Let's define our own version of built-in type List
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- Let's define Tree type structure
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)    = x == y
occurs x (Node a y b) = x == y || occurs x a || occurs x b

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node a x b) = flatten a ++ [x] ++ flatten b
