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

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                 = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r


-- 8.6 Tautology checker

data Prop = Const Bool 
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop


p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = (Var 'A') `And` (Not (Var 'A'))

p6 :: Prop
p6 = (Var 'A' `And` (Var 'A' `Imply` Var 'B')) `Imply` Var 'B'

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
           where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups $ filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 8.7 Abstract machine

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int 
exec [] n           = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m)

value' :: Expr -> Int
value' e = eval' e []
 