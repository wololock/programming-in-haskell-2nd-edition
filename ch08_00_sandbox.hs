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
