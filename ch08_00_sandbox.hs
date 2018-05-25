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
