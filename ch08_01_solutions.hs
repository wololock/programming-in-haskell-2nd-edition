module Chapter_08_Solutions where

-- Ex. 1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n        = Zero
mult (Succ Zero) n = n
mult (Succ m) n    = add (mult m n) n

nat1 = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
nat2 = Succ (Succ (Succ Zero))

-- Ex. 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

-- Ex. 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

t2 = Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3 ) (Leaf' 4))
t3 = Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))) (Leaf' 1)
t4 = Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))) (Node' (Node' (Leaf' 5) (Leaf' 6)) (Node' (Leaf' 7) (Leaf' 8)))
t5 = Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))) (Node' (Node' (Leaf' 5) (Leaf' 6)) (Leaf' 7))

balanced :: Tree' a -> Bool
balanced (Leaf' _)                   = True
balanced (Node' (Leaf' _) (Leaf' _)) = True
balanced (Node' a b)                 = abs ((leaves a) - (leaves b)) <= 1

leaves :: Tree' a -> Int
leaves (Leaf' _)   = 1
leaves (Node' a b) = leaves a + leaves b
