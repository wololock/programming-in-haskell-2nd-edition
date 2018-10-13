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

