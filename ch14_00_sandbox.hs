module Chapter_14_Sandbox where

import Data.Monoid
import Data.Foldable

-- instance Monoid a => Monoid (Maybe a) where
--     -- mempty :: Maybe a
--     mempty = Nothing

--     -- mappend :: Maybe a -> Maybe a -> Maybe a
--     Nothing `mappend` my    = my
--     mx `mappend` Nothing    = mx
--     Just x `mappend` Just y = Just (x `mappend` y)


-- -- Integer as monoid under addition
-- instance Monoid Int where
--     -- mempty :: Int
--     mempty = 0

--     -- mappend :: Int -> Int -> Int
--     mappend = (+)

-- -- Integer as monoid under multiplication
-- instance Monoid Int where
--     -- mempty :: Int
--     mempty = 1

--     -- mappend :: Int -> Int -> Int 
--     mappend = (*)


s1 :: Sum Int
s1 = mconcat [Sum 2, Sum 3, Sum 4]

s2 :: Sum Int
s2 = Sum 2 <> Sum 3 <> Sum 4

p1 :: Product Int
p1 = mconcat [Product 2, Product 3, Product 4]

p2 :: Product Int
p2 = Product 2 <> Product 3 <> Product 4

b1 :: All
b1 = mconcat [All True, All True, All True]

b2 :: Any
b2 = mconcat [Any False, Any False, Any True]

-- 14.2 Foldables

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

t1 :: Tree Char
t1 = Node (Node (Leaf 'a') (Node (Leaf 'b') (Node (Leaf 'c') (Leaf 'd')))) (Node (Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g'))) (Leaf 'h'))

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x)   = x
    fold (Node l r) = fold l <> fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x)   = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v (Leaf x)   = f x v
    foldr f v (Node l r) = foldr f (foldr f v r) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf x)   = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r


ex1 :: Int
ex1 = getSum (foldMap Sum [1..10])

ex2 :: Int
ex2 = getProduct (foldMap Product [1..10])

t2 :: Tree Int
t2 = Node (Node (Leaf 1) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 2))) (Node (Node (Leaf 6) (Node (Node (Leaf 8) (Leaf 9)) (Leaf 7))) (Leaf 5))

