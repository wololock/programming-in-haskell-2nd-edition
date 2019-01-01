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

testFoldableTree :: IO ()
testFoldableTree = do putStr "t2 == "
                      print t2
                      putStr "length t2 == "
                      print $ length t2
                      putStr "null t2 == "
                      print $ null t2
                      putStr "elem 3 t2 == "
                      print $ elem 3 t2
                      putStr "maximum t2 == "
                      print $ maximum t2
                      putStr "minimum t2 == "
                      print $ minimum t2
                      putStr "foldr1 (+) t2 == "
                      print $ foldr1 (+) t2
                      putStr "toList t2 == "
                      print $ toList t2

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- 14.3 Traversables

traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' g []     = pure []
traverse' g (x:xs) = pure (:) <*> g x <*> traverse' g xs

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x)   = pure Leaf <*> g x
    traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

instance Functor Tree where
    fmap g (Leaf x)   = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

testTravesableTree :: IO ()
testTravesableTree = do putStr "sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2))) == "
                        print $ sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2)))


-- Ex. 1
-- Complete the following instance declaration from Data.Monoid to make a pair type
-- into a monoid provided the two component types are monoids:

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--        -- mempty :: (a,b)
--        mempty = (mempty, mempty)
--
--        -- mappend :: (a,b) -> (a,b) -> (a,b)
--        (x1,y1) `mappend` (x2,y2) = (x1 <> x2, y1 <> y2)
