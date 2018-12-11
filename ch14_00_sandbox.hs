module Chapter_14_Sandbox where

import Data.Monoid

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


