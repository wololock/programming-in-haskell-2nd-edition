module Chapter_12_Sandbox where

inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

inc' :: [Int] -> [Int]
inc' = map (+1)

sqr' :: [Int] -> [Int]
sqr' = map (^2)

--class Functor f where
--    fmap :: (a -> b) -> f a -> f b    

--instance Functor [] where
--    fmap = map

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
    fmap g (Leaf x)   = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)


t1 = Node (Node (Leaf "abc") (Leaf "def")) (Leaf "hij")

inc'' :: Functor f => f Int -> f Int 
inc'' = fmap (+1)

-- 12.2 Applicatives

ap1 = pure (+1) <*> Just 3
ap2 = pure (map (+2)) <*> (Just [1,2,3,4])
ap3 = pure (zip) <*> (Just [1..]) <*> (Just ['a', 'b', 'c', 'd'])

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

-- 12.3 Monads

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n -> case eval' y of
                                Nothing -> Nothing
                                Just m -> n `safediv` m

-- Implementing eval in applicative style
--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--mx >>= f = case mx of
--             Nothing -> Nothing
--             Just x  -> f x

eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = eval'' x >>= \n -> eval'' y >>= \m -> n `safediv` m

eval''' ::Expr -> Maybe Int
eval''' (Val n)   = Just n
eval''' (Div x y) = do n <- eval''' x
                       m <- eval''' y
                       n `safediv` m

--instance Monad [] where
--    -- (>>=) :: [a] -> (a -> [b]) -> [b]
--    xs >>= f = [f x | x <- xs]

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

