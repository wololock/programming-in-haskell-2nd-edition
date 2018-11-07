module Chapter_10_Sandbox where

import System.IO
import Data.List
import Data.Char

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr' "Enter a string: "
            xs <- getLine'
            putStr' "The string has "
            putStr' (show (length xs))
            putStrLn' " characters."

-- Hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word []

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> String -> IO ()
play word letters = do putStr "?: "
                       guess <- getLine
                       let ls = nub $ guess ++ letters
                       if guess == word then
                         putStrLn "You got it!"
                       else                       
                         do putStrLn (match word ls)
                            play word ls


match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


-- Game of nim example

next :: Int -> Int 
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                     where
                       update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "⏺"))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "ERROR: Invalid digit"
                          getDigit prompt

newline :: IO ()
newline = putChar '\n'

nim :: Board -> Int -> IO ()
nim board player = do newline
                      putBoard board
                      if finished board then
                        do newline
                           putStr "Player "
                           putStr (show (next player))
                           putStrLn " wins!"
                      else
                        do newline
                           putStr "Player "
                           putStrLn (show player)
                           row <- getDigit "Enter a row number: "
                           num <- getDigit "Stars to remove: "
                           if valid board row num then 
                             nim (move board row num) (next player)
                           else 
                             do newline
                                putStrLn "ERROR: Invalid move"
                                nim board player


-- Game Of Life

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 30

height :: Int
height = 30

type Board' = [Pos]

glider :: Board'
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showCells :: Board' -> IO ()
showCells b = sequence_ [writeAt p "█" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1),
                          (x+1, y-1), (x-1, y),
                          (x+1, y), (x-1, y+1),
                          (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), 
                isEmpty b p, 
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board' -> IO ()
life b = do cls
            showCells b
            wait 2000
            life (nextgen b)

b1 :: Board'
b1 = [(1,1), (5,4), (12,3), (12,4), (2,3), (5,5), (6,7), (5,4),(4,5), (12,4), (11,3), (11,4), (8,8), (7,8), (8,9), (7,9), (7,7), (6,7), (6,6), (5,6)]

-- Ex. 1
-- Redefine putStr :: String -> IO () using a list comprehension and the library
-- function sequence_ :: [IO a] -> IO () .

putStr'' :: String -> IO ()
putStr'' xs = sequence_ [putChar x | x <- xs]

-- Ex. 2
-- Using recursion, define a version of putBoard :: Board -> IO () that displays
-- nim boards of any size, rather than being specific to boards with just five rows of
-- stars. Hint: first define an auxiliary function that takes the current row number as an
-- additional argument.

putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow n x | (n,x) <- zip [1..] xs]

-- Ex. 4
-- Define an action adder :: IO () that reads a given number of integers from the
-- keyboard, one per line, and displays their sum.

readNumber :: IO (Int)
readNumber = do n <- getLine
                return (read n :: Int)

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- readNumber
           xs <- sequence [readNumber | _ <- [1..n]]
           putStrLn $ "The total is " ++ (show $ sum xs) 
          

           
           
           


