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


