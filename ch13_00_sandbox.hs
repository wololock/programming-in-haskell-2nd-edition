module Chapter_13_Sandbox where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x, xs)])


instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

three :: Parser (Char, Char)
three = do x <- item
           item
           z <- item
           return (x,z)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of 
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- 13.6 Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace) 
           return ()

int :: Parser Int
int = do char '-' 
         n <- nat
         return (-n)
    <|> nat
    

token :: Parser a -> Parser a
token p = do space 
             v <- p
             space 
             return v

-- Using token , we can now define parsers that ignore spacing around identifiers, natural
-- numbers, integers and special symbols:

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- For example, using these primitives a parser for a non-empty list of natural numbers that
-- ignores spacing around tokens can be defined as follows:

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","; natural)
          symbol "]"
          return (n:ns)


