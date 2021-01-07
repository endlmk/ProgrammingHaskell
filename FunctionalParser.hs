module FunctionalParser where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    fmap = liftM
instance Applicative Parser where
    pure v = P (\inp -> [(v, inp)])
    (<*>) = ap

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v, out)] -> parse (f v) out)

failure :: Parser a
failure = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

p :: Parser(Char, Char)
p = do
    x <- item
    item
    y <- item
    return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <-item
    if p x then return x else failure

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
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
    v <- p
    vs <- many p
    return (v:vs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- many1 digit
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Exercise 1
int :: Parser Int
int = do
    char '-'
    v <- nat
    return (-v)
    +++ nat

-- Exercise 2
comment :: Parser ()
comment = do
    string "-- "
    many (sat (/='\n'))
    char '\n'
    return ()

-- Exercise 6
expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
        +++ do
        symbol "-"
        e <- expr
        return (t - e)
        +++ return t

-- Exercise 8
-- 無限再帰！
{-expr :: Parser Int
expr = do
    e <- expr
    do
        symbol "-"
        t <- term
        return (e - t)
        +++ return e-}

--expr_minus :: Parser Int
--expr_minus = do
--    symbol "-"
--    f <- factor
--    return f
--
--expr :: Parser Int
--expr = do
--    f <- factor
--    xs <- many expr_minus
--    return (foldl (-) f xs)

term :: Parser Int
term = do
    p <- powTerm
    do
        symbol "*"
        t <- term
        return (p * t)
        +++ do
        symbol "/"
        t <- term
        return (p `div` t)
        +++ return p

powTerm :: Parser Int
powTerm = do
    f <- factor
    do
        symbol "^"
        p <- powTerm
        return (f ^ p)
        +++ return f

factor :: Parser Int
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return e
    +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
    [(n, [])] -> n
    [(_, out)] -> error ("unused input " ++ out)
    [] -> error "invalid input"

-- for left associative operation

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op =  do a <- p
                  rest a
                  where rest a = do f <- op
                                    b <- p
                                    rest (f a b)
                                 +++ return a

expr' :: Parser Int
expr' = term' `chainl` addop

term' :: Parser Int
term' = factor `chainl` mulop

addop :: Parser (Int -> Int -> Int)
addop = do symbol "+"
           return (+)
        +++ do symbol "-"
               return (-)

mulop :: Parser (Int -> Int -> Int)
mulop = do symbol "*"
           return (*)
        +++ do symbol "/"
               return (div)

eval' :: String -> Int
eval' xs = case parse expr' xs of
    [(n, [])] -> n
    [(_, out)] -> error ("unused input " ++ out)
    [] -> error "invalid input"
