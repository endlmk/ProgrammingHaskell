module ListComprehension where

import Data.Char

-- exercise 1
-- sum [i ^ 2 | i <- [1..100]]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <-[1..n], x^2 + y^2 == z^2 ]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], m == sum [s | s <- [1..(m - 1)], (m `mod` s) == 0 ] ]

--concat [[(x, y) | y<-[4,5,6]] | x<-[1,2,3]]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions k xs = [i | i <- find k (zip xs [0..(length xs - 1)])]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [xi * yi | (xi, yi) <- zip x y]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

letU2int :: Char -> Int
letU2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2letU ((letU2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]
