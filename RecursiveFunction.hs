module RecursiveFunction where

import Prelude hiding (last, take, sum, elem, (!!), replicate, concat, and, init, drop, length, (^))

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n -1))

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n - 1) xs

init :: [a] -> [a]
init (x:xs) | null xs  = []
            | otherwise = x:init xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:replicate (n - 1) x

(!!) :: [a] -> Int -> a
x !! 0 = head x
(x:xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem i (x:xs) = (x == i) || elem i xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = x : merge xs []
merge [] (y:ys) = y : merge [] ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x >= y = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort l) (msort r)
           where (l, r) = halve xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x:take (n - 1) xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs