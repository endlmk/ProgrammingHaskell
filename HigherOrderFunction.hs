module HigherOrderFunction where

import Prelude hiding (all, any, takeWhile, dropWhile)
import Data.Char

-- exercise 1
-- map f (filter p xs)
-- (map f . filter p) xs


all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x b -> (&&) (f x) b) True

any :: (a -> Bool) -> [a] -> Bool
any f = foldr ((||) . f) False

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x:(takeWhile p xs)
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = x:xs
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (pred p) []
            where pred p x | p x = (x:)
                           | otherwise = ([] ++)

dec2int :: [Int] -> Int
dec2int = foldl  (\x y -> 10 * x + y) 0

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) ->c)
uncurry f = \(p, q) -> f p q

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod`2) (`div`2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapf :: (a -> b) -> [a] -> [b]
mapf f = unfold null (f . head) tail

iteratef :: (a -> a) -> a -> [a]
iteratef f = unfold (\_ -> False) id f

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode =  map (chr . bin2int) . chop8

transmit :: String -> String
transmit =  decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

make9 :: [Bit] -> [Bit]
make9 bits = take 9 (bits ++ repeat 0)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

countone :: [Bit] -> Int
countone = length . filter (==1)

makeparity :: [Bit] -> Bit
makeparity bits = if odd (countone bits) then 1 else 0

chr2binp :: Char -> [Bit]
chr2binp c = makeparity bits : bits
             where bits = int2bin (ord c)

encodep :: String -> [Bit]
encodep = concat . map (make9 . chr2binp)

processp :: [Bit] -> [Bit]
processp bits = if head bits == 1
                then if odd ones
                     then tail bits
                     else error "Error!"
                else if even ones
                     then tail bits
                     else error "Error!"
                where ones = countone (tail bits)

decodep :: [Bit] -> String
decodep = map (chr . bin2int . processp) . chop9

transmitp :: String -> String
transmitp =  decodep . channelp . encodep

channelp :: [Bit] -> [Bit]
channelp = tail
