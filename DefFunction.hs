module DefFunction where
import Control.Exception (assert)

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True |||| _ = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c | b == c = b
          | otherwise = True

(&&) :: Bool -> Bool -> Bool
b && c = if b then
            if c then
            True else False
         else False

(&&&) :: Bool -> Bool -> Bool
b &&& c = if b then c else False

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))
