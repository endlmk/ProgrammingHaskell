module Interactive where

import FunctionalParser (expr, parse)
import System.IO (hSetEcho, stdin)


echo :: IO()
echo = do
    c <- getChar
    putChar '\n'
    putChar c
    putChar '\n'

getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do
            xs <- getLine'
            return (x:xs)

putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

putStrLn' :: String -> IO()
putStrLn' xs = do
    putStr' xs
    putChar '\n'

-- exercise 1

--readLine :: IO String
--readLine = readLine' []
--readLine' :: String -> IO String
--readLine' xs = do
--    x <- getChar
--    case x of
--        '\n' -> do
--            putChar '\n'
--            return (reverse xs)
--        '\DEL' -> do
--            Interactive.putStr "\ESC[1D\ESC[1D"
--            readLine' (tail xs)
--        _ -> do
--            putChar x
--            readLine' (x:xs)

readLine :: IO String
readLine = readLoop ""

readLoop :: String -> IO String
readLoop xs = do x <- getChar
                 case x of
                   '\n'   -> return xs
                   '\DEL' -> if null xs
                                then do del 2
                                        readLoop xs
                                else do del 3
                                        readLoop (init xs)
                   _      -> readLoop (xs ++ [x])
              where
                del n = putStr' (concat (replicate n "\ESC[1D \ESC[1D"))

beep :: IO()
beep = putStr' "\BEL"

cls :: IO()
cls = putStr' "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO()
goto (x, y) = putStr' ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos ->String -> IO()
writeat p xs = do goto p
                  putStr' xs

seqn :: [IO a] -> IO()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as


box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]


buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO()
display xs = do writeat (3, 2) "             "
                writeat (3, 2) (reverse (take 13 (reverse xs)))

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

calc :: String -> IO()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                process c xs
                else
                  do beep
                     calc xs

process :: Char -> String -> IO()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO()
quit = goto(1, 14)

delete :: String -> IO()
delete "" = calc ""
delete xs = calc (init xs)

showerror :: String -> IO()
showerror xs = do writeat (2, 14) xs

eval :: String -> IO()
eval xs = case parse expr xs of
            [(n, "")] -> calc (show n)
            [(n, rst)] -> do showerror rst
                             calc xs
            _  -> do beep
                     calc xs

clear :: IO()
clear = calc ""

press :: Char -> String -> IO()
press c xs = calc (xs ++ [c])

run :: IO()
run = do cls
         showbox
         clear

-- lifegame
width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

showcells :: Board -> IO()
showcells b = seqn [writeat p "o" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                           (x + 1, y - 1), (x - 1, y),
                           (x + 1, y), (x - 1, y + 1),
                           (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO()
life b = do cls
            showcells b
            wait 1000000
            life (nextgen b)

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

-- exercise 3
life' :: Board -> IO()
life' b = do cls
             lifewithupdate [] b

lifewithupdate :: Board -> Board -> IO()
lifewithupdate prevb b = do updatecells prevb b
                            wait 1000000
                            lifewithupdate b (nextgen b)

updatecells :: Board -> Board -> IO()
updatecells prevb b = do seqn [writeat p "o" | p <- b, not (elem p prevb)]
                         seqn [writeat p " " | p <- prevb, not (elem p b)]


-- exercise 6
type NimBoard = [Int]

showstars :: NimBoard -> IO()
showstars b = seqn [putStrLn (show id ++ ":" ++ take num (repeat '*'))  | (id, num) <- zip [1..] b ]

getNat :: IO(Int)
getNat = do n <- getLine
            return (read n)

takestars :: NimBoard -> Int -> Int -> NimBoard
takestars b i n = let (x, (y:ys)) = splitAt (i - 1) b in x ++ [y - n] ++ ys

runNim :: NimBoard -> IO()
runNim b = do cls
              showstars b
              putStrLn "Select ID:"
              i <- getNat
              putStrLn "Select Stars:"
              n <- getNat
              let bn = takestars b i n
              if (any (/= 0) bn)
                  then runNim bn
                  else return()
