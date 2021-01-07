module DefTypeAndClass where

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- exercise 1
mul :: Nat -> Nat -> Nat
mul Zero n = Zero
mul (Succ m) n = add n (mul m n)

data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

occursOrd :: Int -> Tree -> Bool
occursOrd m (Leaf n) = m == n
occursOrd m (Node l n r)
  | m == n = True
  | m < n = occursOrd m l
  | otherwise = occursOrd m r

-- exercise 2
occursOrd' :: Int -> Tree -> Bool
occursOrd' m (Leaf n) = m == n
occursOrd' m (Node l n r) = case compare m n of
  EQ -> True
  LT -> occursOrd' m l
  GT -> occursOrd' m r

-- exercise 3
data Tree' = Leaf' Int | Node' Tree' Tree' deriving Show

leafnum :: Tree' -> Int
leafnum (Leaf' n) = 1
leafnum (Node' l r) = leafnum l + leafnum r

balanced :: Tree' -> Bool
balanced (Leaf' n) = True
balanced (Node' l r) = (abs (leafnum l - leafnum r) <= 1) && (balanced l) && (balanced r)

-- exercise 4
splithalf :: [Int] -> ([Int], [Int])
splithalf xs = splitAt center xs
               where center = length xs `div` 2

balance :: [Int] -> Tree'
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r)
             where (l, r) = splithalf xs

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop

type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
--bools n = [ items ++ [True] | items <- bools (n - 1)]
--       ++ [ items ++ [False] | items <- bools (n - 1)]
bools n = map (True:) bss ++ map (False:) bss
          where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVALA Expr | EVALM Expr | ADD Int | MULT Int

evalExpr :: Expr -> Cont -> Int
evalExpr (Val n) c = exec c n
evalExpr (Add x y) c = evalExpr x (EVALA y : c)
evalExpr (Mult x y) c = evalExpr x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y:c) n = evalExpr y (ADD n:c)
exec (EVALM y:c) n = evalExpr y (MULT n:c)
exec (ADD n:c) m = exec c (n + m)
exec (MULT n:c) m = exec c (n * m)

value :: Expr -> Int
value e = evalExpr e []

-- exercise 8
--data Maybe a = Just a | Nothing
--instance Monad Maybe where
--  return = Just
--  (Just x) >>= f = f x
--  Nothing >>= _ = Nothing

--data [] a = [] | a:([]a)
--instance Monad [] where
--  return v = [v]
--  [v] >>= g = g v
--  [] >>= _ = []