import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

natToInteger :: Nat -> Integer
--natToInteger Zero = 0
--natToInteger (Succ n) = 1 + natToInteger n

--natToInteger = head . m
-- where m Zero = [0]
--       m (Succ n) = [sum [x | x <- (1 : m n)]]

-- counts number of 'S'
natToInteger = \ n -> genericLength [c | c <- show n, c == 'S']


integerToNat :: Integer -> Nat
--run n + k pattern via ':set -XNPlusKPatterns' on Prelude
--integerToNat 0 = Zero
--integerToNat (n + 1) = Succ (integerToNat n)

integerToNat (n + 1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero


-- natToInteger (add m n) = natToInteger m + natToInteger n
add :: Nat -> Nat -> Nat

add Zero n = n
add (Succ m) n = Succ (add n m)


mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)


data Tree = Leaf Integer 
          | Node Tree Integer Tree

occurs :: Integer -> Tree -> Bool
--occurs m (Leaf n) = m == n
--occurs m (Node l n r) = case compare m n of
--	LT -> occurs m l
--	EQ -> True
--	GT -> occurs m r

occurs m (Leaf n) = m == n
occurs m (Node l n r) 
  | m == n = True
  | m < n = occurs m l
  | otherwise = occurs m r

data Tree2 = Leaf Integer | Node Tree2 Tree2


balanced :: Tree -> Bool
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r


balance :: [Integer] -> Tree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance xs) (balance ys)
  where (yx, xs) = halve xs

