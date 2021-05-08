import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

nat2int :: Nat -> Integer
nat2int = \n -> genericLength [c | c <- show n, c == 'S']

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- tree
data Tree = Leaf Integer
          | Node Tree Tree

leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1
                        && balanced l && balanced r

halve xs = splitAt (length xs `div` 2) xs

balance :: [Integer] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
                where (ys, zs) = halve xs