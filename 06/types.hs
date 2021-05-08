import Prelude hiding (Left, Right, Maybe, Nothing, Just)

-- type declarations

-- type String = [Char]

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x - 1, y)

-- type Trans = Pos -> Pos

-- left :: Trans
-- left (x, y) = (x - 1, y)


type Pair a = (a, a)

mult :: Pair Int -> Int
mult (a, b) = a * b

copy :: Int -> Pair Int
copy a = (a, a)


type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k xs = head [v | (k', v) <- xs, k == k']
-- find 2 [(1, 'a'), (2, 'c'), (3, 'f')] -- 'c'


-- data declarations

-- data Bool = False | True

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown


data Move = Left | Right | Up | Down

move :: Move -> Pos -> Pos
move Up (x, y) = (x, y - 1)
move Left (x, y) = (x - 1, y)
move Down (x, y) = (x, y + 1)
move Right (x, y) = (x + 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)
-- move Left (1, 1) -- (0, 1)
-- moves [Left, Right, Up, Down, Left] (0, 0) -- (-1, 0)


data Shape = Circle Float
           | Rect Float Float
-- Circle :: Float -> Shape
-- Rect :: Float Float -> Shape

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y


data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x


-- recursive types
data Nat = Zero | Succ Nat
-- Zero :: Nat
-- Succ :: Nat -> Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))
-- nat2int (int2nat 10) -- 10

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) s = Succ (add n s)
-- nat2int (add (int2nat 2) (int2nat 3)) -- 5


data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons h t) = 1 + len t
-- len (Cons 4 (Cons 3 Nil)) -- 2
-- len Nil -- 0


-- arithmetic expressions
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add l r) = size l + size r
size (Mul l r) = size l + size r

eval :: Expr -> Int
eval (Val n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
-- eval (Add (Val 3) (Val 2)) -- 5
-- size (Add (Val 3) (Val 2)) -- 2


-- binary tree
data Tree = Leaf Int
          | Node Tree Int Tree
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
-- data Tree a = Node a [Tree a]

occurs :: Int -> Tree -> Bool
occurs n (Leaf k) = n == k
occurs n (Node l k r) = (n == k)
                        || occurs n l
                        || occurs n r
-- occurs 3 (Node (Leaf 3) 4 (Leaf 5)) -- True
-- occurs 6 (Node (Leaf 3) 4 (Leaf 5)) -- False

flatten :: Tree -> [Int]
flatten (Leaf k) = [k]
flatten (Node l k r) = flatten l ++ [k] ++ flatten r
-- flatten (Node (Leaf 3) 4 (Leaf 5)) -- [3, 4, 5]
-- flatten (Node (Leaf 6) 4 (Leaf 7)) -- [6, 4, 7]

-- occurs for search-tree
occurs' :: Int -> Tree -> Bool
occurs' n (Leaf k) = k == n
occurs' n (Node l k r) | n == k = True
                       | n < k = occurs' n l
                       | otherwise = occurs' n r
-- occurs' 3 (Node (Leaf 3) 4 (Leaf 5)) -- True
-- occurs' 5 (Node (Leaf 3) 4 (Leaf 5)) -- True