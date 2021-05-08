-- marking append vanish

-- reverse
reverse2 :: [a] -> [a]
reverse2 xs = reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)


-- flatten
data Tree = Leaf Int | Node Tree Tree

flatten2 :: Tree -> [Int]
flatten2 t = flatten' t []

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n:ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)


-- compiler correctness
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y


type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n:c) s = exec c (n:s)
exec (ADD:c) (m:n:s) = exec c (m + n:s)
-- exec (compiler' e c) s = exec c (eval e : s)

compile' :: Expr -> Code -> Code
compile' (Val n) c = PUSH n : c
compile' (Add x y) c = compile' x (compile' y (ADD : c))

compile :: Expr -> Code
compile e = compile' e []

instance Show Expr where
    show (Val n) = "(Val " ++ show n ++ ")"
    show (Add x y) = "(Add" ++ show x ++ " " ++ show y ++ ")"

instance Show Op where
    show (PUSH n) = "(PUSH " ++ show n ++ ")"
    show (ADD) = "(ADD)"

e :: Expr
e = (Add (Add (Val 2) (Val 3)) (Val 4))
-- e -- (Add (Add (Val 2) (Val 3)) (Val 4))
-- eval e -- 9
-- compile e -- [(PUSH 2), (PUSH 3), (ADD), (PUSH 4), (ADD)]