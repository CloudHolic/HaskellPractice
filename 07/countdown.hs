-- the countdown problem
data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int
          | App Op Expr Expr

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- formalizing the problem
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where yss = subs xs
-- subs [1, 2] -> [[], [1], [2], [1, 2]]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- interleave 1 [2, 3] -> [[1, 2, 3], [2, 1, 3], [2, 3, 1]]

perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concat (map (interleave x) (perm xs))
-- perm [1, 2, 3] -> [[1, 2, 3], [1, 3, 2], [2, 3, 1], ...]

choices :: [a] -> [[a]]
choices xs = concat (map (perm) (subs xs))
-- choices [1, 2] -> [[], [1], [2], [1, 2], [2, 1]]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]


-- brute force
split :: [a] -> [([a], [a])]
split xs = [splitAt i xs | i <- [1..(n - 1)]]
            where n = length xs

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

bSolutions :: [Int] -> Int -> [Expr]
bSolutions ns n = [e | ns' <- choices ns,
                       e <- exprs ns',
                       eval e == [n]]


-- fast version 1
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div], valid o x y]

fSolutions :: [Int] -> Int -> [Expr]
fSolutions ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns',
                       m == n]


-- fast version 2
valid2 :: Op -> Int -> Int -> Bool
valid2 Add x y = x <= y
valid2 Sub x y = x > y
valid2 Mul x y = x <= y && x /= 1 && y /= 1
valid2 Div x y = x `mod` y == 0 && y /= 1

eval2 :: Expr -> [Int]
eval2 (Val n) = [n | n > 0]
eval2 (App o l r) = [apply o x y | x <- eval l,
                                   y <- eval r,
                                   valid2 o x y]

results2 :: [Int] -> [Result]
results2 [] = []
results2 [n] = [(Val n, n) | n > 0]
results2 ns = [res | (ls, rs) <- split ns,
                     lx <- results2 ls,
                     ry <- results2 rs,
                     res <- combine2' lx ry]

combine2' :: Result -> Result -> [Result]
combine2' (l, x) (r, y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div], valid2 o x y]

f2Solutions :: [Int] -> Int -> [Expr]
f2Solutions ns n = [e | ns' <- choices ns,
                        (e, m) <- results2 ns',
                        m == n]