-- abstract machine

-- expression
data Expr = Val Int
          | Add Expr Expr

-- control stack
type Cont = [Op]
data Op = EVAL Expr
        | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n -- eval n
eval (Add x y) c = eval x (EVAL y : c) -- eval x before y

exec :: Cont -> Int -> Int
exec [] n = nat
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value :: Expr -> Int
value e = eval e []