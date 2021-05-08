import Control.Applicative
import Control.Monad

-- state monad
type State = Int
data ST a = S (State -> (a, State))

apply :: ST a -> State -> (a, State)
apply (S f) x = f x

instance Functor ST where
    fmap = liftM

instance Applicative ST where
    pure = return
    (<*>) = ap

instance Monad ST where
    return x = S (\s -> (x, s))
    st >>= f = S (\s -> let (x, s') = apply st s in apply (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
-- example
tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree (a, Int))
mlabel (Leaf x) = do n <- fresh
                     return (Leaf (x, n))
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

label :: Tree a -> Tree (a, Int)
label t = fst (apply (mlabel t) 0)

-- label tree -- Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2))