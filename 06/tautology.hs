-- tautology checker
data Prop = Const Bool
          | Var Char
          | Not Prop
          | Or Prop Prop
          | And Prop Prop
          | Imply Prop Prop

-- A and ~A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- A and B -> A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A -> A and B
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A and (A -> B)) -> B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (Or p1 p2) = eval s p1 || eval s p2
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Imply p1 p2) = eval s p2


vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2
-- vars p1 -- "AA"
-- vars p2 -- "AA"
-- vars p3 -- "ABA"
-- vars p4 -- "AABB"

uniq :: Eq a => [a] -> [a]
uniq = foldr (\x xs -> if elem x xs then xs else x:xs) []
-- uniq (vars p4) -- "AB"

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) prev ++ map (True:) prev
            where prev = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
                where vs = uniq (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]