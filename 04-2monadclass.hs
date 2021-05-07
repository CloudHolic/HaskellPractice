import Control.Applicative
import Control.Monad
import Data.Char
import Prelude hiding (Maybe, Just, Nothing)

-- monad
newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return v = P $ \inp -> [(v, inp)]
    p >>= f = P $ \inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> parse (f v) out

item :: Parser Char
item = P $ \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

ignore2 :: Parser (Char, Char)
ignore2 = do x <- item
             item
             z <- item
             return (x, z)
-- parse ignore2 "7A3BCEF" -- [(('7', '3'), "BCEF")]


-- monadplus
instance MonadPlus Parser where
    mzero = P $ \_ -> []
    p `mplus` q = P $ \inp -> case parse p inp of
                                [] -> parse q inp
                                [(v, out)] -> [(v, out)]

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

failure :: Parser Char
failure = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q
-- parse (item +++ return 'd') "abc" -- [('a', "bc")]
-- parse (item +++ return 'd') "" -- [('d', "")]


-- derived primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
-- parse (string "google") "naver google yahoo" -- []
-- parse (string "google") "google yahoo" -- [("google", " yahoo")]
-- parse (string "google") "goo yahoo" -- []

manyC :: Parser a -> Parser [a]
manyC p = many1C p +++ return []

many1C :: Parser a -> Parser [a]
many1C p = do x <- p
              xs <- manyC p
              return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- manyC alphanum
           return (x:xs)
-- parse ident "left = 3" -- [("left", " = 3")]

nat :: Parser Int
nat = do xs <- many1C digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()
-- parse nat "123 abc" -- [(123, " abc")]
-- parse space "     abc" -- [((), "abc")]

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

nlist :: Parser [Int]
nlist = do symbol "["
           n <- natural
           ns <- many (do symbol ","
                          natural)
           symbol "]"
           return (n:ns)
-- parse nlist "[1, 2, 3]" -- [([1,2,3], "")]
-- parse nlist "[1, 2]" -- [([1,2], "")]
-- parse nlist "[1, 2" -- []
-- parse nlist "[1 2" -- []
-- parse nlist "[1," -- []


-- arithmetic expressions
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (t * f)
            +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
                [(n, [])] -> n
                [(_, out)] -> error ("unused input: " ++ out)
                [] -> error ("invalid input: " ++ xs)
-- eval "2 * 3 + 4" -- 10
-- eval "2 * (3 + 4)" -- 14
-- eval "2 * 3 +" -- *** Exception: unused input: +
-- eval "2 * 3 - 4" -- *** Exception: unused input: - 4
-- eval "-4" -- *** Exception: invalid input: -4


-- programming with effects
data Expr = Val Int | Div Expr Expr

evalC :: Expr -> Int
evalC (Val n) = n
evalC (Div x y) = evalC x `div` evalC y

-- evalC (Val 3) -- 3
-- evalC (Div (Val 3) (Val 4)) -- 0
-- evalC (Div (Val 8) (Val 4)) -- 2
-- evalC (Div (Val 8) (Val 0)) -- *** Exception: divide by zero

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just (n `div` m)

-- too complicated method
eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
                        Nothing -> Nothing
                        Just n -> case eval2 y of
                                        Nothing -> Nothing
                                        Just m -> safediv n m

-- more abstracted
seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn _ Nothing = Nothing
seqn Nothing _ = Nothing
seqn (Just x) (Just y) = Just (x, y)

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = apply f (eval3 x `seqn` eval3 y)
                    where f (n, m) = safediv n m

-- sequential evaluating
(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>== f = case m of
                Nothing -> Nothing
                Just x -> f x

eval4 :: Expr -> Maybe Int
eval4 (Val x) = Just x
eval4 (Div x y) = eval4 x >>== \n ->
                  eval4 y >>== \m ->
                  safediv n m


-- list monad
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)
-- pairs [1, 2, 3] [4, 5, 6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]