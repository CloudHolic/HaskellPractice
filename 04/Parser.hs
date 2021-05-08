module Parser where
import Control.Applicative
import Control.Monad
import Data.Char

-- monadplus
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

instance MonadPlus Parser where
    mzero = P $ \_ -> []
    p `mplus` q = P $ \inp -> case parse p inp of
                                [] -> parse q inp
                                [(v, out)] -> [(v, out)]

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

item :: Parser Char
item = P $ \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

failure :: Parser Char
failure = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      +++ nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ do symbol "-"
                   e <- expr
                   return (t - e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            +++ do symbol "/"
                   t <- term
                   return (f `div` t)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural