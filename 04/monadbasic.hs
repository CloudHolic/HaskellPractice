-- basic parsers
type Parser a = String -> [(a, String)]

item :: Parser Char
item = \xs -> case xs of
                [] -> []
                (x:xs) -> [(x, xs)]
-- item "hello world" -- [('h', "ello world")]
-- item "" -- []

failure :: Parser a
failure = \xs -> []

returnC :: a -> Parser a
returnC v = \xs -> [(v, xs)]

-- failure "hello world" -- []
-- (returnC "hello") " world" -- [("hello", "world")]
-- (returnC "hello") "" -- [("hello", "")]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \xs -> case p xs of
                    [] -> parse q xs
                    [(y, ys)] -> [(y, ys)]

parse :: Parser a -> String -> [(a, String)]
parse p xs = p xs
-- parse (returnC '1') "234" -- [('1', "234")]
-- parse failure "abcd" -- []
-- parse (failure +++ (returnC '1')) "abcd" -- [('1', "abcd")]
-- parse (item +++ returnC 'd') "abc" -- [('a', "bc")]


-- sequencing
(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== q = \xs -> case p xs of
                    [] -> []
                    [(y, ys)] -> parse (q y) ys

-- consume only one Char
parseTwice :: Parser (Char, Char)
parseTwice = item >>== \x -> returnC (x, x)
-- parseTwice "5BEAF" -- [(('5', '5'), "BEAF")]

ignore2 :: Parser (Char, Char)
ignore2 = item >>== \x -> item >>== \y -> item >>== \z -> returnC (x, z)
-- ignore2 "2A371" -- [(('2', '3'), "71")]