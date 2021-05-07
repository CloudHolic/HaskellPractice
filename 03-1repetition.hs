import Data.Char

-- recursion on lists
productC :: [Int] -> Int
productC [] = 1
productC (n:ns) = n * productC ns

lengthC :: [a] -> Int
lengthC [] = 0
lengthC (x:xs) = 1 + lengthC xs

reverseC :: [a] -> [a]
reverseC [] = []
reverseC (x:xs) = reverse(xs) ++ []

zipC :: [a] -> [b] -> [(a, b)]
zipC [] _ = []
zipC _ [] = []
zipC (x:xs) (y:ys) = (x, y) : zipC xs ys

dropC :: Int -> [a] -> [a]
dropC 0 xs = xs
dropC _ [] = []
dropC n (x:xs) = dropC (n - 1) xs

-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x:(xs ++ ys)


-- quicksort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]


-- mutual recursion
evenC :: Int -> Bool
evenC 0 = True
evenC n = oddC (n - 1)

oddC :: Int -> Bool
oddC 0 = False
oddC n = evenC (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


-- examples
initC :: [a] -> [a]
initC [_] = []
initC (x:xs) = x : initC xs

-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + (m * (n - 1))


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
                                else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
            where (ys, zs) = halve xs


-- higher order function
twice :: (a -> a) -> a -> a
twice f x = f (f x)

mapC :: (a -> b) -> [a] -> [b]
mapC f [] = []
mapC f (x:xs) = f x : mapC f xs
-- or define as list comprehension :
-- map f xs = [f x | x <- xs]
-- map (+1) [1, 3, 5, 7] -- [2, 4, 6, 8]

filterC :: (a -> Bool) -> [a] -> [a]
filterC p [] = []
filterC p (x:xs) | p x = x : filterC p xs
                 | otherwise = filterC p xs
-- or define as list comprehension :
-- filter p xs = [x | x <- xs, p x]
-- filter even [1..10] -- [2, 4, 6, 8, 10]


-- foldr (fold right)
foldrC :: (a -> b -> b) -> b -> [a] -> b
foldrC f v [] = v
foldrC f v (x:xs) = f x (foldrC f v xs)

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

map' :: (a -> b) -> [a] -> [b]
map' p xs = foldr (\x acc -> p x : acc) [] xs


-- composition
--(.) :: (b -> c) -> (a -> b) -> (a -> c)
--f . g = \x -> f (g x)

odd' :: Int -> Bool
odd' = not . even

twice' :: (a -> a) -> a -> a
twice' f = f . f


-- all, any
all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (\x acc -> p x && acc) True xs

-- use Data.Char
any :: (a -> Bool) -> [a] -> Bool
any p xs = or [p x | x <- xs]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)


-- takeWhile, dropWhile
takeWhileC :: (a -> Bool) -> [a] -> [a]
takeWhileC p [] = []
takeWhileC p (x:xs) | p x = x : takeWhileC p xs
                    | otherwise = []
-- takeWhile isAlpha "abc def" -- "abc"

dropWhileC :: (a -> Bool) -> [a] -> [a]
dropWhileC p [] = []
dropWhileC p (x:xs) | p x = dropWhileC p xs
                    | otherwise = x:xs
-- dropWhile isAlpha "fp 101" -- " 101"


-- church numerals
zero = \s z -> z
one = \s z -> s z
two = \s z -> s . s
-- same as
-- two = \s z -> (s . s) z

c2i x = x (+1) 0
-- c2i zero -- 0
-- c2i one -- 1
-- c2i two -- 2

c2s x = x ('*':) ""
-- c2s zero -- ""
-- c2s one -- "*"
-- c2s two -- "**"

add x y = \s z -> x s (y s z)
-- c2i (add one two) -- 3

mul x y = \s z -> x (y s) zero
-- c2i (mul two five) -- 10


idC :: a -> a
idC = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id