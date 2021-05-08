-- head [2, 3, 4] -- 2
-- tail [2, 3, 4] -- [3, 4]

-- init [2, 3, 4] -- [2, 3]
-- last [2, 3, 4] -- 4

-- take 2 [2, 3, 4] -- [2, 3]
-- drop 2 [2, 3, 4] -- [4]

-- [1, 2, 3, 4] !! 1 -- 2
-- [1, 2, 3, 4] !! 2 -- 3
-- [1, 2, 3, 4, 5] !! 2 -- 3
-- [1, 2, 3, 4, 5] !! 0 -- 1

-- product [1, 2, 3, 4, 5] -- 120
-- [1, 2, 3] ++ [4, 5] -- [1, 2, 3, 4, 5]
-- reverse [1, 2, 3, 4] -- [4, 3, 2, 1]

double x = x + x
quadruple x = double (double x)
-- take (double 2) [1, 2, 3, 4, 5, 6] -- [1, 2, 3, 4]

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> Int -> Int
add' x y = x + y
add3 = add' 3
-- add3 4 -- 7

-- fst :: (a, b) -> a
-- head :: [a] -> a
-- take :: Int -> [a] -> [a]
-- zip :: [a] -> [b] -> [(a, b)]
-- [1, 2, 3] `zip` ['a', 'b', 'c'] -- [(1, 'a'), (2, 'b'), (3, 'c')]

-- sum :: Num => [a] -> a
-- (<) :: Ord a => a -> a -> Bool
-- (==) :: Eq a => a -> a -> Bool
-- (+) :: Num a => a -> a -> a

palindrome xs = reverse xs == xs
-- :t palindrome -- Eq [a] -> [a] -> Bool

-- :t Show -- Show a => a -> String
-- show False -- "False"
-- show [1, 2, 3] -- "[1, 2, 3]"

-- :t Read -- Read a => String -> a
-- read "False" -- Bool
-- read "[1, 2, 3]" :: [Int] -- [1, 2, 3]

abs n   | n >= 0 = n
        | otherwise = -n

signum n    | n > 0 = 1
            | n < 0 = -1
            | otherwise = 0

not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- or
(&&&) :: Bool -> Bool -> Bool
True &&& b = b
False &&& _ = False

-- \x -> x + 1
add'' = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

odds n = map (\x -> x `mod` 2 /= 0) [0..n]

-- 1 + 2
-- same as
-- (+) 1 2

-- double (k (/2))