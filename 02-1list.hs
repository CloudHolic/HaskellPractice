import Data.Char

-- [x^2 | x <- [1..3]] -- [1, 4, 9]

-- [(x, y) | x <- [1..3], y <- [4..5]]
-- [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)]

-- [(x, y) | y <- [4..5], x <- [1..3]]
-- [(1, 4), (2, 4), (3, 4), (1, 5), (2, 5), (3, 5)]

-- [(x, y) | x <- [1..3], y <- [x + 1]]
-- [(1, 2), (2, 3), (3, 4)]

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
-- concat [[1, 2, 3], [4, 5], [6]]
-- [1, 2, 3, 4, 5, 6]

-- [x | x <- [1..10], even x]
-- [2, 4, 6, 8, 10]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- factors 17 -- [1, 17]
-- factors 15 -- [1, 3, 5, 15]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
-- primes 40
-- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

-- zip :: [a] -> [b] -> [(a, b)]
-- zip [1, 2, 3] ['a', 'b', 'c', 'd']
-- [(1, 'a'), (2, 'b'), (3, 'c')]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)
-- pairs [1, 2, 3, 4]
-- [(1, 2), (2, 3), (3, 4)]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]
-- sorted [1, 2, 3, 4] -- True
-- sorted [1, 2, 5, 3, 4] -- False

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                    where n = (length xs) - 1
-- positions 0 [0, 1, 0, 1, 1, 1, 1, 0]
-- [0, 2, 7]

-- zip "abc" [1, 2, 3] -- [('a', 1), ('b', 2), ('c', 3)]
-- take 3 "asdasd" -- "asd"
-- length "adasd" -- 5

-- use Data.Char
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]