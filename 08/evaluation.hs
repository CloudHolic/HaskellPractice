-- infinite list
ones :: [Int]
ones = 1 : ones
-- take 5 ones -- [1, 1, 1, 1, 1]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x


-- generate primes
primes :: [Int]
primes = seive [2..]

seive :: [Int] -> [Int]
seive (p:xs) = p : seive [x | x <- xs, x `mod` p /= 0]
-- take 10 primes -- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
-- takeWhile (< 15) primes -- [2, 3, 5, 7, 11, 13]


-- strict application
sumWith :: Int -> [Int] -> Int
sumWith v [] = v
sumWith v (x:xs) = (sumWith $! (v + x)) xs
-- sumWith 0 [1, 2, 3]

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = ((foldl' f) $! (f v x)) xs