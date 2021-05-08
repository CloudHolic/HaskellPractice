import Data.Char

-- The Caesar Cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c   | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c
-- shift (-1) 'a' -- 'z'
-- shift (3) 'a' -- 'c'

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]
-- encode 1 "abc" -- "bcd"
-- encode 3 "haskell is fun" -- "kdvnhoo lv ixq"
-- encode (-3) "kdvnhoo lv ixq" -- "haskell is fun"


-- Cracking the Cipher
tables :: [Float]
tables = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

lowers :: String -> Int
lowers cs = length [c | c <- cs, isLower c]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e) ^ 2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x' == x]
                 where n = (length xs) - 1

crack :: String -> String
crack xs = encode (-factor) xs
            where
                factor = head (positions (minimum chiTab) chiTab)
                chiTab = [chisqr (rotate n table) tables | n <- [0..25]]
                table = freqs xs
-- crack "kdvnhoo lv ixq" -- "haskell is fun"
-- crack "vscd mywzboroxcsyxs kbo esopev" -- "list comprehensioni are uieful"
-- crack "vscd mywzboroxcsyxs kbo ecopev" -- "list comprehensioni are useful"

-- crack (encode 3 "haskell") -- "piasmtt"
-- crack (encode 3 "boxing wizards jump quickly") -- "wjsdib rduvmyn ephk lpdxfgt"


-- [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
-- same as
-- concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]