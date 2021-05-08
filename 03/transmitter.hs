import Data.Char

-- string transmitter
type Bit = Int

unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + acc * 2) 0

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

trnasmit :: String -> String
trnasmit = decode . channel . encode

map' f = unfold null (f . head) tail
iterate' f = unfold (const False) id f
-- (const False) is pred that returns always False