-- continuation
square :: Int -> Int
square x = x * x

add :: Int -> Int -> Int
add x y = x + y

square_cps :: Int -> (Int -> r) -> r
square_cps x = \cont -> cont (square x)

add_cps :: Int -> Int -> (Int -> r) -> r
add_cps x y = \cont -> cont (add x y)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y = \cont ->
    square_cps x $ \squared_x ->
        square_cps y $ \squared_y ->
            add_cps squared_x squared_y cont

-- square_cps 3 print -- "9"
-- add_cps 3 4 print -- "7"
-- pythagoras_cps 3 4 print -- "25"