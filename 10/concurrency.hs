import Control.Applicative
import Control.Monad

-- process modeling
data Action = Atom (IO Action)
            | Fork Action Action
            | Stop

data Concurrent a = Concurrent ((a -> Action) -> Action)

action :: Concurrent a -> Action
action (Concurrent concur) = concur (\a -> Stop)

stop :: Concurrent a
stop = Concurrent (\cont -> Stop)

atom :: IO a -> Concurrent a
atom = \io -> Concurrent $ \cont -> Atom $ do a <- io
                                              return (cont a)

fork :: Concurrent a -> Concurrent ()
fork concur = Concurrent $ \cont -> Fork (action concur) (cont ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent a) (Concurrent b) = Concurrent $ \cont -> Fork (a cont) (b cont)

instance Functor Concurrent where
    fmap = liftM

instance Applicative Concurrent where
    pure = return
    (<*>) = ap

instance Monad Concurrent where
    -- g :: \a -> Concurrent b
    (Concurrent a) >>= g = 
        Concurrent (\contB -> a (\contA -> case g a of (Concurrent b) -> b contB))

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (Atom x:xs) = x >>= \ac -> roundRobin (xs ++ [ac])
roundRobin (Fork x y:xs) = roundRobin (xs ++ [x, y])
roundRobin (Stop:xs) = roundRobin xs

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42 = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")

myex0 = run $ (ho >> ho >> ho) >> (hi >> hi >> hi) >> atom (putStr "\n")
            where
                ho = atom (putStr "ho")
                hi = atom (putStr "hi")

myex1 = run $ fork (ho >> ho >> ho) >> (hi >> hi >> hi) >> atom (putStr "\n")
            where
                ho = atom (putStr "ho")
                hi = atom (putStr "hi")

myex2 = run $ fork (put3 "ba") >> fork (put3 "di") >> put3 "bu" >> atom (putStr "\n")
            where
                put3 = sequence . take 3 . repeat . atom . putStr

myex3 = run $ par (put3 "ba") (put3 "di" >> stop) >> atom (putStr "\n")
            where
                put3 = sequence . take 3 . repeat . atom . putStr

myex4 = run $ (par (put3 "ba") (put3 "di")) >> atom (putStr "\n")
            where
                put3 = sequence . take 3 . repeat . atom . putStr

myex5 :: Concurrent ()
myex5 = do fork (atom $ putStrLn "test")
           atom $ putStrLn "hello"

myex6 :: Concurrent ()
myex6 = do val <- par (atom $ return "hi") (atom $ return "hello")
           atom $ putStrLn val