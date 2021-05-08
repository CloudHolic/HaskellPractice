import System.IO

-- io basics
a :: IO (Char, Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x, y)

getLineC :: IO String
getLineC = do x <- getChar
              if x == '\n' then
                  return []
              else
                  do xs <- getLineC
                     return (x:xs)

putStrC :: String -> IO ()
putStrC [] = return ()
putStrC (x:xs) = do putChar x
                    putStrC xs

putStrLnC :: String -> IO ()
putStrLnC xs = do putStrC xs
                  putChar '\n'
                  

-- hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                    putStrLn "You got it!"
                    else do putStrLn (diff word xs)
                            guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]
-- diff "haskell" "pascal" -- "-as--ll"