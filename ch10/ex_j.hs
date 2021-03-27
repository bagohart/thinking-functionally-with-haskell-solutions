hangman :: IO ()
hangman = do xs <- readFile "Words"
             play (words xs)

play :: [String] -> IO ()
-- play = undefined
play (word:words) = do oneGame word
                       putStrLn "Play again? (yes or no)"
                       input <- getLine
                       if input == "yes"
                          then play words
                          else pure ()
play [] = putStrLn "You guessed ALL THE WORDZ, IT'S OVER!!!11!1"

oneGame :: String -> IO ()
oneGame word = do putStrLn "I am thinking of a word"
                  putStrLn $ fmap (const '_') word
                  putStrLn "Try and guess it."
                  guess (fmap Secret word)

data Position = Secret Char | Discovered Char -- deriving (Show,Eq)

instance Show Position where 
    show (Secret _) = "_"
    show (Discovered c) = [c]

isSecret :: Position -> Bool
isSecret (Secret _) = True
isSecret _ = False

isDiscovered :: Position -> Bool
isDiscovered (Discovered _) = True
isDiscovered _ = False

getLetter :: Position -> Char
getLetter (Secret c) = c
getLetter (Discovered c) = c

foldPosition :: (Char -> a) -> (Char -> a) -> Position -> a
foldPosition g _ (Secret c) = g c
foldPosition _ h (Discovered c) = h c

guess :: [Position] -> IO ()
guess s = do putStr "guess: "
             input <- getLine
             if length input /= length s
                then putStrLn "Wrong number of letters!" >> guess s
                else if input == map getLetter s
                        then putStrLn "You got it!"
                        else let s' = updateState s input in putStrLn (printState s') >> guess s'
                            where printState s' = fmap (foldPosition (const '_') id) s'

updateState :: [Position] -> String -> [Position]
updateState s word = fmap updatePosition s
    where updatePosition (Discovered c) = Discovered c
          updatePosition (Secret c) = if c `elem` word then Discovered c else Secret c

-- the sample solution is... not terribly exciting here.
-- It seems to do something slightly different, but the task was underspecified anyway...
-- and it's more gode golfy, without dedicated data structures and stuff.
