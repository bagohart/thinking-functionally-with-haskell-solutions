takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) = if p x then x : takeWhile1 p xs else []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) = if p x then dropWhile1 p xs else (x:xs)

whitespace :: Char -> Bool
whitespace x = x `elem` [' ', '\t', '\n']

type Word1 = [Char]
words1 :: String -> [Word1]
words1 [] = []
words1 (c:cs) = if whitespace c 
                   then words1 (dropWhile1 whitespace cs)
                   else takeWhile1 (not . whitespace) (c:cs) : words1 (dropWhile1 (not . whitespace) cs)
-- sample solution uses break (good idea. I forgot its name) and always drops leading whitespace first
-- (probably more clear because this acts as normalization)
