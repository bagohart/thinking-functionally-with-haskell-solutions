import Data.List

nodups :: (Ord a) => [a] -> Bool
nodups = nodups' . sort

nodups' :: (Ord a) => [a] -> Bool
nodups' [] = True
nodups' [x] = True
nodups' (x1:x2:xs) = if x1==x2 then False else nodups' (x2:xs)

-- or, for example:
nodups2 :: (Ord a) => [a] -> Bool
nodups2 = nodups'' . sort

nodups'' :: (Ord a) => [a] -> Bool
nodups'' xs = all (\p -> fst p /= snd p) $ zip xs (tail xs)

-- or...
nodups3 :: (Ord a) => [a] -> Bool
nodups3 = nodups''' . sort

nodups''' :: (Ord a) => [a] -> Bool
nodups''' xs = all (==True) $ zipWith (/=) xs (tail xs)
-- ahaha I forgot. sample solution reminds me that all (==True) = and x_X
