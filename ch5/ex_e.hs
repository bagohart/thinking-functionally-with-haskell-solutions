import Data.List

-- don't swap elements around
-- this will be a bit (...quite very) stupid
nub1 :: (Eq a) => [a] -> [a]
nub1 = nub' []

nub' :: (Eq a) => [a] -> [a] -> [a]
nub' _ [] = []
nub' os (x:xs) 
    | x `elem` os = nub' os xs
    | otherwise = x : nub' (x:os) xs
-- the sample solution is less stupid :)
-- and uses filter for every element, so the complete list needn't be searched for every element

nub2 :: (Ord a) => [a] -> [a]
nub2 = nub'' . sort

nub'' :: (Ord a) => [a] -> [a]
nub'' [] = []
nub'' [x] = [x]
nub'' (x1:x2:xs) = if x1 == x2 then nub'' (x2:xs) else x1 : nub'' (x2:xs)
-- the sample solution with dropWhile is a bit nicer.

-- or, a bit convoluted without pattern matching:
nub3 :: (Ord a) => [a] -> [a]
nub3 = (nub''' Nothing) . sort

nub''' :: (Ord a) => Maybe a -> [a] -> [a]
nub''' _ [] = []
nub''' Nothing (x:xs) = x: nub''' (Just x) xs
nub''' (Just y) (x:xs) = if x == y then nub''' (Just y) xs else x : nub''' (Just x) xs
