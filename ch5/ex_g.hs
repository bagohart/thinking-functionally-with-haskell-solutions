minimum1 :: Ord a => [a] -> a
minimum1 [] = error "minimum of empty list is baaaaad"
minimum1 (x:xs) = minimum' x xs

minimum' :: Ord a => a -> [a] -> a
minimum' y [] = y
minimum' y (x:xs) = if y < x then minimum' y xs else minimum' x xs
