reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

revcat1 :: [a] -> [a] -> [a]
revcat1 xs ys = reverse1 xs ++ ys

revcat2 :: [a] -> [a] -> [a]
revcat2 [] ys = ys
revcat2 (x:xs) ys = revcat2 xs (x:ys)

reverse2 :: [a] -> [a]
reverse2 xs = revcat2 xs []

----

lenplus :: [a] -> Int -> Int
lenplus [] n = n
lenplus (x:xs) n = lenplus xs (1+n)

length2 xs = foldl (\n x -> 1+n) 0 xs
