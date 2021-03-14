import Data.Foldable

-- first try that compiles. bad.
mean1 :: [Float] -> Float
mean1 xs = sum xs / fromIntegral (length xs)

mean2 :: [Float] -> Float
mean2 [] = 0 -- ...
mean2 xs = sum xs / fromIntegral (length xs) -- space leak: xs is retained in memory -_-

sumlen1 :: [Float] -> (Float,Int)
sumlen1 xs = (sum xs, length xs)

sumlen2 :: [Float] -> (Float,Int)
sumlen2 [] = (0,0)
sumlen2 (x:xs) = (s+x,n+1) where (s,n) = sumlen2 xs

sumlen3 :: [Float] -> (Float,Int)
sumlen3 = foldr f (0,0) where f x (s,n) = (s+x,n+1)

sumlen4 :: [Float] -> (Float,Int)
sumlen4 = foldl' g (0,0) where g (s,n) x = (s+x,n+1)

sumlen5 :: [Float] -> (Float,Int)
sumlen5 = foldl' f (0,0)
    where f (s,n) x = s `seq` n `seq` (s+x,n+1)

mean3 :: [Float] -> Float
mean3 [] = 0
mean3 xs = s / fromIntegral n
    where (s,n) = sumlen5 xs
