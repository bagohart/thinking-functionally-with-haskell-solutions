multiples :: [[Integer]]
multiples = [map (n*) [1..] | n <- [2..]]

multiples2 :: [[Integer]]
multiples2 = [map (2*n*) [1..] | n <- [2..]]

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)   | x < y = x : merge xs (y:ys)
                      | x == y = x : merge xs ys
                      | x > y = y : merge (x:xs) ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr1 xmerge

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x : merge xs ys

-- cyclic list
ones :: [Int]
ones = 1:ones

-- infinite list
ones2 = repeat 1

-- infinite
repeat1 x = x : repeat1 x

-- cyclic. dafuq? this is much faster.
repeat2 x = xs where xs = x:xs

iterate1 f x = x : iterate1 f (f x)
iterate2 f x = xs where xs = x : map f xs
iterate3 f x = x : map f (iterate3 f x)

-- iterate3 (2*) 1
-- = 1 : map (2*) (iterate3 (2*) 1)
-- = 1 : map (2*) (1 : map (2*) (iterate3 (2*) 1))
-- = 1 : 2 : map (2*) (map (2*) iterate3 (2*) 1)
-- = 1 : 2 : map (2*) (map (2*) (1: map (2*) (iterate3 (2*) 1)))
-- = 1 : 2 : 4 : map (2*) (map (2*) (map (2*) (iterate3 (2*) 1)))
-- ... Producing the 3rd element requires applying (2*) 3 times. uh oh.
-- So the first n elements need n^2 applications of f. lol.

-- iterate2 (2*) 1
-- = xs where xs = 1 : map (2*) xs
-- { now it looks like magic. Set xs = 1:ys and continue... }
-- 1:ys where ys = map (2*) (1:ys)
-- 1:ys where ys = 2 : (map (2*) ys)
-- { ys = 2 : zs }
-- 1:2:zs where zs = (map (2*) (2:zs))
-- 1:2:zs where zs = 4 : map (2*) zs
-- ...
-- this does not throw away intermediate results, so in n applications of f, the first n elements are computed. phew.
