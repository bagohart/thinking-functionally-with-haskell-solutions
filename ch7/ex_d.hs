cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]
-- This is inefficient, as it calculates cp xss again and again.

-- because it desugars to
cpdesugar [] = [[]]
cpdesugar (xs:xss) = concat (map f xs)
    where f x = [x:ys | ys <- cpdesugar xss]

cp' :: [[a]] -> [[a]]
cp' = foldr op [[]]
    where op xs yss = [x:ys | x <- xs, ys <- yss]
-- This avoids recalculation of yss

-- So what about this instead?:
cp2 :: [[a]] -> [[a]]
cp2 [] = [[]]
cp2 (xs:xss) = [x:ys | ys <- cp xss, x <- xs]

-- this desugars to
cp2_desugar [] = [[]]
cp2_desugar (xs:xss) = concat (map f (cp2_desugar xss))
    where f ys = [x:ys | x <- xs]

-- This looks like it computes cp xss only once. So it seems ok.
-- The order is different though.

-- Now let's do fusion for foldr.
fcp' :: (Ord a) => [[a]] -> [[a]]
fcp' = filter nondec . cp2

nondec :: (Ord a) => [a] -> Bool
nondec [] = True
nondec [x] = True
nondec (x:y:xs) = (x <= y) && nondec (y:xs)

nondec' xs = and (zipWith (<=) xs (tail xs))

-- f . foldr g a = foldr h b
-- if
-- 1. f is strict
-- 2. f a = b
-- 3. f (g x y) = h x (f y)    for all x and y

-- First, write cp2 as foldr something.
cpf :: [[a]] -> [[a]]
cpf = foldr op [[]]
    where op xs yss = [x:ys | x <- xs, ys <- yss]

-- So
-- fcp' = f . foldr g a
-- with
-- f = filter nondec
-- a = [[]]
-- g = \xs yss -> [x:ys | x <- xs, ys <- yss]       -- (op from cpf)
-- Now try to compute the right side: foldr h b

-- 1. filter nondec is strict? Yes, it evaluates its arguments completely with <= ...
-- 2. f a = filter nondec [[]] = [[]] = b
-- 3. filter nondec (g xs yss) = h xs (filter nondec yss)
--      h xs yss' = [x:ys | x <- xs, ys <- yss, null ys || x <= head ys]

fcp :: (Ord a) => [[a]] -> [[a]]
fcp = foldr h [[]]
    where h xs yss' = [x:ys | x <- xs, ys <- yss', null ys || x <= head ys]

-- the sample solution gives a slightly different result:
fcp_sample :: (Ord a) => [[a]] -> [[a]]
fcp_sample = foldr g [[]]

g xs [[]] = [[x] | x <- xs]
g xs yss = [x:ys | x <- xs, ys <- yss, x <= head ys]

-- which just uses the recursion base more explicitly, otherwise it seems identical.
