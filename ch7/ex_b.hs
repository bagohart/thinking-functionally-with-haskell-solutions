import Data.Foldable

length1 :: [a] -> Int
length1 [] = 0
length1 (_:xs) = 1 + length1 xs

-- ^ This does NOT evaluate in constant space, it creates
-- length1 [1,2,3] = 1 + length1 [2,3] = 1 + (2 + (length1 [3]))

length2 :: [a] -> Int
length2 = foldl' (\n _ -> n+1) 0
-- This looks like it should work. It uses seq' though to implement foldl'.

-- How can we avoid seq? We need to force evaluation of the term that calculates the length,
-- but without using seq. To force evaluation, we can instead use pattern matching or just an if clause, right?
-- And we need to actually get to the value - if we only let it bubble up to the result but we never touch it,
-- then we can't force evaluation. So we need to access it sooner rather than later.
-- Something like:

length3 :: [a] -> Int
length3 xs = lenacc xs 0

lenacc :: [a] -> Int -> Int
lenacc [] n = n
lenacc (x:xs) n = if n == 0 then lenacc xs 1 else lenacc xs (n+1)
-- the if check ensures that (n+1) gets evaluated to n' = (n+1), so the term cannot accumulate
-- a bunch of 1s, only one.
-- We could prevent this further by forcing evaluation of (n+1), too, but this way it already satisfies the task
-- which was to keep it in constant space.
