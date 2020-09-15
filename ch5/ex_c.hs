-- any p = not . all (not . p)
-- ^ this says "is true for any one entry iff not false for all entries"
-- (the dot is missing in the book, this seems wrong)
-- which is basic boolean logic, and should thus be correct.
-- what about infinite lists? obviously,
-- any (\_ -> True) [1..] terminates.
-- not . all (not . (\_->True)) $ [1..] is less obvious, but it works:
-- the first not enforces execution of all
-- the thing fails on the first statement, and all is short-circuited immediately.

p :: a -> Bool
p _ = True

-- any null = null . cp
-- ^ an array contains an empty array, if its cross product is empty
-- This seems correct. the only interesting cases seem to be [] and [[]]
-- and for both of them it is true

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp xss
