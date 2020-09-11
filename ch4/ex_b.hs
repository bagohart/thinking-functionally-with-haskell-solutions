import Data.List

allPairs :: Integral a => [(a, a)]
allPairs = [(x,y) | x <- [0..], y <- [0..]]

-- this function is subtly broken: it enumerates Only the pairs where x is 0
-- The problem here is how the cross product works in Haskell:
-- One value is chosen for the first thing, then all values are chosen for the second thing.
-- 
-- There are probably different ways to enumerate this correctly.
-- Let's try the run-diagonally-across-a-2-dimensional-infinite-matrix method which is
-- often used to demonstrate the enumerability of the rational numbers
-- except that we start at 0, not at 1.
--   0     1     2     3     4
-- 0 (0,0) (0,1) (0,2) (0,3) (0,4)
-- 1 (1,0) (1,1) (1,2) (1,3) (1,4)
-- 2 (2,0) (2,1) (2,2) (2,3) (2,4)
-- 3 (3,0) (3,1) (3,2) (3,3) (3,4)
-- 4 (4,0) (4,1) (4,2) (4,3) (4,4)
--
-- so we get
-- (0,0) 
-- (1,0) (0,1) 
-- (2,0) (1,1) (0,2)
-- (3,0) (2,1) (1,2) (0,3)
--
-- Strategy: define a function to compute one line as a list. then concat all of them.

diagonalRun :: Integral a => a -> [(a,a)]
diagonalRun n = unfoldr successor (n, 0)

successor :: Integral a => (a,a) -> Maybe ((a,a),(a,a))
successor = \(x,y) -> if x < 0 then Nothing else Just ((x,y),(x-1,y+1))

allPairs2 :: Integral a => [(a,a)]
allPairs2 = concat $ map diagonalRun [0..]

-- alternative:
diagonalRun' :: Integral a => a -> [(a,a)]
diagonalRun' n = take (fromIntegral $ n+1) (iterate (\(x,y) ->  (x-1, y+1)) (n,0))

allPairs3 :: Integral a => [(a,a)]
allPairs3 = concat $ map diagonalRun' [0..]

-- the sample solution ends up with mostly the same strategy, but computed a bit differently:
-- it restricts the second element to go up to only as far as the first element:
-- and subtracts at directly.
-- not sure if there's any difference in performance.
-- My solution seems to do simpler arithmetic, so maybe it is faster.
-- it's not so nice with list comprehensions and stuff though
allPairsSolution = [(x,d-x) | d <- [0..], x <- [0..d]]
