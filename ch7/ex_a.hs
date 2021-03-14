unsorted = [1,8,5,7,6,4,3,3,9,2]

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

-- inserts an element into a list that is already sorted, and afterwards it is still sorted.
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x:y:ys else y:(insert x ys)

--     sort [3,4,2,1] 
--   = sort (3:[4,2,1])
--   = insert 3 (sort [4,2,1])
--   = insert 3 (insert 4 (sort [2,1]))
--   = insert 3 (insert 4 (insert 2 (sort 1)))
--   = insert 3 (insert 4 (insert 2 (insert 1 (sort []))))
--   = insert 3 (insert 4 (insert 2 (insert 1 [])))
--   = insert 3 (insert 4 (insert 2 [1]))
--   = insert 3 (insert 4 (1:(insert 2 []))
--   = insert 3 (1:(insert 4 (insert 2 [])))
--   = 1 : (insert 3 (insert 4 (insert 2 []))) -- HNF!
--   -- force next element...
--   = 1 : (insert 3 (insert 4 [2]))
--   = 1 : (insert 3 (2 : insert 4 []))
--   = 1 : (2 : (insert 3 (insert 4 [])))
--   = 1 : 2 : (insert 3 [4])
--   = 1 : 2 : 3 : [4]

-- (i) how long does it take to compute head . sort, applied on a list of length n, with lazy evaluation?
-- under lazy evaluation, the algorithm finds the smallest element and swaps it to the front.
--     So it works in O(n).
--         The trick is in the definition of insert: it treats the first two elements, and then the recursion
--         is skipped by the lazy evaluation.
--         How to prove this?
-- T(head . sort)(0) = O(1) . This crashes o_O (theta, actually...)
-- T(head . sort)(1) = O(1) 
-- T(head . sort)(n+1) = T(head . insert)(n) + T(???)
-- This doesn't really work, this notation was built for eager evaluation.

-- (ii) How long does it take to compute head . sort, applied on a list of length n, with eager evaluation?
-- Insertion sort takes N^2, everyone knows this. Can we prove it?
-- T(sort)(0) = O(1)
-- T(insert)(0) = O(1)
-- T(sort)(n+1) = T(insert)(n) + T(sort)(n)
-- T(insert)(n+1) = O(1) + T(insert)(n) -- worst case assumption here
-- Solving, this yields
-- T(insert)(n) = O(n)
-- and thus
-- T(sort)(n) = O(n^2)

-- (iii)
sort2 :: (Ord a) => [a] -> [a]
sort2 [] = []
sort2 xs = y:sort2 ys where (y,ys) = select xs

select :: (Ord a) => [a] -> (a,[a])
select [x] = (x,[])
select (x:xs) | x <= y = (x,y:ys)
              | otherwise = (y,x:ys)
              where (y,ys) = select xs

    {-
sort [3,4,2,1] 
= y:sort ys where (y,ys) = select [3,4,2,1]
                         = select (3:[4,2,1])
                         = (1,[3,4,2])
... select [4,2,1] = (1,[4,2])
... select [2,1] = (1,[2])
... select [1] = (1,[])
-}
-- Looks like this gives the same result: The algorithm looks for the smallest number and bubbles it to the front.
