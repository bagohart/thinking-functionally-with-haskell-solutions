take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- take' 0 undefined = []
-- and
-- take' undefined [] = undefined
--
-- Cool question: is there a definition such that
-- both above evaluations are []?
-- It would be possible to satisfy the second requirement:
take'' :: Int -> [a] -> [a]
take'' _ [] = []
take'' 0 _ = []
take'' n (x:xs) = x : take'' (n-1) xs

-- but not both at once, since I have to choose one to be evaluated first
-- what is asked here is something that basically evaluates
-- (undefined or True) and (True or undefined) to True
-- if undefined is interpreted as non-terminating then this would seem plausible, but I don't think
-- Haskell lets me interpret it like this?

-- take n xs ++ drop n xs = xs
-- ^ the first n elements of the list plus all elements after the first n elements
-- ^ this seems correct, even on infinite lists, because take comes first, and terminates early

-- take m . drop n = drop n . take (m+n)
-- ^ elements n+1..n+b vs elements 1..m+n without the first n
-- seems correct.

-- take m . take n = take (m `min` n)
-- drop everything after m, then n elements
-- seems correct. take m on a list < m is a noop.

-- drop m . drop n = drop (m+n)
-- drop first n, then drop following first m, or drop both at once.
-- seems correct.

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = go n [] xs
    where
        go 0 ys xs = (ys,xs)
        go n ys [] = (ys,[])
        go n ys (x:xs) = go (n-1) (ys ++ [x]) xs

-- the sample solution avoids the ++ operator using a where clause and direct recursion of original function
-- not sure if it is more intelligible than my definition but it is a bit shorter
