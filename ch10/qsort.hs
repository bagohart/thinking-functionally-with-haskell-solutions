isSorted xs = and $ zipWith (<=) xs (tail xs)

unsorted = [0,10,5,20,3,4,9,2,7,1,3,1,3,8]

qsort0 :: Ord a => [a] -> [a]
qsort0 [] = []
qsort0 (x:xs) = qsort0 [y | y <- xs, y < x] ++ [x] ++ qsort0 [y | y <- xs, x <= y]

-- this ... is not so nice.
-- it would compile, but some information about what sort of newListArray is needed etc. is missing
-- so I won't bother
--
-- qsort1 :: (Ord a) => [a] -> [a]
-- qsort1 xs = runST $
--     do xa <- newListArray (0,n-1) xs
--        qsortST xa (0,n)
--        getElems xa
--     where n = length xs
