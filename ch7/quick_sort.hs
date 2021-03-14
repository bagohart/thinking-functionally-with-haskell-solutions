qsort1 :: (Ord a) => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 [y | y <- xs, y < x] ++ [x] ++
                qsort1 [y | y <- xs, x <= y]

partition1 p xs = (filter p xs, filter (not . p) xs)

partition2 p = foldr op ([],[])
    where op x (ys,zs) | p x = (x:ys,zs)
                       | otherwise = (ys,x:zs)

qsort2 [] = []
qsort2 (x:xs) = qsort2 ys ++ [x] ++ qsort2 zs
    where (ys,zs) = partition2 (<x) xs

qsort3 [] = []
qsort3 (x:xs) = sortp xs [] []
    where
        sortp [] us vs = qsort3 us ++ [x] ++ qsort3 vs
        sortp (y:xs) us vs = if y < x
                                then sortp xs (y:us) vs
                                else sortp xs us (y:vs)
