merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

sort1 :: (Ord a) => [a] -> [a]
sort1 [] = []
sort1 [x] = [x]
sort1 xs = merge (sort1 ys) (sort1 zs)
    where (ys,zs) = halve xs

halve xs = (take m xs,drop m xs)
    where m = length xs `div` 2

halve2 xs = splitAt (length xs `div` 2) xs

sort2' n xs = (sort1 (take n xs),drop n xs)

sort2 0 xs = ([],xs)
sort2 1 xs = ([head xs],tail xs)
sort2 n xs = (merge ys zs, xs'')
    where (ys,xs')  = sort2 m xs
          (zs,xs'') = sort2 (n-m) xs'
          m         = n `div` 2

halve3 [] = ([],[])
halve3 [x] = ([x],[])
halve3 (x:y:xs) = (x:ys,y:zs)
    where (ys,zs) = halve xs
