
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)   | x < y = x : merge xs (y:ys)
                      | x == y = x : merge xs ys
                      | x > y = y : merge (x:xs) ys

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x : merge xs ys

multiples :: [[Integer]]
multiples = [map (n*) [1..] | n <- [2..]]

-- if all lists are in strictly increasing order, merge is associative
-- if the first elements of xs,ys,zs are in strictly increasing order, then xmerge is associative, too.
-- Q: Can we replace foldr1 xmerge multiples with foldl1 xmerge multiples ?
-- i.e. is there a difference between a right and left fold (without base element) in this case?
e1 = foldr1 xmerge multiples
e2 = foldl1 xmerge multiples

-- Just testing this yields that no.
-- e1 = [2..]
-- but e2 = bottom.
-- Why?
-- We know that multiples = [ms1, ms2, ms3, ...] so
-- e1 = foldr1 xmerge multiples
-- = ms1 `xmerge` (ms2 `xmerge` (ms3 `xmerge` (... )))
-- = ms1_1 : (ms1' `merge` (ms2 `xmerge` (ms3 `xmerge` (... ))))
-- So the xmerge produces a first element without looking at the right argument, and thus it is lazy in its
-- second argument, so all the remaining things are not evaluated. In contrast,
--
-- e2 = foldl1 xmerge multiples
-- = (((ms1 `xmerge` ms2) `xmerge` ms3) `xmerge` ms4) ...
-- The left argument could still be evaluated by lazy magic, but the problem is we don't even get there
-- because constructing the left argument requires traversing the whole list which is doomed in this case.
