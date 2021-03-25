-- In the text, we had the definition
-- mergeAll = foldr1 xmerge
--
-- ... But that didn't actually work out, so we replaced it with
-- mergeAll (xs:xss) = xmerge xs (mergeAll xss)
-- because pattern matching on the base case [x] = x:[] made the whole thing undefined
--
-- Would this be a reasonable alternative:
-- mergeAll = foldr xmerge []

-- ... The reason we didn't use the foldr1 thing was to prevent the pattern match on x:[]
-- foldr is defined differently:
-- foldr f e [] = e
-- foldr f e (x:xs) = ...
-- So we pattern match only against (x:xs), not (x:[]).
-- Since the list is never empty, the [] case never occurs, so this looks like it could work.
-- Let's test it...

mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr xmerge []

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)   | x < y = x : merge xs (y:ys)
                      | x == y = x : merge xs ys
                      | x > y = y : merge (x:xs) ys

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x : merge xs ys

primes = 2: ([3..] \\ composites)
    where composites = mergeAll [map (p*) [p..] | p <- primes]

(\\) :: Ord a => [a] -> [a] -> [a]
(\\) (x:xs) (y:ys) | x < y = x:(xs \\ (y:ys))
                   | x == y = xs \\ ys
                   | x > y = (x:xs) \\ ys

-- This... actually works.
-- Of course, it is a but stupid since foldr implies we could need the base case some time,
-- but we in fact don't, so the more explicit definition of mergeAll without a base case seemed reasonable.
-- Not sure if foldr1 should ever be used with infinite lists... hm.
