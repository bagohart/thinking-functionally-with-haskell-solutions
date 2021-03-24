-- from before:
mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr1 xmerge

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)   | x < y = x : merge xs (y:ys)
                      | x == y = x : merge xs ys
                      | x > y = y : merge (x:xs) ys

-- this makes sense only if x < head ys is always the case or something.
xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x : merge xs ys

-- this is basically correct by construction... (if it terminates, that is)
primes = [2..] \\ composites
composites = mergeAll multiples
multiples = [map (n*) [n..] | n <- [2..]]

-- subtraction, where both lists are strictly increasing (and, probably, infinite, I think?)
(\\) :: Ord a => [a] -> [a] -> [a]
(\\) (x:xs) (y:ys) | x < y = x:(xs \\ (y:ys))
                   | x == y = xs \\ ys
                   | x > y = (x:xs) \\ ys

-- improvement: construct only the multiples of the primes, not all multiples
primes2 = [2..] \\ composites2
    where composites2 = mergeAll [map (p*) [p..] | p <- primes2]
-- this is bottom :'(
-- Problem: primes2 requires composites2 which requires primes2 etc.
-- So this doesn't work if we don't provide a first element. Let's try...

primes3 = 2:([3..] \\ composites3)
    where composites3 = mergeAll [map (p*) [p..] | p <- primes3]

-- mergeAll [map (p*) [p..] | p <- primes3]
-- = mergeAll [map (p*) [p..] | p <- 2:undefined]
-- = undefined
-- because foldr1 f (x:bottom) = quasi bottom. hm.
-- :'(

mergeAll2 (xs:xss) = xmerge xs (mergeAll2 xss)
-- this is better than foldr1 xmerge because it never tests the case
-- foldr1 f [x]
-- which for (x:undefined) goes full bottom.
-- Also, for finite lists it should crash. oh well.

primes4 = 2:([3..] \\ composites4)
    where composites4 = mergeAll2 [map (p*) [p..] | p <- primes4]
