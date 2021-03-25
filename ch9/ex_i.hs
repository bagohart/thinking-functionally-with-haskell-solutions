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

approx :: Integer -> [a] -> [a]
approx n [] | n > 0 = []
approx n (x:xs) | n>0 = x : approx (n-1) xs

-- old definitions needed for the proof or something.
--------

prs n = approx n (2:([3..] \\ crs n))
crs n = mergeAll [map (p*) [p..] | p <- prs n]

-- Proof that crs n = c1 : c2 : .. : c_m : bot where m = p^2_n
-- Induction maybe?
-- IB:
-- crs 0 = mergeAll [map (p*) [p..] | p <- prs 0]
-- = mergeAll [map (p*) [p..] | p <- undefined]
-- = undefined
-- ... this seems useless.
-- 
-- For the IS, we need to argue about what happens with the primes and magic multiples.
-- Too much magic for me atm :)

-- What is even meant by primes_5?
-- This is probably like inserting the definition of primes 5 times, and afterwards bottom.
-- The generated numbers also depend on what multiples I can create like that.
-- Whatever exactly happens here probably needs more understanding of the multiples thingy o_O
