-- essentially different...
-- a^3 + b^3 = c^3 + d^3 can be written in 8 different ways because
-- the left side and the right side could be swapped and
-- the left and right summand can be swapped
-- Note that
-- if (a,b,c,d) is valid, then changing any single number makes the whole thing invalid
-- => Therefore, [a,b] and [c,d] must be disjoint, or it is invalid
-- (a,b,c,d) and (c,d,a,b) are not essentially different
-- (a,b,c,d) and (d,c,b,a) are not essentially different
-- To sum up: if (a,b,c,d) and (a',b',c',d') are valid and essentially different, then
-- 1) [a,b] disjoint to [c,b] and [a',b'] disjoint to [c',b']
-- 2) {a,b} = {a',b'} => {c,d} and {c',d'} must be disjoint (does that even exist then?)
-- 3) {a,b} = {c',d'} => {c,d} and {a',b'} must be disjoint (does that even exist then?)
-- ...
-- Not sure what to do about this.
-- We can impose any arbitrary order and decide that
-- a<=b and c<=d
-- In this way, we can still generate all the things.
-- But we also need restrictions between both pairs.
-- If a == c, then either b == d (trivial) or b != d (must be unequal)
-- Therefore, we can assume a != c and even a < c wlog

quads :: Integer -> [(Integer, Integer, Integer, Integer)]
quads n = [(a,b,c,d) | a <- [1..n], b <- [a..n], c <- [a+1..n], d <- [c..n], a^3 + b^3 == c^3 + d^3]
