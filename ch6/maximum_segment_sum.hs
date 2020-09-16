import Data.List

mss :: [Int] -> Int
mss = maximum . map sum . segments

segments = concat . map inits . tails

-- this is very direct, and also rather inefficient:
-- it calculates all the segments (including multiple empty lists),
-- and sums all of them

{-
mss = maximum . map sum . segments # definition segments
= maximum . map sum . concat . map inits . tails # map f . concat = concat . map (map f)
= maximum . concat . map (map sum) . map inits . tails # map f . map g = map (f . g)
= maximum . concat . map (map sum . inits) . tails # maximum . concat = maximum . map maximum (since non-empty lists are given as argument) (where does this rule come from? f . concat = f . map f? this is obviously wrong, e.g. for f = length, so this seems to be a new ad hoc law for maximum?)
= maximum . map (maximum) . map (map sum . inits) . tails # map f . map g = map (f . g)
= maximum . map (maximum . map sum . inits) . tails # map sum . inits = scanl (+) 0
= maximum . map (maximum . scanl (+) 0) . tails

^ now we have applied the first obvious optimization: don't throw away the intermediate results,
instead save them to compute a list of which the first list was a prefix
but we still compute all the tails, which we want to avoid, too.

scanl (+) 0 [x,y,z]
= [0, 0+x, (0+x)+y, ...]
= 0:map(x+) [0,y,y+z]
= 0:map (x+) (scanl (+) 0 [y,z])
^ aka: identity element is always the first thing, and the first element influences all further results

this can be stated more generally, but apparently (@) needs to be associative, and the identity thing is required
scanl (@) e = foldr f [e]
    where f x xs = e:map (x@) xs
this... makes sense.

we wanted to apply a fusion rule on this:
foldr1 max . scanl (+) 0
so we get

foldr1 (<>) . foldr f [e] = foldr h b
    where f x xs = e:map (x@) xs
^ we need to check the three conditions again:
1) foldr1 (<>) is strict ? yes, max is strict
2) foldr1 (<>) [e] = e ? yes, max [l] = l
3) foldr1 (<>) (e:map (x@) xs) = h x (foldr1 (<>) xs) ?
left side:  foldr1 (<>) (e:map (x@) xs) = e <> (foldr1 (<>) map (x@) xs)
right side: with xs = [y], it simplifies to:
 h x (foldr1 (<>) xs) =  h x (foldr1 (<>) [y]) = h x y
and the left side to:
 e <> (foldr1 (<>) map (x@) xs) = e <> (foldr1 (<>) map (x@) [y])
 = e <> (x@y)
 => definition of h is:
 h x y = e <> (x@y)

with definition for h, check rule 3:
3) foldr1 (<>) (e:map (x@) xs) = e <> (x @ (foldr1 (<>) xs))
by eleminating identity element (it is a fold!) and using pointfree style, we get
foldr1 (<>) . map (x@) = (x@) . foldr1 (<>)
this is obviously something with associativity. (see exercises)

Relevant: x + max(y,z) = max(x+y, x+z)

back to main expression:
    maximum . map (maximum . scanl (+) 0) . tails # fusion rule with all the things
=   maximum . map (foldr (@) 0) . tails
    where x @ y = 0 `max` (x + y)

the foldr . tails thing can be expressed as scanr
similar to foldl . inits = scanl

then we get
-}

mss3 = maximum . scanr (@@) 0
    where x @@ y = 0 `max` (x + y)

-- which seems quite obvious in hindsight o_O
-- this algorithm says "sum up all the values from right to left (but never less than 0), save all the intermediate results and at the end look which number was biggest"
--
-- all of this is pretty magic


