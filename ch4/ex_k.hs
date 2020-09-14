-- This takes a value that must be a list of tuples, because map is applied on it.
-- The result is a tuple of things, and both these things must be lists. of things. so...
unzip' :: [(a,b)] ->([a],[b])
unzip' = fork (map fst, map snd)
-- in other words, it splits a list of tuples in the corresponding tuple of lists, retaining all the things in it.

-- this takes a single value, which must be a pair, and applies things on it
cross' :: (a->r, b->s) -> (a,b) -> (r,s)
cross' (f,g) = fork (f . fst, g . snd)
-- in other words, it applies different functions on elements of a pair, and we get a different pair

fork :: (a->b, a->c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

{---
now prove the thing:
cross (map f, map g) . unzip = unzip . map (cross (f,g))
^ which means basically:
left side: on a list of pairs, split it in pair of lists, map f/g over them
right side: on a list of pairs, map f/g on the single elements, then split in pairs
or, in other words: splitting the list first in pairs and then mapping is equivalent to
first mapping and then splitting in pairs.
this looks like it should be obviously correct.
as to infinite/undefined lists: doesn't really matter, will blow up in either case.
cross does not touch the contents of the lists/tuples, but the map will.

I can use:
map id = id
map (f . g) = map f . map g
and
cross (f,g) . fork (h,k) = fork (f . h, g . k)
^ a value is doubled to apply a function on it. the composed function is 'added' immediately or later.
this seems pretty obvious.
fork (f,g) . h = fork (f . h, g . h)
^ this is very similar to the last thing: if the first function applied to the value is identical,
then it's enough to apply it once. read it from right to left to save one function application.
fst . cross (f,g) = f . fst
^ if we throw away the second value anyway, we can compute on it whatever we want.
read from left to right as an optimization
snd . cross (f,g) = g . snd
^ analogous to the previous one.

cross (map f, map g) . unzip = unzip . map (cross (f,g))

Notably, we don't have any laws about unzip, but since unzip is expressed in terms of fork, we can inline it
and hopefully get something useful.

cross (map f, map g) . fork (map fst, map snd) = fork (map fst, map snd) . map (cross (f,g))
^ so let's try to derive this.
cross (map f, map g) . fork (map fst, map snd) # law 1
= fork (map f . map fst, map g . map snd) ... ?

^ let's start on the right side, too:
= unzip . map (cross (f,g)) # definition
= fork (map fst, map snd) . map (cross (f,g)) # law 2
= fork (map fst . map (cross (f,g)), map snd . map (cross (f,g))) # functor law
= fork (map (fst . cross (f,g)), map (snd . cross (f,g))) # law 3 & 4
= fork (map (f . fst), map (g . snd)) # functor law
= fork (map f . map fst, map g . map snd) # law 1 (hey, I've seen this before)
= cross (map f, map g) . fork (map fst, map snd) # definition
= cross (map f, map g) . unzip

BAM!
It was easier backwards, since with law 3&4 things have to be created out of thin air.
---}
