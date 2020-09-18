import Data.List

-- 1.
-- takePrefix nondec [0,3,7,6,8,9] = [1,3,7]
-- takePrefix (all even) [2,4,7,8] = [2,4]
-- takePrefix (all p) = takeWhile p
takePrefix :: ([a] -> Bool) -> [a] -> [a]
-- takePrefix p xs = last $ filter p (inits xs)
takePrefix p = last . filter p . inits

nondec :: Ord a => [a] -> Bool
nondec xs = and $ zipWith (<=) xs (tail xs)

one x = [x]
none x = []

{-
2.

none . f = none
^ aka: you don't need to apply a function to a result that you are going to throw away anyway.

map f . none = none
^ mapping over nothing is like not mapping over nothing.

map f . one = one . f
-}

{-
3.

fst . fork (f,g) = f
snd . fork (f,g) = g
fork (f,g) . h = fork (f.h, g.h) # I think I've seen this one before
-}

{-
4.
test p (f,g) x = if p x then f x else g x

test p (f,g) . h = test (p.h) (f.h, g.h)
h . test p (f,g) = test p (h.f, h.g)

alternative definition for filter:
filter p = concat . map (test p (one,none))
^ put every element in an array with 0 or 1 elements, then merge all the arrays again.

Prove:
filter p = map fst . filter snd . map (fork (id,p))
^ you can map this on p while keeping original value, filter the pairs, and shrink the pairs again to keep the original value
^ this is... kinda obvious ?_?

Start on the right side, use the alternative definition of filter and see what happens:
    map fst . filter snd . map (fork (id,p))
=   map fst . (concat . map (test snd (one,none))) . map (fork (id,p))
=   map fst . concat . map (test snd (one,none)) . map (fork (id,p))        # functor
=   map fst . concat . map ((test snd (one,none)) . (fork (id,p)))          # test p (f,g).h = test p (f.h,g.h)
=   map fst . concat . map ((test snd (one.fork (id,p), none.fork (id,p)))) # none . f = none
=   map fst . concat . map ((test snd (one.fork (id,p), none)))
... this isn't going anywhere. hm.
The sample solution suggests to put the concat to the left. this seems... obvious, since that is where we have to arrive at the end. hm.

    map fst . filter snd . map (fork (id,p))
=   map fst . concat . map (test snd (one,none)) . map (fork (id,p))        # map f . concat = concat . map (map f)
=   concat . map (map fst) . map (test snd (one,none)) . map (fork (id,p))        # functor law, apply twice
=   concat . map (map fst . test snd (one,none) . fork (id,p))

Now show:
map (test p (one,none)) = map (map fst . test snd (one,none) . fork (id,p))
<=
Show
test p (one,none) = map fst . test snd (one,none) . fork (id,p)

Start with right side:

    map fst . test snd (one,none) . fork (id,p) # push thing from left into test (some law from above)
=   test snd (map fst . one, map fst . none) . fork (id,p) # law of one and none
=   test snd (one . fst, none) . fork (id,p) # put thing from right into test
=   test (snd . fork (id,p)) (one . fst . fork (id,p), none . fork (id,p)) # fork laws
=   test p (one . id, none . fork (id,p)) # none law
=   test p (one . id, none) # f . id = f
=   test p (one, none) # f . id = f
BAM.

I wonder what would happen if I push the things into test from the right side first...
    map fst . test snd (one,none) . fork (id,p) # push thing from right into test (some law from above)
=   map fst . test (snd . fork (id,p)) (one.fork (id,p), none . fork (id,p)) 
=   map fst . test p (one.fork (id,p), none) # push thing from left into test
=   test p (map fst.one.fork (id,p), map fst . none)
=   test p (one.fst.fork (id,p), none)
=   test p (one.id, none)
=   test p (one, none)
ok, that was basically exactly the same. phew. 
-}

{-
5.
map (fork (f,g)) = uncurry zip . (??)

^ the left side says: apply both f and g on things in a list, then get a tuple of lists

Let's try to follow the types without thinking too much:

zip :: [a] -> [b] -> [(a,b)]
and
uncurry :: (a -> b -> c) -> (a,b) -> c
then:
uncurry zip :: ([a],[b]) -> [(a,b)]

Also,
fork :: ((a -> b),(a -> c)) -> a -> (b,c)
and
map :: (a->b) -> [a] -> [b]
therefore:
map (fork (f,g)) :: [a] -> [(b,c)]

... whatever. We see that the left expression expects an array of a, whereas the right thing expects a pair of arrays
So the ?? has to expect an array of a, und turn that into a pair of arrays
I think... map fork does exactly that except we have to unzip the things first? xD


map (fork (f,g)) = uncurry zip . (??)
?? = unzip . map (fork (f,g))
then
map (fork (f,g)) = uncurry zip . unzip. map (fork (f,g))
because uncurry zip . unzip = id
... ?_? what is this task about again o_O?

Try again...
-}

fork :: ((a -> b),(a -> c)) -> a -> (b,c)
fork (f,g) x = (f x, g x)

fk5 f g = map (fork (f,g))
{-
5. second try
map (fork (f,g)) expects [a], returns [(b,c)]
uncurry zip expects ([b],[c]), returns [(b,c)]

so the ?? gets from [a] to ([b],[c])
the canonical way to achieve this would indeed be: go from [a] to [(b,c)], then unzip it to ([b],[c])
I could define this in one step, but why would I?
?? = lol f g
-}

lol :: (a->b) -> (a->c) -> [a] -> ([b],[c])
lol _ _ [] = ([],[])
lol f g (x:xs) = (f x : as, g x : bs)
    where (as, bs) = lol f g xs

-- This works. It seems also useless. lol.

{-
hm, the sample solution obviously wanted me to do something completely different
and switch map fork around:
map (fork (f,g)) = uncurry zip . (??)
with ?? = fork (map f, map g)
i.e.
map (fork (f,g)) = uncurry zip . fork (map f, map g)

which basically reads like
"I can fork every element of an array with 2 functions, or equivalently I can fork the array first, and map 2 functions over both instances."

Notably, this does not follow from the fork laws from section 3.

-}

{-
6.
Task: calculate efficient program (linear # applications of f) for:
takePrefix (pr . foldl f e)

Recall:
takePrefix p = last . filter p . inits

It is obvious why that should be possible more efficiently:
the second prefix to check is just the first, but one more application of f.
because foldl.
this could become quite difficult o_O

    takePrefix p
=   last . filter p . inits # use the new statement about filter from 4
=   last . map fst . filter snd . map (fork (id,p)) . inits # new statement about map (fork (f,g)) from 5
=   last . map fst . filter snd . uncurry zip . fork (map id, map p) . inits # functor law
=   last . map fst . filter snd . uncurry zip . fork (id, map p) . inits # def. p (only for this task!)
=   last . map fst . filter snd . uncurry zip . fork (id, map (pr . foldl f e)) . inits
^ ok, that part seemed easy. now if inits is fold, I can use fusion law and everything will be great.
but actually, I think I did that already...
because originally the definition of scanl was
scanl f e = map (foldl f e) . inits
but I can't apply this yet, but there's another fork law I hope...

=   last . map fst . filter snd . uncurry zip . fork (id, map (pr . foldl f e)) . inits # fork (f,g) . h = fork (f.h, g.h)
=   last . map fst . filter snd . uncurry zip . fork (id . inits, map (pr . foldl f e) . inits) # id
=   last . map fst . filter snd . uncurry zip . fork (inits, map (pr . foldl f e) . inits) # functor law
=   last . map fst . filter snd . uncurry zip . fork (inits, map pr . map foldl f e . inits) # spec. scanl
=   last . map fst . filter snd . uncurry zip . fork (inits, map pr . scanl f e) # last . map g = g . last
=   fst . last . filter snd . uncurry zip . fork (inits, map pr . scanl f e) #

yay, I got everything except the last step. and that's probably not even a performance improvement,
it "only" makes the function look a bit cleaner.

Still, I think there should be an easier way to express this function, and without loss of performance.
(also, the fst . last at the beginning, though correct, looks pretty weird o_O)
Something that better expresses the idea of "continue folding until p holds no more"
Actually, this could probably be expressed in a single fold, that just also accumulates the entries.
Well, maybe this isn't actually any better o_O
-}

-- Alternative to takePrefix (p . foldl f e)
-- alternative :: (b -> Bool) -> (a -> b) -> b -> [a] -> [a]
-- alternative p f e = foldl op []
--     where op ... this looks a bit broken, but approximately like this o_O
-- ^ actually this is complete nonsense.
-- This function cannot be expressed as a fold, since the fold does not have termination condition!
-- Try again...
alternative :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> [a]
alternative p f e = snd . last . filter (p . fst) . scanl op (e,[])
    where op (e,a) x = (e `f` x, a ++ [x])

-- this seems to work. it is also very similar to the arrived solution.
-- basically, it skips the fork/zip thing and zipts directly, otherwise it's identical
-- well not quite, it is probably actually slower, because of the a ++ [x] part.
-- this could be remedied with x:a and reverse at the end, but... that doesn't make the code more readable either.
-- so I guess I have stockholm syndrome at last, and this code is actually the most readable way to express this.
-- lol.
