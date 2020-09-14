{-
Prove this:
cross (f,g) . cross (h,k) = cross (f.h, g.k)
^ This looks like the functor law, just on a pair of things, with different functions.

cross (f,g) . cross (h,k) # definition
= fork (f . fst, g . snd) . fork (h . fst, k . snd) # law 1
= cross (f, g) . fork (fst, snd) . fork (h . fst, k . snd) ???

start over...
cross (f,g) . cross (h,k) # definition
= fork (f . fst, g . snd) . fork (h . fst, k . snd) # law 2
= fork (f . fst . fork (h . fst, k . snd), g . snd . fork (h . fst, k . snd)) # definition cross
= fork (f . fst . cross (h,k), g . snd . cross (h,k)) # law 3 & 4
= fork (f . h . fst, g . k . snd) # definition cross
= cross (f . h, g . k)

BAM.

Show:
cross (id,id) = id

cross (id,id) # definition
= fork (id . fst, id . snd) # law 3 & 4, just invent a function (why not id?)
= fork (fst . cross (id,id), snd . cross (id,id)) # law 2
= fork (fst, snd) . cross (id,id)
... that looks useless. try again.

cross (id,id) # definition
= fork (id . fst, id . snd) # definition id
= fork (fst, snd)
... hmm.
This is obviously just id, but I don't have a law about that.
with non-pointfree style we can simply put in the definitions:
fork (fst, snd) (x,y)
= (fst (x,y), snd (x,y))
= (x, y)
But with pointfree style I don't know.
The book neglects to tell me, too :-(
-}

type Pair' a b = (a,b)

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
    bimap f g (x,y) = (f x, g y)

-- express cross in terms of bimap
cross2 :: (a->r, b->s) -> (a,b) -> (r,s)
cross2 = \(f,g) -> bimap f g

-- I just extracted things out of a tuple, and put them to a function that allows partial application
-- so currying magic works, too:
cross3 :: (a->r, b->s) -> (a,b) -> (r,s)
cross3 = uncurry bimap
-- recall
-- uncurry :: (a -> b -> c) -> (a, b) -> c

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
    bimap f _ (Left' x) = Left' (f x)
    bimap _ g (Right' x) = Right' (g x)
