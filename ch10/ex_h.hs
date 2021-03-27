-- Prove:
-- liftM f = id >=> (return  . f)
-- This seems a bit stupid. I combine a return and an id and a >=> only to get fmap? Well, I guess I can do that...
-- The types should be
-- f :: a -> b
-- id :: m a -> m a
-- (return . f) :: a -> m b
-- So the right side has a monad, doesn't do anything with it, then takes out its value, applies f on it and puts that in another monad and joins back the end result. Lots of things happening here.
-- What can I do about this?
-- I can't leap frog it, because on the left side there is only a lonely id.
-- The second rule from (g) doesn't apply, either, because the return is on the wrong side.
-- Also I don't actually have any rules for liftM, except the Functor rules which only talk about more liftMs,
-- which seems to not possibly lead to the other term. Hhhhmmmmmmm...
-- Actually, I can just plug in the definition from (d)...
--
-- liftM f $ ma
-- { Def. }
-- = (>>= (return . f)) $ ma
-- { Write with \ }
-- = (\ma -> ma >>= (return . f)) $ ma
-- { Def. id }
-- = (\ma -> id ma >>= (return . f)) $ ma
-- { Apply lambda }
-- id ma >>= (return . f)
--
-- Starting on the other side:
-- id >=> (return . f) $ ma
-- { Def >=> }
-- = id ma >>= (return . f)
--
-- ok, that seemed not too hard.

-- Now prove join = (id >=> id)
-- Recall the definition of join from (d):
-- join0 = (>>= id)
-- Then,
-- join $ mma
-- { Def. join }
-- mma >>= id
--
-- Aaaand I'm stuck. Start on the other side:
-- (id >=> id) $ mma
-- { Def. >=> }
-- = id mma >>= id
-- { Def. id }
-- = mma >>= id
-- Aaaaaand I'm done.

-- But wait, there's more.
-- The first 'new' laws are just the Functor laws.
-- The fourth rule, that I'm supposed to prove is:
-- liftM f . join = join . liftM (liftM f)
-- Which says that we can lift over two layers and then join the thing or join it first and then lift over only one layer.
-- Hm. This is probably a good thing.
-- So let's just plug in the definitions for liftM and join and see if that's enough?
-- Recall:
-- liftM f = (>>= (return . f))
-- join = (>>= id)
--
-- liftM f . join $ mma
-- { Def. join }
-- = liftM f $ (mma >>= id)
-- { Def. liftM }
-- = (mma >>= id) >>= return . f
--
-- join . liftM (liftM f) $ mma
-- { Def. liftM }
-- = join $ mma >>= (return . (liftM f))
-- { Def. join }
-- = (mma >>= (return . (liftM f))) >>= id
-- ... ?_?
--
-- This seems to not be going anywhere. Try again using the rules developed at the start of this exercise.
-- Left side:
-- liftM f . join
-- { join rule }
-- liftM f . (id >=> id)
-- { liftM rule }
-- (id >=> (return . f)) . (id >=> id)
-- { leapfrog (?) }
-- (id . (id >=> id)) >=> return . f
-- { Def. id }
-- (id >=> id) >=> return . f
--
-- Right side:
-- join . liftM (liftM f)
-- { join rule, liftM rule }
-- = (id >=> id) . (id >=> (return . (liftM f)))
-- { liftM rule }
-- = (id >=> id) . (id >=> (return . (id >=> (return . f))))
-- { leapfrog (?) }
-- = (id . (id >=> (return . (id >=> (return . f))))) >=> id
-- { Def. id }
-- = (id >=> (return . (id >=> (return . f)))) >=> id
-- { >=> associative }
-- = id >=> ((return . (id >=> (return . f))) >=> id)
-- { leapfrog (*1) }
-- = id >=> ((return >=> id) . (id >=> (return . f)))
-- { return left-identity of >=> }
-- = id >=> (id . (id >=> (return . f)))
-- { Def. id }
-- = id >=> (id >=> (return . f)))
-- { >=> associative }
-- = (id >=> id) >=> return . f
-- DONE!
-- Nice.
-- ... The sample solution suggests using the second rule instead from (g). Let's try that, too:
-- join . liftM (liftM f)
-- ...
-- = id >=> ((return . (id >=> (return . f))) >=> id)
-- { (g.2) }
-- = id >=> (id . (id >=> (return . f)))
-- { Def. id }
-- = id >=> (id >=> (return . f))
-- { >=> associative }
-- = (id >=> id) >=> return . f
-- Okay, that saves, like, one step, so I've basically done the proof for g.2 again it seems.

-- Subthingy (*1):
-- (return . (id >=> (return . f)) >=> id)
-- { leapfrog }
-- = (return >=> id) . (id >=> (return . f))
