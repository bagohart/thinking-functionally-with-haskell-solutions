-- Prove the leapfrog rule:
-- (f >=> g) . h   =   (f . h) >=> g
-- Note that (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
-- So h :: d -> a
-- i.e. h is not a monadic function!
-- The rule does _look_ strange, but on second thought, it is obvious: f >=> g means that f is applied first, and then h even before that.
-- So this should be easy. Maybe.
--
-- (f >=> g) . h
-- { Def. >=> }
-- ... this doesn't really work. I have to apply it to some value. Try again.
--
-- (f >=> g) . h $ x
-- { Apply h }
-- = (f >=> g) (h x)
-- f (h x) >>= g
--
-- And the right side:
-- (f . h) >=> g $ x
-- { Apply >=> }
-- = (f . h) x >>= g
-- { Apply f . h }
-- = f (h x) >>= g
-- 
-- ok, that was in fact pretty easy.

-- Second task: use the leap frog rule to prove:
-- (return . h) >=> g    =    g . h
-- Obverse g :: b -> m c
-- and so h :: a -> b
-- Intuitive meaning: wrapping something in a monad without effects and then getting it out again doesn't make any difference.
-- This should be easy to prove with the first technique, actually,
-- because this doesn't actually seem any more complicated than the leap frog rule itself:
-- (return . h) >=> g    $ x
-- { Def . >=> }
-- = (return . h) x >>= g
-- { Def. (.) }
-- = return (h x) >>= g
-- { left identity of return }
-- = g (h x)
-- { Def. (.) }
-- = (g . h) x
--
-- ok, this worked. Now see if it's easier with the leap frog rule...
-- Recall, we want to prove:
-- (return . h) >=> g    =   g . h
-- (>=>) is only on the left side, so we should probably start there.
-- return . h >=> g
-- { Leapfrog rule }
-- (return >=> g) . h
-- { Left identity of return }
-- g . h
-- ok, that was really nice.
-- I think the take-away here is that the identity laws for >=> are applicable without the argument, which makes everything a bit easier.
