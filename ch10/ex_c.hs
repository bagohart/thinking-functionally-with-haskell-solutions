-- Recall: itis inefficient because `cp xss` is computed again and again
-- We can compute it first, then it is not computed multiple times, but the result is different.
--
-- Def. commutative:
-- do x <- p
--    y <- q
--    f x y
--
-- is equivalent to
-- do y <- q
--    x <- p
--    f x y
--
-- IO is not commutative because
-- putChar 'a' >> putChar 'b'
-- is not the same as
-- putChar 'b' >> putChar 'a'

-- Obviously, the list monad is not commutative:
xlist = [1..5]
ylist = [9..10]

l1 = do x <- xlist
        y <- ylist
        pure (x,y)

l2 = do y <- ylist
        x <- xlist
        pure (x,y)

-- What about the Maybe monad?
-- The Maybe monad is obviously commutative:
-- it fails iff at least one of them fails.
-- Since both terms are independent, the order does not at all matter.
-- Note that this holds only because the two are independent;
-- if q dependen on x (or p on y), then it would probably be wrong.
-- Can we prove this?
-- lhs =
-- do x <- p
--    y <- q
--    f x y
-- If p = Just x and q = Just y then lhs = f x y 
-- the rhs
-- do y <- q
--    x <- p
--    f x y
-- gets the same result.
-- For all other cases, lhs = rhs = Nothing.
--
-- So there is not much to do here except check all the cases.
