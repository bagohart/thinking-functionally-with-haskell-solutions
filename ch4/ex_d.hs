-- [e | x <- xs, p x, y <- ys]
-- vs
-- [e | x <- xs, y <- ys, p x]

-- it looks like in both list comprehensions, the thing on the left side
-- does not depend on the thing on the right side
-- but that may be a misunderstanding in notation
-- both things are identical, and both lists have one (and the same) element except
-- the order of the condition and the second list thingy is a bit different, so...
--
-- Finiteness, especially of the second list, makes a difference. Consider:
l1 = [1 | x <- [1..3], False, y <- [1..]]
l2 = [1 | x <- [1..3], y <- [1..], False]
-- l1 is empty, l2 is undefined (since all values for y are tried, which are infinitely many)
--
-- Therefore:
-- if ys is finite, the results should be the same
-- if p x is true, the results should be the same
-- p is independent of the y <- ys part, so this is a bit like short circuiting
-- and should definitely be faster. note that p can be different for different x's, so this holds also for finite ys's.
--
-- The sample solution adds that ys can be undefined and can thus lead to the difference between
-- [] and undefined
