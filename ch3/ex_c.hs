-- div' :: Integral a => a -> a -> a
-- div' x y = floor (x/y)

-- This doesn't even compile because / is defined only for Fractional, not Integral
-- We could make it compile like this:

div' :: Integral a => a -> a -> a
div' x y = floor (toRational x / toRational y)

-- But then the absolute value depends on the sign, and that seems wrong
