-- type Angle = Float
-- We can't use == for the modulo test because it's just a type synonym, and Float already has a definition for ==.
-- Even if we could override it, we probably shouldn't.

newtype Angle = Angle Float
-- now we can do it because for a newtype we can have completely new instances.

instance Eq Angle where 
    (Angle a1) == (Angle a2) = normalize a1 - normalize a2 < eps
                                where eps = 0.001
                                      normalize x | x < 0 = normalize (x + 2 * pi)
                                                  | x > 2 * pi = normalize (x - 2 * pi)
                                                  | otherwise = x
                                                -- this repeated thingy is pretty stupid :)
                                                -- (but... the sample solution does it too? ok. lol)
