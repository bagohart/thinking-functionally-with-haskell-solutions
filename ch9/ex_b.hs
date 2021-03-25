-- avoid nameclash with prelude
myCycle :: [a] -> [a]
myCycle [] = error "cycling the void is, like, totally forbidden, yo"
myCycle xs = cs where cs = xs ++ cs
-- this seems like it passes all the tests, should be cyclic and works for infinite lists, obviously.
-- I wonder though if this repeated ++ is slow?
-- the sample solution doesn't say anything about this. hm.
