{-
        y ~= sqrt x
    =>  x/y ~= sqrt x and (y <= sqrt x <= x/y or x/y <= sqrt x <= y)
    To find an approximation that is better than either y or x/y: (y+x/y)/2 which is in the middle of the interval.
    Reason: Not sure o_O

    good enough approximation: just use 10^-6 or 10^-5 or something.
    The absolute test would be useful, if the precision would be absolute, but since the comma can move around in
    floating point numbers, this is unsuitable here.
-}

sqrt' :: Float -> Float
sqrt' x = until (closeEnough x) (improve x) x

improve :: Float -> Float -> Float
improve x y = (y + (x/y)) / 2

epsilon :: Float
epsilon = 1.0e-6

closeEnough :: Float -> Float -> Bool
closeEnough x y = abs (y^2 - x) < epsilon * x

-- the sample solution inlines all uses of x and epsilon and uses where clauses to define closeEnough and improve
-- apart from that it seems to be basically identical
