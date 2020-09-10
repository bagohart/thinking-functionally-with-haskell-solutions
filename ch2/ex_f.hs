exp1 :: Integer -> Integer -> Integer
exp1 x n |  n == 0      = 1
         |  n == 1      = x
         |  otherwise   = x * exp1 x (n-1)

-- useful laws here:
-- x^2m = (x^2)^m
-- x^2m+1 = x(x^2)^m
exp2 :: Integer -> Integer -> Integer
exp2 x n |  n == 0 = 1
         |  n == 1 = x
         |  even n = exp2 (x * x) (n `div` 2)
         |  odd n  = x * exp2 (x * x) (n `div` 2)

-- how many multiplications does this take? at every step it takes either 1 or 2 multiplications, until n is 1 or 0.
-- could be uneven after each step:
-- 31 / 2 = 15 / 2 = 7 / 2 = 3 / 2 = 1
-- so that's approximately 2 * (log 2) n or something
