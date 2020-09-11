{-
   Strategy: 
   treat cases of < 1, > 1 differently
   for x > 1 start with an interval of [1, x]. Then halve the interval until [m,n] with m+1=n
   for x < 1, the sqrt is also < 1 but > 0, so the result is 0

   Example run:
   sqrt 17 -> [1,18]
   -> (9.5^2 > 17) -> [1,10]
   -> (5.5 ^ 2 > 17) -> [1,6]
   -> (3.5 ^ 2 < 17) -> [3,6]
   -> (4.5 ^ 2 > 17) -> [3,5]
   -> (4^2 < 17) -> [4,5]
   -> done.
       in each step, use ceil or floor to compute the new bound

   or with integer numbers:
   sqrt 17 -> [1,18]
   -> (9^2 > 17) -> [1,9]
   -> (5^2 > 17) -> [1,5]
   -> (3^2 < 17) -> [3,5]
   -> (4^2 < 17) -> [4,5]
   -> done.
        that seems even easier.

    The result is almost identical to the approach in the book,
    but notably the condition for changing the interval is different.
-}

isqrt :: Float -> Integer
isqrt x = if x < 0 then error "isqrt of negative number is undefined" else isqrtnn x

isqrtnn :: Float -> Integer
isqrtnn x   
  | x < 1     = 0
  | otherwise = fst $ until unit (shrink x) (bound x)

unit (m, n) = m+1 == n

-- start at a higher number to compute e.g. sqrt 1.2 correctly
bound x = (1, floor(x + 1))

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if fromInteger (p^2) <= x then (p,n) else (m, p)
                     where p = choose (m, n)

type Interval = (Integer, Integer)

choose :: Interval -> Integer
choose (m,n) = (m+n) `div` 2

-- the sample solution seems to have differing ideas on which basic blocks were allowed for this function
-- so it implements a bound function that starts at 1 and doubles the value until it is above
-- otherwise the sample solution seems to have no other ideas
