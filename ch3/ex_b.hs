{-|
  2 cases:
  a ^^ (-n) = 1 / (a^n) # recurse on ^
  a ^^ n = a^n # recurse on ^
-}

-- write new ^^ as ^^^
(^^^) :: (Fractional a, Integral b) => a -> b -> a
(^^^) x n = if n > 1 then x ^ n else 1 / (x ^ (-n))
