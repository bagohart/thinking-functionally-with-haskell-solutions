floor :: Float -> Integer
floor = read . (takeWhile (/= '.')) . show

-- This doesn't work for several reasons:
-- 12345678.0 is printed as 1.2345678e7, which gets floored to 1
-- Floating point numbers contain NaN, in Haskell obviously printed as "Infinity", which gets me a "Prelude.read: no parse" Exception
-- 0.0000000123 becomes 1.23e-8, which gets incorrectly floored to 1
