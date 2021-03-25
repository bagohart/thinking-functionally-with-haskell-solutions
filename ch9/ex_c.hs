fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- This... works and is very slow

fc = 0:1: zipWith (+) fc (tail fc)
-- This... works and is fast.
