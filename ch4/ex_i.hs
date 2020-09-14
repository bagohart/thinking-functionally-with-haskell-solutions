-- map (f . g) xs = map f (map g xs)
-- ^ this means: applying two functions on a list is like mapping a function on a list, and then the next.
-- this looks like the functor law thingy, so what is this about?
--
-- 1: no, because map does not evaluate the whole list eagerly
-- 2: this sounds reasonable, but I think it's wrong. let's check:
f :: Int -> Int
f _ = 5

g :: Int -> Int
g _ = undefined
-- then, both map (f . g) [1..5] and map f (map g [1..5]) evaluate to [5,5,5,5,5]
-- because
-- map (f . g) [1..5] = (f.g) 1 : map (f.g) [2..5]
-- = f ( g 1) : map ...
-- = 5 : map ... = ...
-- and
-- map f (map g [1..5]) = map f (g 1 : map g [2..5])
-- =f (g 1) : map f (map g [2..5]) = 5 : map f (map g [2..5]) = ...
-- so the key insight here seems to be that map does not evaluate eagerly, so it makes no difference

-- 3: ok, this looks even more like functor law thingy.
-- 4: um. yes?
-- 5: seems true. there are less list operations. though with lazy eval, they are mostly pipelined.
-- which, actually doesn't change the fact that there are still two list traversals =/
-- 6: yeah, I don't actually know how exactly that works under the hood...
-- 7: the questions seem to be as confused as I am.

