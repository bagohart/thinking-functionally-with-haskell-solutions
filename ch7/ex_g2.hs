-- Polluted namespace, continue here.
f = (+)
g = (*)

h (y,z) x = (f y x, g z x)

xs = [1..5]

r1 = (foldl f 0 xs, foldl g 1 xs)
r2 = foldl h (0,1) xs

-- yay, this works.
-- The lazify trick doesn't work however:
f' x y = 42
g' x y = 23
h' t x = (f' (fst t) x, g' (snd t) x)
h'' (y,z) x = (f' y x, g' z x)

bot = [1..]
r1' = (foldl f' 0 xs, foldl g' 1 xs)
r2' = foldl h' (0,1) bot

r1'' = (foldl f' 0 xs, foldl g' 1 xs)
r2'' = foldl h'' (0,1) bot

-- Because with ((x0 • x1) • x2) • ... xn
-- the evaluation will go to the outermost • which is at the end of the list, which is never reached.
