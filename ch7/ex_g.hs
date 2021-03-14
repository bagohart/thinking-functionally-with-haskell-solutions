-- Prove:
-- h x (y,z) = (f x y, g x z)
-- => for all finite lists xs:
-- (foldr f a xs, foldr g b xs) = foldr h (a,b) xs
--
-- Idea of this statement: doing two distinct folds on a list means traversing the list twice.
-- disadvantage: double traverse + space leak if then the list xs is kept in memory
-- Instead traverse it only once and fold both things, so instead of:
-- [x1 ... xn]
-- ->
-- x1 `f` (x2 `f` ... (xn `f` a))
-- x1 `g` (x2 `g` ... (xn `g` a))
-- Do this:
-- [x1 ... xn]
-- ->
-- x1 `h` (x2 `h` ... (xn `h` (a,b)))

-- Try to prove the statement using induction.
-- Statement to prove:
-- h x (y,z) = (f x y, g x z)
-- => for all finite lists xs:
-- (foldr f a xs, foldr g b xs) = foldr h (a,b) xs
--
-- IB: xs = []
-- foldr h (a,b) xs = foldr h (a,b) [] = (a,b)
-- (foldr f a xs, foldr g b xs) = (foldr f a [], foldr g b []) = (a,b)
--
-- IS: xs' = x:xs
-- foldr h (a,b) xs'
-- { Def. xs' }
-- = foldr h (a,b) (x:xs)
-- { Def. foldr }
-- = x `h` (foldr h (a,b) xs) = ...
-- { IS }
-- = x `h` (foldr f a xs, foldr g b xs) 
-- { Assumption }
-- = (f x (foldr f a xs), g x (foldr g b xs))
-- { Def. foldr twice }
-- = (foldr f a (x:xs), foldr g b (x:xs))
-- = (foldr f a xs', foldr g b xs')
-- DONE.
-- The sample solution computes it backwards, otherwise it seems identical.

-- Does the result hold for all lists xs? i.e. what happens if
-- a) xs is undefined?
-- b) xs is infinite?
--
-- a) if xs is undefined, f is short circuiting, g is not, then (foldr f a xs, foldr g b xs) = (x', bot)
-- but foldr h (a,b) xs keeps evaluating the first argument in the tuple, although it will always yield the same result.
-- Since f is short circuiting on the first argument, it doesn't matter what happens later.
-- As a result, we get bot, which is not even (bot,bot) actually.
--
-- The sample solution suggests using foo (x,y) = 1, aka a constant function to distinguish between bot and (bot,bot).
-- I'm not sure this is getting at the same thing as I am. Let's check...
-- Reminder:
-- h x (y,z) = (f x y, g x z)
-- => for all finite lists xs:
-- (foldr f a xs, foldr g b xs) = foldr h (a,b) xs
f x y = 1
g x y = x + y
h x (y,z) = (f x y, g x z)
i x (y,z) = (f x y, f x z)

bot = [1..]

r1 = foldr h (0,0) bot
r1' = foldr i (0,0) bot
r2 = (foldr f 0 bot, foldr g 0 bot)
-- okay, so:
-- foldr i (a,b) keeps evaluating the whole list. not sure why though? Let's evaluate:
-- foldr i (0,0) [1..]
-- = i 1 (foldr i (0,0) [2..])
-- = i 1 (i 2 (foldr i (0,0) [3..]))
-- ...
-- ok, so i needs to force the computation of the tuple, and this is exactly what should be skipped.
-- The issue here is that i never tries to apply f.
-- But this seems fixable, by not forcing evaluation of the tuple:
i2 x t = (f x (fst t), f x (snd t))
r1'' = foldr i2 (0,0) bot
-- ^ yeah, this actually works.
-- So, is it now the same?
h2 x t = (f x (fst t), g x (snd t))
r1''' = foldr h2 (0,0) bot
-- it seems so, this yields (1,bot) which is the same as
-- (foldr f 0 bot, foldr g 0 bot)
-- hm.

