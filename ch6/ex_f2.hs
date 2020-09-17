{-
  Prove:
  foldl (@) e xs = foldr (<>) e xs
  for all finite lists where
  (x <> y) @ z = x <> (y @ z)
  e @ x = x <> e

  In other words: foldl and foldr give the same result if the operations are similar and sorta associative. lol.

INDUCTION!!!!!!!!!11111
[]:
foldl (@) e [] = e
foldr (<>) e [] = e

(x:xs):
    foldl (@) e (x:xs) # foldl.2
=   foldl (@) (e @ x) xs # IA
=   foldr (<>) (e @ x) xs # e @ x = x <> e
=   foldr (<>) (x <> e) xs

    foldr (<>) e (x:xs) # foldr.2
=   x <> (foldr (<>) e xs) # IA
=   x <> (foldl (@) e xs)

... these look different. uh oh.
companion results from the last task where:
foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
and
foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
maybe I can use those?
    foldl (@) e (x:xs) # foldl.2
=   foldl (@) e ([x] ++ xs)
=   foldl (@) (foldl (@) e [x]) xs
=   foldl (@) (e @ x) xs
^ this is just the definition for one element. so, no.

Let's look at the sample solution:
    foldl (@) e (x:xs) # foldl.2
=   foldl (@) (e @ x) xs # e@x = x<>e
=   foldl (@) (x <> e) xs

    foldr (<>) e (x:xs) # foldr.2
=   x <> (foldr (<>) e xs) # IA
=   x <> (foldl (@) e xs)

hm, this is what I had, too.
The sample solution proposes to just induction this away, too. lol?
So, show: (for any e)

foldl (@) (x <> y) xs = x <> (foldl (@) y xs)
[]:
foldl (@) (x <> y) [] = x <> y
x <> (foldl (@) y []) = x <> y

(z:zs):
    foldl (@) (x <> y) (z:zs) # foldl.2
=   foldl (@) ((x <> y)@z) zs # <>, @ "associative"
=   foldl (@) (x <> (y@z)) zs # IA
=   x <> (foldl (@) (y@z) zs)

    x <> (foldl (@) y (z:zs))
=   x <> (foldl (@) (y@z) zs)
BAM

Actually, inductioning away the different sides seems quite sensible.
I think this says basically, that you can move one element from the very left to the very right
because "associative".
-}
