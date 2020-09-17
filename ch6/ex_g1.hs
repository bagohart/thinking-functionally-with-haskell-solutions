{-
This looks like the intermediate results from the previous task:
foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs

Show:
foldl f e . concat = foldl (foldl f) e
right side works on [[a]] and (foldl f) as (foldl (foldl f) ass) ... this looks scary
anyway, this means folding on concat is like recursive fold, which seems somewhat plausible,
except this is even correct if f is not associative, which means that this law has to uphold
identical execution order. anyway...

this looks like a task for fusion law maybe?
book says, concat can be expressed like
concat = foldr (++) []
so that gives us
    foldl f e . concat
=   foldl f e . foldr (++) []
... which is bad, because we need foldl on the right side. maaaaybe the last task can help here. hm.

foldl f e . foldr (++) [] = foldr H B
F . foldr G A = foldr H B
3 conditions:
F = foldl f e
G = (++)
A = []
1. foldl f e is strict.
2. F A = foldl f e [] = e
3. find H s.t.
F (G xs ys) = H xs (F ys)
foldl f e (xs ++ ys) = H xs (foldl f e ys)

now use the given statement to transform the left side:
foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
And we have to satisfy:
foldl f (foldl f e xs) ys = H xs (foldl f e ys)
^ left side: fold the base thing (foldl f e xs) from the left to remaining ys
^       aka (e `f` xs) `f` ys
^ right side: 
^ \xs * -> (e `f` ys) ???
this looks unsolvable. hm.

_____
... wtf the sample solution suggests induction proofs?_? let's try...
Then the statement is:
    (foldl f e . concat) xss = foldl (foldl f) e xss
<=>  foldl f e (concat xss) = foldl (foldl f) e xss

[]:
foldl f e (concat []) = foldl f e [] = e
foldl (foldl f) e [] = e

(xs:xss):
Left side:
    foldl f e (concat (xs:xss)) # concat.2
=   foldl f e (xs ++ concat xss) # property 1
=   foldl f (foldl f e xs) (concat xss) # IA
=   foldl (foldl f) (foldl f e xs) xss
Right side:
    foldl (foldl f) e (xs:xss)
=   foldl (foldl f) (foldl f e xs) xss

BAM it works. ok. hm.
-}

