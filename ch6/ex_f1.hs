{---
Prove: foldl f e xs = foldr (flip f) e (reverse xs) for all finite lists xs.
In other words: folding from the right or the left gives the same result if the list and function are reversed properly.
This does not seem to surprising. Let's induction it:
IB: xs = []
Left side:
foldl f e [] = e
Right side:
foldr (flip f) e (reverse []) = foldr (flip f) e [] = e

IS: May be statement hold for some xs
Left side:
    foldl f e (x:xs) = foldl f (f e x) xs # IA
= foldr (flip f) (f e x) (reverse xs)
Right side:
    foldr (flip f) e (reverse (x:xs)) # rev.2
=   foldr (flip f) e (reverse xs ++ [x])
= ... this seems to be going nowhere. let's try the other definition of reverse

    foldr (flip f) e (reverse (x:xs)) # rev.2
=   foldr (flip f) e (foldl (flip (:)) [] (x:xs)) # foldl.2
=   foldr (flip f) e (foldl (flip (:)) (x:[]) xs) # list sugar
=   foldr (flip f) e (foldl (flip (:)) [x] xs) # IA
=   foldr (flip f) e (foldr (flip (flip (:))) [x] (reverse xs)) # flip . flip = id
=   foldr (flip f) e (foldr (:) [x] (reverse xs))
... this looks correct, but not really helpful. I'll look for another law to apply.

Let's try this law:
foldr f e (xs ++ ys) = foldr f e xs @ foldr f e ys
and the first definition of reverse, that uses (++)
Right side:
    foldr (flip f) e (reverse (x:xs)) # rev.2
=   foldr (flip f) e (xs ++ [x])
=   foldr (flip f) e xs @ foldr (flip f) e [x]
where e @ x = x
and   f x (y @ z) = f x y @ z
Does that hold? e is any value, we can't choose that.
So we must choose @ such that e is its identity.
this seems infeasible =/

Yet another attempt: reverse can be expressed (though inefficiently) as a right fold:
reverse = foldr snoc []
    where snoc x xs = xs ++ [x]
then, we might be able to use fusion law.
uh oh.
    foldr (flip f) e (reverse (x:xs)) # rev.2
=   foldr (flip f) e (foldr (\x xs -> xs ++ [x]) [] (x:xs))

We have F . foldr G A = foldr H B
with F = foldr (flip f) e
G = (\x xs -> xs ++ [x])
A = []

Conditions:
1. F is strict ? yes, a fold on undefined lists crashes. [x]
2. F A =  B ? yes, for B = e
    foldr (flip f) e [] = e
3. foldr (flip f) e . foldr (\x xs -> xs ++ [x]) [] = foldr H e
^ we need for this
F (G x y) = H x (F y) for all x and y, so
    foldr (flip f) e (\x xs -> xs ++ [x])
=   foldr (flip f) e (xs ++ [x]) = H x (foldr (flip f) e xs)
hm...
this should be true for
H = \x y -> (flip f) y x
  = \x y -> f x y
  = f
  o_O
Then,
foldr (flip f) e . foldr (\x xs -> xs ++ [x]) []  = foldr f e
o_O
Was this intended?
Continue with the proof attempt...

    foldr (flip f) e (reverse (x:xs)) # rev.2
=   foldr (flip f) e (foldr (\x xs -> xs ++ [x]) [] (x:xs))
=   foldr f e (x:xs)
... this seems wrong. like, complete garbage. (although only off by one letter, coincidentally?) hhhmmmm...

give up and consult the sample solution:
1. Define g = flip f
Show foldl f e xs = foldr g e (reverse xs) by induction:
[]:
foldl f e [] = e
foldr g  (reverse []) = foldl g e [] = e
(x:xs):
    foldl f e (x:xs)
=   foldl f (f e x) xs # IA
=   foldr g (f e x) (reverse xs)
(ok, I had that...)

    foldr g e (reverse (x:xs))  # rev.2 naive and direct recursive definition (hm... I was there)
=   foldr g e (reverse xs ++ [x]) # magic claim (wtf)
=   foldr g (foldr g e [x]) (reverse xs) # def. g and foldr
=   foldr g (f e x) (reverse xs)

Magic claim is:
foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
^ this basically says that a fold can be "stopped" in the middle
I knew that I needed this, but I did't realize that I could just take this claim and prove that separately o_O
Maybe, that is. Let's try induction on the magic claim:
[]:
foldr f e ([] ++ ys) = foldr f e ys
foldr f (foldr f e ys) [] = foldr f e ys

(x:xs):
    foldr f e ((x:xs) ++ ys) # ++.2
=   foldr f e (x:(xs ++ ys)) # foldr.2
=   f x (foldr f e (xs ++ ys)) # IA
=   f x (foldr f (foldr f e ys) xs)

    foldr f (foldr f e ys) (x:xs) # foldr.2
=   f x (foldr f (foldr f e ys) xs)
BAM

The companion result is:
foldl f e (xs ++ ys) = foldl f (foldl f e xs) ys
^ which is basically the same statement.
Let's prove this, too:
[]:
foldl f e ([] ++ ys) = foldl f e ys
foldl f (foldl f e []) ys = foldl f e ys
(x:xs):
    foldl f e ((x:xs) ++ ys) # ++.2
=   foldl f e (x:(xs++ys)) # foldl.2
=   foldl f (f e x) (xs++ys) # IA
=   foldl f (foldl f (f e x) xs) ys

    foldl f (foldl f e (x:xs)) ys # foldl.2
=   foldl f (foldl f (f e x) xs) ys
That was... not harder than the first statement.
---}
