cp1 :: [[a]] -> [[a]]
cp1 [] = [[]]
cp1 (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp1 xss

-- that was not so hard. when you recognize the pattern, basically no changes are needed
cp2 :: [[a]] -> [[a]]
cp2 = foldr g [[]]
    where g = \xs yss -> [x:ys | x <- xs, ys <- yss]


{-
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp xss

Find cp2 such that
cp2 = foldr f e
ok, that was easy.

Identity proof:
lenth . cp = product . map length
^ aka: the number of things in the cross product is the product of the list lengths
^ this is, like, obvious? hm.

1. Use fusion theorem to express length . cp as instance of foldr
The fusion law is
f . foldr g a = foldr h b
obviously, f = length, and we have
length . foldr f (\xs yss -> [x:ys | x <- xs, ys <- yss]) [[]] = foldr h b
Conditions:
1) f is strict? length is strict, since length undefined = undefined
2) f a = b ? length [[]] = b, so b = 1
3) f (g x y) = h x (f y) for all x and y
let's see...
    g = \xs yss -> [x:ys | x <- xs, ys <- yss]
    length (g xs yss) = h xs (length yss)

here, yss are the crossproducts of the remaining list.
    obviously, h needs to multiply the things...
    h = \xs lyss -> length xs * lyss
    Can I prove this?

length (g xs yss) = length [x:ys | x <- xs, ys <- yss]
= length [[x1 _ ys1]..[xn _ ys1] .. [x1 _ ysm] .. [xn _ ysm]]
= ... this is obviously length xs * length yss, but how to prove this?

2. Express map length as foldr instance
This is... not so hard, since map can be expressed in terms of foldr as everyone knows?
    map length = foldr (\xs yss -> length xs : yss) []

3. Express product . map length as foldr, using Fusion
Use result of last step to get:
    product . map length
=   product . foldr (\xs yss -> length xs : yss) []

Fusion law, we get: f . foldr g a = foldr h b
f = product
a = []
g = (\xs yss -> length xs : yss)
product (g x ys) = h x (product ys)

Conditions:
1. product is strict, yay.
2. product [] = 1 => b = 1
3. h = (\xs lyss -> length xs * lyss)

4.
Left side:
length . cp = foldr (\xs lyss -> length xs * lyss) 1
Right side:
product . map length = foldr (\xs lyss -> length xs * lyss) 1

BAM

The sample solution has the same approach, except it uses pointfree style to define
h = (*) . length
and
f = (:) . length
-}
