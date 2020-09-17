{-
Check:
head . map f = f . head

Induction on finite lists xs:
xs = []
head (map f []) = head [] = undefined
xs = [x]
head (map f [x]) = head [f x] = f x
x:xs ...
head (map f (x:xs)) = head (f x : map f xs) = f x

f (head []) = f undefined # only if f is strict
=   undefined
f (head [x]) = f x
f (head (x:xs)) = f x

both expressions are equal - but only if f is strict!
-}
