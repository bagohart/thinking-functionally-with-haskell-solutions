{-
Show:
reverse (xs ++ ys) = reverse ys ++ reverse xs
for all finite lists

Problem: unclear which definition. use the first (naive) one:
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

The statement seems true:
reverse [1,2,3,4] = reverse [3,4] ++ reverse [1,2] = [4,3] ++ [2,1]

Induction on xs.
IB:
reverse ([] ++ ys) = reverse ys
reverse ys ++ reverse xs = reverse ys ++ [] = reverse ys
IS:
    reverse ((x:xs) ++ ys) # (++).2
=   reverse (x: (xs ++ ys)) # reverse.2
=   reverse (xs ++ ys) ++ [x] # IA
=   (reverse ys ++ reverse xs) ++ [x]

    reverse ys ++ reverse (x:xs) # reverse.2
=   reverse ys ++ (reverse xs ++ [x])

because of associativity, both terms are equal.
-}
