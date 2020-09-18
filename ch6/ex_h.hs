{-
  sum (scanl (/) 1 [1..])
= sum
1
(1 / 1)
(1 / 1) / 2
((1 / 1) / 2) / 3
(((1 / 1) / 2) / 3) / 4
...
= (n -> inf) sum (i=1..n) (1/ 1*2*...*n)


(((1 * 1) * (1/2)) * (1/3)) * (1/4)
= (1/2) * (1/3) * (1/4)
= 1 / (prod 1 .. n)
= 1 / (n!)

This corresponds to the popular definition of e as
e = sum {over k=0 to inf} (1/k!)
-}
