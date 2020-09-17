{-
mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ x) y = mult x y + y

0*y = 0 # ok.
(1+x)*y = x*y + y # ok.

To show:
mult (x+y) z = mult x z + mult y z # aka distribution over multiplication: (x+y)*z = x*z + y*z

Try induction on y, since that seems to be where mult is defined.
Base case: y = 0
mult (x+y) z = mult (x+0) z = mult x z
and
mult x z + mult y z = mult x z + mult 0 z = mult x z + 0 = mult x z
Induction assumption: Let the statement be true for some y in |N.
Induction step:
    mult (x+(y+1)) z # (+) associative
=   mult ((x+y)+1) z # definition of mult
=   mult (x+y) z + z # IA
=  ((mult x z) + (mult y z)) + z

    mult x z + mult (y+1) z # def. mult
=   mult x z + ((mult y z) + z)

both final terms are equal because (+) is associative
-}
