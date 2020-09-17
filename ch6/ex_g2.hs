{-
    Show:
    foldr f e . concat = foldr (flip (foldr f)) e

concat = foldr (++) []
=>
Show:
    foldr f e . foldr (++) [] = foldr (flip (foldr f)) e
using the fusion law.
F . foldr G A = foldr H B

F = foldr f e
G = (++)
A = []
Conditions:
1. F = foldr f e is strict. [x]
2. F A = B
F A = foldr f e [] = e
=> B = e
3. F (G xs ys) = H xs (F ys) must be satisfied.
=>
foldr f e (xs ++ ys) = H xs (foldr f e ys)
transform the left side:
foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
=> Satisfy the following
foldr f (foldr f e ys) xs = H xs (foldr f e ys)
H = \xs y -> foldr f y xs
= flip (foldr f)

=> foldr f e . concat
= foldr (flip (foldr f)) e
... it just worked? lol.

____

the sample solution suggests to use induction. let's try...
[]:
    foldr f e . concat $ []
=   foldr f e (concat [])
=   foldr f e []
=   e

    foldr (flip (foldr f)) e []
=   e

(xs:xss):
Left side:
    foldr f e . concat $ (xs:xss) # concat.2
=   foldr f e (xs ++ concat xss) # property 2
=   foldr f (foldr f e (concat xss)) xs # IA
=   foldr f (foldr (flip (foldr f)) e (concat xss)) xs

Right side:
    foldr (flip (foldr f)) e (xs:xss) # foldr.2
=   (flip (foldr f)) xs (foldr (flip (foldr f)) e xss) # flip.1
=   foldr f (foldr (flip (foldr f)) e xss) xs
... nope, this is a bit broken.

Sample solution uses flip. let's repair this attempt:
Left side:
    foldr f e . concat $ (xs:xss) # concat.2
=   foldr f e (xs ++ concat xss) # property 2
=   foldr f (foldr f e (concat xss)) xs # use flip on outer (wtf) foldr
=   flip (foldr f) xs (foldr f e (concat xss)) # IA on innermost foldr thingy
=   flip (foldr f) xs (foldr (flip (foldr f)) e xss) # flip again (?-no)
=   foldr f (foldr (flip (foldr f)) e xss) xs
^ no, that didn't work. sample solution says I should use foldr directly:
=   flip (foldr f) xs (foldr (flip (foldr f)) e xss) # foldr.2
=   ???
^ try to start from the other side:
    foldr (flip (foldr f)) e (xs:xss)
=   (flip (foldr f)) xs (foldr (flip (foldr f)) e xss)
^ ok, that was not that hard.

it seems you would get that solution by starting on both sides, and then recognizing that one must apply flip.

I actually like my solution better though. I wonder if it is correct (:
-}
