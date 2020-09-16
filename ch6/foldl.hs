{-
[w,x,y,z] ~> (((e • w) • x) • y) • z
foldl5 :: (b -> a -> b) -> b -> [a] -> b
foldl5 _ e [] = e
foldl5 f e (x:xs) = foldl5 f (f e x) xs

fold5 f e [w,x,y,z] 
= foldl5 f e (w:[x,y,z])
= foldl5 f (f e w) [x,y,z]
= foldl5 f (f e w) x:[y,z]
= foldl5 f ((e `f` w) `f` x) [y,z]
= foldl5 f ((e `f` w) `f` x) y:[z]
= foldl5 f (((e `f` w) `f` x) `f` y) : [z]
= foldl5 f (((e `f` w) `f` x) `f` y) z : []
= foldl5 f ((((e `f` w) `f` x) `f` y) `f` z) []
= ((((e `f` w) `f` x) `f` y) `f` z)

yay it works

foldl f e xs = foldr (flip f) e (reverse xs)
foldl (@) e [a,b,c,d] = ((((e @ a) @ b) @ c) @ d)
and
foldr (flip (@)) e (reverse [a,b,c,d]) = foldr (flip @) e [d,c,b,a]
= (flip (@)) d (foldr (flip (@)) e [c,b,a])
= (foldr (flip (@)) e [c,b,a])  @ d
= ((foldr (flip (@)) e [b,a]) @ c) @ d
= (((foldr (flip (@)) e [a]) @ b) @ c) @ d
= ((((foldr (flip (@)) e []) @ a) @ b) @ c) @ d
= (((e @ a) @ b) @ c) @ d
yay it works

more relationships... todo
foldl (@) e xs = foldr (<>) e xs
^ left fold and right fold lead to the same result if
  (x <> y) @ z = x <> (y @ z)
  e @ x = x <> e
  these laws look really strange.
  like associative/commutative except the operators suddeny change o_O
  this is probably somehow because the types can change on folding

  if they don't, and <> = @ and @ associative with identity e, then we get something more intuitive:
  foldr (@) e xs = foldl (@) e xs
  in other words, associative thingys with identities can be left or right folded, doesn't matter.
  somehow, that seems obvious, when the complete result of the fold is written down...
-}
