{-
fusion law: 
    f . foldr g a = foldr h b
    ^ holds for conditions 1-3

Use fusion law to prove the following:
    foldr f a . map g = foldr h a
        ^ "I can fold the list directly, or I can first map the list on to something else,
            and then fold it with another function and _the same_ base element"

To prove this, recall map g = foldr ((:) . g) []
Then
foldr f a . map g = foldr h a
<=>
foldr f a . foldr ((:) . g) [] = foldr h a

Condition 1: f strict => (foldr f a) is a strict function ?
(foldr f a) undefined = f undefined (foldr f a undefined)

Condition 2: f a = b => (foldr f a) [] = a ?
^ yes, this holds by definition

Condition 3: f (g x y) = h x (f y) for all x and y => (show)
(foldr f a) (((:) . g) x y) = h x ((foldr f a) y)
foldr f a ((g x) : xs) = h x (foldr f a xs)
foldr f a ((g x) : xs) ~> f (g x) (foldr f a xs)
=> f (g x) y = h x y
=> h = f . g
(g slurps the first argument, f takes the second)
foldr f a . map g = foldr (f . g) a
^ which is a more precise (and actually only now correct) way of saying the first statement:
  "you can map first and then fold, or apply the function as part of/right before the fold"
  this is... indeed why fold can be used to define map.

-}
