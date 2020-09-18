{-
Spec for scanr:
scanr f e = map (foldr f e) . tails

Reminder:
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f e = map (foldl f e) . inits

and

Then, an induction proof was used to arrive at
scanl f e [] = [e]
scanl f e (x:xs) = escanl f (f e x) xs

Let's try the same method here. Reminder:

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

[]:
    scanr f e []
=   map (foldr f e) . tails $ []
=   map (foldr f e) (tails [])
=   map (foldr f e) ([[]])
=   foldr f e [] : map (_) []
=   e : []
=   [e]

(x:xs):
    scanr f e (x:xs) # def. scanr
=   map (foldr f e) . tails $ (x:xs) # $ and .
=   map (foldr f e) (tails (x:xs)) # tails.2
=   map (foldr f e) ((x:xs) : tails xs) # map.2
=   (foldr f e) (x:xs) : (map (foldr f e) tails xs) # def. scanr
=   (foldr f e) (x:xs) : (scanr f e xs)

... so I could write scanr like this:
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e [] = [e]
scanr f e (x:xs) = foldr f e (x:xs) : (scanr f e xs)

but then I don't get any benefit, since I have only inlined the definitions :)
what I actually want is to compute (foldr f e xs) only once, since
foldr f e (x:xs) = x `f ` foldr f e xs
let's see...

=   (foldr f e) (x:xs) : (map (foldr f e) tails xs) # foldr.2
=   f x (foldr f e xs) : (map (foldr f e) tails xs) # 
let's consider this as a separate statement, and induction it on xs.

[]:
    f x (foldr f e xs) : (map (foldr f e) tails xs) # 
=   f x (foldr f e []) : (map (foldr f e) tails [])
=   (f x (foldr f e [])) : (map (foldr f e) [[]])
=   (f x (foldr f e [])) : (foldr f e) [] : []
=   (f x m) : m : []
    where m = (foldr f e []) = foldr f e xs

(y:ys):
    f x (foldr f e xs) : (map (foldr f e) tails xs) # 
=   f x (foldr f e (y:ys)) : (map (foldr f e) tails (y:ys)) # 
=   f x (foldr f e (y:ys)) : (map (foldr f e) (y:ys) : (tails ys))
=   f x (foldr f e (y:ys)) : foldr f e (y:ys) : map (foldr f e) (tails ys) # extract m
=   f x m : m : map (foldr f e) (tails ys)
    where m = (foldr f e (y:ys)) = foldr f e xs

This looks better, although doing it like this feels a bit like cheating o_X
It gives us the following definition:
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f e [] = [e]
scanr f e (x:[]) = (f x m) : m : []
scanr f e (x:y:ys) = f x m : m : map (foldr f e) (tails ys)
    where m = (foldr f e (y:ys))

Actually, this is going nowhere. The expression map (foldr f e) (tails ys)
does not reuse the previous result, so...

Take a step back.
The book, for scanl, uses a subsidiary claim about foldl.
Maybe, I need to do something about foldr here, that lets me reuse the things.

...
=   (foldr f e) (x:xs) : (map (foldr f e) tails xs) # 
...
Actually what should be the result?
[a,b,c]
[a@(b@(c@e)), b@(c@(e)), c@(e), e]
[]
[e]
so, take the first element, compute it onto the next element, prepend it to the list, compute the rest list

Something like this:

scanr f e [] = [e]
scanr f e (x:xs) = f x next : rest
    where next = head rest
          rest = scanr f e xs

Here, the rest is computed only once, which is what we wanted.
The head thing could be expressed as patterns, but having a function for it may be more suitable for the
equational reasoning thing. speaking of which...
I don't know, let's look at the solution...

(x:xs):
    scanr f e (x:xs) # spec. scanr
=   map (foldr f e) . tails $ (x:xs) # $ and .
=   map (foldr f e) (tails (x:xs)) # tails.2
=   map (foldr f e) ((x:xs) : tails xs) # map.2
=   (foldr f e) (x:xs) : (map (foldr f e) tails xs) # spec. scanr
=   (foldr f e) (x:xs) : scanr f e xs # foldr.2
=   f x (foldr f e xs) : scanr f e xs # claim: foldr f e xs = head (scanr f e xs)
=   f x (head (scanr f e xs) : scanr f e xs # use where
=   f x (head ys) : ys
    where ys = scanr f e xs

Actually, I was *almost* done.
Let's prove the subsidiary claim:
foldr f e xs = head (scanr f e xs)

[]:
    foldr f e [] = e
and
    head (scanr f e []) # spec. scanr
=   head (map (foldr f e) . tails $ [])
=   head (map (foldr f e) [[]])
=   head (foldr f e [] : [])
=   head (e:[])
=   e

(x:xs):
    foldr f e (x:xs)
and
    head (scanr f e (x:xs)) # spec. scanr
=   head (map (foldr f e) . tails $ (x:xs))
=   head (map (foldr f e) (x:xs) : tails xs)
=   head (foldr f e (x:xs) : map (foldr f e) tails xs)
=   foldr f e (x:xs)
    
ok, that was easy. But I'm not really convinced that the equational reasoning was so helpful here.
I could indeed prove that the more efficient version is correct,
but the key insight that lead to the subsidiary claim occurred because I thought about what the result should be,
dumb reasoning did NOT lead to it =/
Maybe I had higher expectations on this than justified.
Actually, the book spells out what is nice about this:
"There s no need to bring in a totally different logical language to reason about programs."
Fair enough, I guess.

-}

-- scanr f e = map (foldr f e) . tails
