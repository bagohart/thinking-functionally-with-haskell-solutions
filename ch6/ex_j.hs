mss :: [Int] -> Int
mss = maximum . map sum . subseqs

-- a subsequence is the list minus random elements, but the order is preserved
subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = xss ++ map (x:) xss
    where xss = subseqs xs

-- without doing any sort of calculation, the more efficient alternative for mss is obvious:
mss2 :: [Int] -> Int
mss2 = sum . filter (>0)

{-
But can we calculate/prove this?

mss 
=   maximum . map sum . subseqs
=   maximum . map sum . (subseqs ...
^ actually, using subsequs definition here makes the thing more complicated and without eliminating subseqs,
and I don't have any laws about subseqs anyway, so...???
maybe express it differently somehow? seems non-trivial. try induction then.

[]:
    mss []
=   maximum ( map sum ( subseqs []))
=   maximum ( map sum [[]] )
=   maximum ( [sum []])
=   maximum 0 = 0

this is... maybe a bit faster, so we can improve the program already a bit. maybe.:
mss3 :: [Int] -> Int
mss 3 [] = 0
-}

{-
(x:xs):
    mss (x:xs) # spec mss
=   maximum ( map sum ( subseqs (x:xs))) # subseqs.2
=   maximum ( map sum ( subseqs xs ++ map (x:) subseqs xs )) 
^ this looks like it might be going somewhere because maybe we can now compute that adding the x is not always
necessary because some magic about the maximum?
I could use # map f (xs ++ ys) = map f xs ++ map f ys (I guess)
but is that reasonable? Probably not, since I somehow need to reason that I can filter away the x, if it does not
help me with the maximum. It seems that I need to do the opposite and look at (subseqs xs vs x:subseqs xs) in together!

Let's look at this in isolation:
    maximum (map sum [xs, x:xs])
=   foldr1 max (map sum [xs, x:xs])
... I could express map as a fold. Does that help me with the fusion rule?
=   foldr1 max (foldr ((:) . sum) [] [xs, x:xs])

F . foldr G A = foldr H B
F = foldr 1 max
G = (:) . sum
A = []
Conditions:
1. foldr1 max is strict [x]
2. F A = foldr1 max [] = undefined
... this looks pretty bad. uh oh. So, no, this won't work.
-}

{-
The sample solution proposes to write subseqs as a fold. uhu? Let's try:
Actually, this isn't too hard. less thinking and more dumb pattern recognition helps to factor out the recursion:
subseqs = foldr op [[]]
with
op = (\x ys -> ys ++ map (x:) ys)
Now let's try to fusion this.
    maximum . map sum . subseqs
=   maximum . (map sum . foldr op [[]])
^ Could try fusion law on only the inner thing or both. maybe try it on inner thing only, but unclear.

F . foldr G A = foldr H B
F = map sum
G = op
    where op = (\x ys -> ys ++ map (x:) ys)
A = [[]]
Conditions:
1. map sum is strict [x]
2. F A = map sum [[]] = [sum []] = [0]
=> B = [0]
3. F (G x y) = H x (F y)
map sum (op x y) = H x (map sum y)
=> H = \a b -> b ++ map (a+) b
With H applied:
map sum (op x y) = map sum y ++ map (x+) (map sum y)
^ I would probably have to prove the identity here. (The sample solution is too lazy for this ?_?)
Actually, let's try.
-}

{-
Prove for all x and ys:
map sum (op x ys) = map sum ys ++ map (x+) (map sum ys)
    where op = (\x ys -> ys ++ map (x:) ys)
Induction this over ys:
...What do I even induction over? []? [[]]? o_O
The statement should hold for all values, so I probably need [] to cover all the values:

[]:
Left side:
    map sum (op x [])
=   map sum ([] ++ map (x:) [])
=   map sum ([] ++ [])
=   map sum []
=   []
Right side:
    map sum [] ++ map (x+) (map sum [])
=   [] ++ map (x+) (map sum [])
=   [] ++ map (x+) []
=   [] ++ []
=   []
-}

{-
(y:ys):
Left side:
    map sum (op x (y:ys))                                   # def. op
=   map sum ((y:ys) ++ map (x:) (y:ys))                     # ++.2
=   map sum (y : (ys ++ map (x:) (y:ys)))                   # map.2
=   sum y : map sum (ys ++ map (x:) (y:ys))                 # map and ++
=   sum y : (map sum ys ++ map sum (map (x:) (y:ys)))       # functor law
=   sum y : (map sum ys ++ map (sum . (x:)) (y:ys))         # map.2
=   sum y : (map sum ys ++ sum (x:y) : map (sum . (x:)) ys) #

Right side:
    map sum (y:ys) ++ map (x+) (map sum (y:ys)) # map.2 
=   sum y : (map sum ys) ++ map (x+) (map sum (y:ys)) # functor law
=   sum y : (map sum ys) ++ map ((x+) . sum) (y:ys)   #
=   sum y : (map sum ys) ++ (x+ sum y) : map ((x+) . sum) (ys) 
-}

{-
These seem almost identical. Now just prove that
sum (x:y) = (x + sum y) # should be 1 step by definition of sum
and
map (sum . (x:)) ys = map ((x+) . sum) ys
i.e.
sum . (x:) = (x+) . sum
[]:
sum . (x:) $ [] = sum [x] = x
(x+) . sum $ [] = x+0 = 0
y:ys:
    sum . (x:) $ (y:ys)
=   sum (x:y:ys)
=   x + y + sum ys

    (x+) . sum $ y:ys
=   x + y + sum ys
done.
BAM.
Note that the initial statement was proven without referring to the Induction Assumption.
I guess then it isn't really induction but whatever :)
-}

{-
Go back to the proof of whatever it was that we were doing o_O
=> 
    map sum . subseqs
=   map sum . foldr op [0]
=   foldr (\a b -> b ++ map (a+) b) [0]
^ this means I don't need to compute the subsequences, I can just add things to things directly. not so bad.

In total we get:
mss = maximum . foldr (\a b -> b ++ map (a+) b) [0]
Since the right thing is a fold, maybe we can apply the fusion law again?

F . foldr G A = foldr H B
F = maximum
G = \a b -> b ++ map (a+) b
A = [0]
Conditions:
1. F = maximum is strict
2. F A = maximum [0] = 0
=> B = 0
3. F (G x ys) = H x (F ys) for all x and ys
    maximum ((\a b -> b ++ map (a+) b) x ys) = H x (maximum ys)
<=> maximum (ys ++ map (x+) ys) = H x (maximum ys)
H = \x ysmax -> if x > 0 then x + ysmax else ysmax
or
H = \x ysmax -> max ysmax (x+ysmax)
The second thing is what the sample solution arrives at.
The final thing is then:

mss = foldr (\x m -> max x (x+m)) 0
in other words: go backwards through the list and add something if it makes it bigger.
oh well.
I still like the filter solution better (:
-}

{-
Also: the sample solution proves nothing here, although applying the fusion law seems somewhat helpful.
Can I prove the second thing, too? i.e.

    maximum (ys ++ map (x+) ys) = H x (maximum ys)
=   maximum (ys ++ map (x+) ys) = max (maximum ys) (x + maximum ys)

Let's induction this on ys. ys is never empty, has at least one element!
[y]:
Left side:
    maximum ([y] ++ map (x+) [y])
=   maximum ([y] ++ [x+y])
=   maximum ([y,x+y])
=   ... =max y (x+y)

Right side:
    max (maximum [y]) (x + maximum [y])
=   max y (x+y)

[y:ys]:
Left side:
    maximum ((y:ys) ++ map (x+) (y:ys))
=   maximum (y : (ys ++ map (x+) (y:ys))
=   maximum (y : (ys ++ (x+y) : map (x+) (ys))
^ to use the IH, I'd have to get the x+y out there, but this needs commutativity. this is a lot of work,
^ but it might get me something like
=   maximum (y : (x+y) : (ys ++ map (x+) (ys))
=   max y (max (x+y) maximum (ys ++ map (x+) ys)) # IA
=   max y (max (x+y) max (maximum ys) (x + maximum ys))
=   y `m` (x+y `m` (maximum ys `m` x + maximum ys)

Right side:
    max (maximum (y:ys)) (x + maximum (y:ys))
=   max (max y (maximum ys)) (x + max y (maximum ys))
=   (y `m` maximum ys) `m` (x + y `m` maximum ys)

And now we see it is almost equal. I would need some more subsidiary results about max and maximum.
This is a bit tiring. I'll stop here. lol.
-}

