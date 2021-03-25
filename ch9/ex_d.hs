-- ... this is probably gonna be tricky. uh oh.
-- (i) list is strictly increasing
-- (ii) head list = 1
-- (iii) x in list => 2x, 3x, 5x in list
-- (iv) that's all the the numbers.

-- let's start it with an explicit number because otherwise it probably won't terminate ?_?
-- So something like
hamming :: [Integer]
hamming = 1: merge3 (map (2*) hamming) (map (3*) hamming) (map (5*) hamming)
-- this should be still wrong because it doesn't exclude duplicates which are forbidden by (i).
-- but let's see if this works
-- Also we implement merge3 directly to avoid the xmerge and foldr1 thingy which seemed overly complicated
-- and may not even be applicable here because of the order of the elements but I'm not sure about that.
merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 (x:xs) (y:ys) (z:zs) | x <= y && x <= z = x : merge3    xs  (y:ys) (z:zs)
                            | y <= x && y <= z = y : merge3 (x:xs)    ys  (z:zs)
                            | z <= x && z <= y = z : merge3 (x:xs) (y:ys)    zs

-- That... actually looks pretty good except it produces lots of duplicates. Whoa.
-- What if we just remove them afterwards?
unique :: Eq a => [a] -> [a]
unique (x:y:xs) = if x == y then unique (x:xs) else x : unique (y:xs)

hamming2 :: [Integer]
hamming2 = unique $ 1: merge3 (map (2*) hamming2) (map (3*) hamming2) (map (5*) hamming2)

-- This... is just bottom.
-- Which is probably because of unique requiring a second element which cannot be computed.
-- Let's try uniquing only the remainder because the 1 should be unique anyway:

hamming3 :: [Integer]
hamming3 = 1: (unique $ merge3 (map (2*) hamming3) (map (3*) hamming3) (map (5*) hamming3))
-- This... produces a single 1 and then bottoms. Maybe give it one more?

hamming4 :: [Integer]
hamming4 = 1:2: (unique $ merge3 (map (2*) hamming4) (map (3*) hamming4) (map (5*) hamming4))
-- This looks good except it looks a bit ugly and also it produces, unsurprisingly, a duplicate 2 at the beginning ._.

-- This gets hacky and hopefully there's a better solution, but can we simply tail the 2* list?
hamming5 :: [Integer]
hamming5 = 1:2: (unique $ merge3 (map (2*) (tail hamming5)) (map (3*) hamming5) (map (5*) hamming5))
-- This... seems to work. lol.

-- Can we move the unique to the outside and feed it with two elements? No, 2 is not enough, I just tried it. 3?
hamming6 :: [Integer]
hamming6 = unique $ 1:2:3: (merge3 (map (2*) (tail hamming6)) (map (3*) hamming6) (map (5*) hamming6))
-- ... This works and is fast.
-- It doesn't even produce a duplicate 3 because the unique apparently filters it out.
-- Note that without the `tail` in the first thingy, we get garbage,
-- then the list isn't even strictly increasing anymore. Hm.

-- What if we make the `unique` part of the `merge3`?
merge3unique :: Ord a => [a] -> [a] -> [a] -> [a]
merge3unique (x:xs) (y:ys) (z:zs)
  | x < y && x < z = x : merge3unique xs (y:ys) (z:zs)
  | y < x && y < z = y : merge3unique (x:xs) ys (z:zs)
  | z < x && z < y = z : merge3unique (x:xs) (y:ys) zs
  | x == y && y == z = x : merge3unique xs ys zs
  | x == y && x < z = x : merge3unique xs ys (z:zs)
  | x == z && x < y = x : merge3unique xs (y:ys) zs
  | y == z && y < x = y : merge3unique (x:xs) ys zs
-- this looks ugly and error-prone, but maybe it works?

hamming7 :: [Integer]
hamming7 = 1: merge3unique (map (2*) hamming7) (map (3*) hamming7) (map (5*) hamming7)
-- This... seems to in fact work. Nice. Now the hamming7 definition is quite nice.
-- Can I make the merge3unique a bit less ugly?
-- I don't see how, since it seems inevitable that I put back the elements in the single lists after looking at them.
-- Can i make it a function that works on 2 lists and apply it twice?

merge2unique :: Ord a => [a] -> [a] -> [a]
merge2unique (x:xs) (y:ys)
  | x == y = x : merge2unique    xs     ys
  | x < y  = x : merge2unique    xs  (y:ys)
  | x > y  = y : merge2unique (x:xs)    ys

hamming8 :: [Integer]
hamming8 = 1 : (((map (2*) hamming7) `merge2unique` (map (3*) hamming7)) `merge2unique` (map (5*) hamming7))
-- Now that looks nice, seems fast and correct. yay.

-- Looking at the sample solution...
-- it comes to the same conclusion, but recalls that merge2unique was already introduced at the beginning
-- of this chapter o_O
