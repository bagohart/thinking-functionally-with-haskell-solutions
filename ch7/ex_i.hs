data BinTree a = Leaf a | Fork (BinTree a) (BinTree a) deriving (Show, Eq)

t1 :: BinTree Int
t1 = Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4))

labels0 :: BinTree a -> [a]
labels0 (Leaf x) = [x]
labels0 (Fork u v) = labels0 u ++ labels0 v
-- this is straightforward and nice, but slow :(
--
-- T(labels0)(1) = O(1) (theta...)
-- T(labels0)(2*n) = T(labels0)(n) + T(labels0)(n) + O(n) <- the last thing is the append. I hope.
-- = 2 * T(labels0)(n) + O(n)
-- = ... probably something logarithmic. O(n * log n) or something. The term is a bit smaller than in exercise e,
-- but probably only a constant factor 2.
-- I could master theorem this and stuff, but...
-- ^ this calculation is for perfect balanced binary trees.
--
-- It gets worse, because it doesn't need to be balanced:
-- If for n leaves, the right subtree is always a Leaf, i.e.
--                          x
--                      x        x
--                   x    x
--                 x   x
--
-- T(labels0)(1) = O(1)
-- T(labels0)(n+1) = T(labels0)(n) + O(n)
-- sum this up and we get
-- => T(labels0)(n) = O(n^2)
--
-- Now let's make it faster using accumulating parameter technique:
-- The given parameter should be added to the final result of labels, so
labels1 t = labacc1 t []

labacc1 :: BinTree a -> [a] -> [a]
labacc1 (Leaf x) acc = x : acc -- it doesn't get simpler than this, I think.
labacc1 (Fork u v) acc = labacc1 u (labacc1 v acc)
-- ^ this is obviously correct: it goes from right to left through the tree und only ever sticks
-- things to the front of the list.
-- Now can I derive this intuitive result via calculating / reasoning?
--
-- labacc1 (Fork u v) acc
-- { Def. }
-- = labels0 u ++ labels0 v ++ acc
-- { Def. labacc1 }
-- = labels0 u ++ (labacc1 v acc) { Def. labacc1 }
-- = labacc1 u (labacc1 v acc)
--
-- or something.
-- Can I prove it's faster?
-- Again, using the worst case from above:
--
-- T(labacc1)(1,m) = O(1) -- the Leaf case.
-- T(labacc1)(n+1,m) = T(labacc1)(1,m+n) + T(labacc1)(n,m)
-- = O(1) + T(labacc1)(n,m)
-- solve this =>
-- T(labacc1)(n,m) = O(n)
-- yay.

-- Part 2: prove that labels (build xs) = xs for all finite nonempty lists xs
build :: [a] -> BinTree a
build [x] = Leaf x
build xs = Fork (build ys) (build zs)
    where (ys,zs) = halve xs

halve xs = (take m xs, drop m xs) where m = length xs `div` 2

labels :: BinTree a -> [a]
labels (Leaf x) = [x]
labels (Fork u v) = labels u ++ labels v

-- Try to use induction without thinking?
-- IB:
-- labels (build []) = error.
-- labels (build [x]) = labels (Leaf x) = [x]
-- IS: (use (x1:x2:...xn:xs) with n = length xs
-- labels (build (x1:x2:...xn:xs))
-- = labels (Fork (build [x1...xn]) (build xs))
-- { Def. labels }
-- = labels (build [x1...xn]) ++ labels (build xs)
-- { IH }
-- = [x1...xn] ++ xs
-- { Def. ++ }
-- = x1:x2: ... : xn : xs
--
-- The sample solution does the same thing, calling it general induction, and says the IH is that
-- the statement holds for all smaller lists.
-- I feel a bit uneasy about this o_O
