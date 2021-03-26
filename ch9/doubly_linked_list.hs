data DList a = Cons a (DList a) (DList a)

instance Show a => Show (DList a)
    where show d = show (elemm d)

elemm :: DList a -> a
elemm (Cons a p n) = a

prev,next :: DList a -> DList a
prev (Cons a p n) = p
next (Cons a p n) = n

p1 = Cons "Page 1" p3 p2
p2 = Cons "Page 2" p1 p3
p3 = Cons "Page 3" p2 p1

book = [p1,p2,p3]

mkCDList :: [a] -> DList a
mkCDList as = head xs
    where xs = zipWith3' Cons as (rotr xs) (rotl xs) -- this is so cyclic.

-- this is like zipWith3, but lazier.
zipWith3' f (x:xs) ys zs = f x (head ys) (head zs) : zipWith3' f xs (tail ys) (tail zs)
zipWith3' _ _ _ _ = []

-- should satisfy
-- map elem xs = as -- I can retrieve all the original elements again
-- map prev xs = rotr xs
-- map next xs = rotl xs

rotr xs = [last xs] ++ init xs -- put the last element to the front
rotl xs = tail xs ++ [head xs] -- put the first element at the end

-- for any list xs of doubly-linked lists we have
-- xs = zipWith3 Cons
--     (map elem xs) (map prev xs) (map next xs)
