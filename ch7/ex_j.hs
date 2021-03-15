-- selects the k-th smallest element in an array, the 0-th being the smallest.
select0 k = (!!k) . sort

-- original 'quicksort'.
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++
              sort [y | y <- xs, y >= x]

-- todo: find a faster select.
-- why is select0 slow? Probably because if the big elements are at the beginning, then they are shuffled around repeatedly,
-- although they are not actually important for the end result.
-- Though maybe that's nonsense, in every step all the things are shuffled around, so... ?_?
-- The quicksort thing is quadratic. what could we actually hope for?
-- Finding the smallest element is possible in O(n).
-- Finding the k-th smallest element would probably entail something like:
-- • traverse the list once from left to right
-- • Save the k smallest elements, possibly in inverted order.
-- • For every new element, try to save it, but don't if it's too big.
-- Do we actually have to save all the k greatest things? Maybe not:
-- • Save the first element, and declare it the 0-th smallest.
-- • Iterate the whole list. every time, a smaller one comes along, declare it the n+1-th smallest.
-- At the end, we have ... the index of the first element in the sorted array. that seems pretty useless =/
-- So try to implement the first idea:

select1 :: Ord a => Int -> [a] -> a
select1 k xs = select1' k xs []

-- idea: The second parameter is an accumulating parameter, saving the current max k smallest elements.
select1' :: Ord a => Int -> [a] -> [a] -> a
select1' k [] acc = last acc
select1' k (x:xs) acc = select1' k xs (take (k+1) (sort (x:acc)))
-- This seems to work fine and looks obviously correct (ok, I tripped over an off-by-one...)
-- but is it fast? not sure:
-- T(select1')(0,m) = O(m)
-- T(select1')(n,m) = T(select1')(n-1,m+1) + T(sort)(m+1) <- but in a mostly sorted list.
-- this seems stupid - for k = length xs, I would still have to sort the whole list,
-- and the analysis would pretend that I even have to do it again and again, which is sorta true o_O
-- But then it doesn't really matter how I implement this, I always end up with n^2 if I sort the whole thing using quicksort =/
--
-- Anyway, the task tells me to calculate something. ...
-- select k = (!!k) . sort
-- select k [x1 ... xk]
-- = ... this isn't going anywhere?
--
-- Let's cheat -_-
--
-- The sample solution tells me that this is a key property:
-- (xs ++ [x] ++ ys) !! k
--  | k < n = xs !! k
--  | k == n = x
--  | k > n = ys !! (n-k)
--  where n = length xs
--  This doesn't say anything about being sorted yet.
--
-- Also...
-- k = 5, n = 4
-- k' = 4-5 = -1.
-- This looks wrong.
-- And everything in here is prone to off-by-ones. uh oh.
--
-- xs=[0,1,2,3] x=4 ys=[5,6,7]
--
-- k = 4 -> x
-- k < x -> xs !! k
-- k = 5 -> ys !! (5-|xs|-1) -> ys !! 0
-- 
-- k < n -> 
--
--  ...Where is this going? Maybe some recursion thing such as:
select2 :: Ord a => Int -> [a] -> a
select2 0 xs = head . sort $ xs
select2 k xs = select2 (k-1) (removeSmallest xs)

removeSmallest :: Ord a => [a] -> [a]
removeSmallest = undefined
-- ... no, not really. It seems that the author didn't just 'calculate' either.

-- What is actually happening is we take the result from above and use the known 'quicksorty' approach to split the list at the front element...
-- so (x:xs) -> ys ++ [x] ++ [zs]
-- where, and this seems to be the thing here, we don't need to sort ys and zs! The position of x is now fixed,
-- so we can continue to search only in the relevant part of just stop
select3 k [] = error "list too short :'("
select3 k (x:xs)  | k < n = select3 k ys
                  | k == n = x
                  | k > n = select3 (k-(n+1)) zs
    where ys = [y | y <- xs, y < x]
          zs = [z | z <- xs, z >= x]
          n = length ys

-- sample solution was off by one broken because it forgot the x and switched the n / k.
-- Otherwise it is pretty nice. Can we time it?
-- The worst case is, again, if the first element is greatest but not what we want, so we build a complete new list
-- I.e. we want the smallest element with k=0, but the list is in inverted order.
-- In every step we compute ys with length |xs|, and then recurse in this list.
-- Building ys takes O(n), and we have to do this n times. We stop when xs = []
-- T(select)(1) = O(1)
-- T(select)(n+1) = O(n) + T(select)(n)
-- => T(select)(n) = O(n^2)
--
-- The sample solution also suddenly starts to talk about "reasonable distributions", i.e. assume that
-- the list is shuffled before select is called.
-- This is supposed to be in O(n). Why?
-- I'm not at all sure - In the first step I need to compute ys, and that takes O(n) already.
-- Maybe this evaluation uses some black magic from lazy evaluation, but this would contradict earlier computations. ... ?_?
