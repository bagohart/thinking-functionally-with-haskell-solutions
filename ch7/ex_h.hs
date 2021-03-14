-- Recall
partition0 p xs = (filter p xs, filter (not . p) xs)

test0 = partition0 (>4) [1..10]

-- Express the two tuple components using foldr:
t1 p xs = foldr (\x ys -> if p         x then x:ys else ys) [] xs
t2 p xs = foldr (\x ys -> if (not . p) x then x:ys else ys) [] xs

-- Now use the result of Ex. 7.g to calculate another definition for partition:
partition1 p xs = foldr h ([],[]) xs where 
    h x (ys,zs) = (if p x then (x:ys) else ys, if (not . p) x then x:zs else zs)

test1 = partition1 (>4) [1..10]

-- Obviously, partition1 is still stupid, doing the test once is sufficient!:
partition2 p xs = foldr h ([],[]) xs where 
    h x (ys,zs) = if p x then (x:ys,zs) else (ys,x:zs)

test2 = partition2 (>4) [1..10]

-- which seems obviously good: it traverses the list exactly once, and it checks p for every x in xs exactly once.
-- This is really nice :)

-- Next part of the exercise:
part :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
part p xs us vs = (filter p xs ++ us,
                   filter (not . p) xs ++ vs)
-- Not sure what this is about. I'm told to use this weird part with the accumulating parameters to calculate
-- another version of partition. Now, this part seems to be not too clever,
-- since it filters xs twice and also checks twice.
-- Maybe this is just a test if I can apply the accumulating thingy. Let's try:
-- us und vs are appended to the left/right part of the partitions, and without a check. So they must be empty,
-- otherwise it must be garbage. So
partition3 p xs = part p xs [] []

test3 = partition3 (>4) [1..10]

-- This works, obviously, but it seems I just went back to the first definition, basically. Hm.
-- I could of course optimize this part thing in the same way as in the first part of the exercise, i.e.:
partition4 p xs = part2 p xs [] []

part2 :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
part2 p xs us vs = (as ++ us, bs ++ vs)
    where (as,bs) = foldr op ([],[]) xs
          op x (ys,zs) = if p x then (x:ys,zs) else (ys,x:zs)

test4 = partition4 (>4) [1..10]

-- But I'm not sure what the big insight is supposed to be here... ?_?
--
-- ...
--
-- Okay, looking at the sample solution: the idea is that part should be recursive. Does this improve anything? Let's see...
partition5 p xs = part3 p xs [] []

-- "Calculate" means: put in some values and see what we get. Reminder about the definition of part:
-- part :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
-- part p xs us vs = (filter p xs ++ us,
--                    filter (not . p) xs ++ vs)
-- 
-- part p [] us vs = (us,vs)
-- ^ this goes straight into the definition.
--
-- For the recursive part, let's try to actually use the accumulated parameters:
--
-- part p [x] us vs = if p x then part p xs (x:us) vs else part p xs us (x:vs)
-- ^ So we move x to the right side where it won't be touched again.
-- 

part3 :: (a -> Bool) -> [a] -> [a] -> [a] -> ([a],[a])
part3 p [] us vs = (us,vs)
part3 p (x:xs) us vs = if p x then part3 p xs (x:us) vs else part3 p xs us (x:vs)

test5 = partition5 (>4) [1..10]

-- okay, this is what the sample solution also arrives at.
-- Actually, it should declare part3 as a local definition, but whatever.

-- So what's the relevant observation here?
-- Probably, that the accumulating technique and the foldr technique both yield useful results,
-- but different ones.
-- I think, both approaches involve a single traversal of the list and both check each element exactly once,
-- so there shouldn't be a significant difference in performance.
-- But who knows o_O
