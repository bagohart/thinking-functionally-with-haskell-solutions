-- both lists must be in ascending order
-- idea: with empty lists we are obviously done
-- with non-empty lists and unequal elements, throw away the smaller element and repeat
-- this way any number cannot "walk past" an equal number unrecognized
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint (x:xs) (y:ys)
    | x == y = False
    | x < y = disjoint xs (y:ys)
    | x > y = disjoint (x:xs) ys
