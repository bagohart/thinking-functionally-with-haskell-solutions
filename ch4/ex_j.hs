-- map f . take n = take n . map f
-- ^ this says that it doesn't matter whether the values in a list are transformed first and then dropped, or vice versa. Read this from right to left to get an optimization. (sort of, because f maybe isn't evaluated)
-- this seems correct for finite lists.
-- for infinite lists, it should be correct, too.
-- for undefined lists or lists with undefined elements... it should work, too, since always
-- only as much as is needed is evaluated, and take is concerned only with the structure of the list,
-- whereas f is concerned only with the element

-- map f . reverse = reverse . map f
-- this looks correct. again, both parts do something completely different.
-- on an infinite or undefined list, this blows up either way

-- map f . sort = sort . map f
-- now this is obviously wrong. note that sort depends on the contents of the list.
-- this is different than in the previous two examples.
-- for a simple counterexample, consider
-- map negate . sort $ [1..3] = [-1, -2, -3]
-- sort . map negate $ [1..3] = [-3, -2, -1]

-- map f . filter p = map fst . filter snd . map (fork (f,p))
-- where fork computes two different functions on a single value (yielding a tuple)
-- this seems obviously true, again this reads like an optimization: don't transform until you have filtered.
-- but again, there's lazy thingies and stuff...
-- as usual, this works because no side effects. we can compute two functions on a value, or not, and don't need to worry about it.

-- filter (p . g) = map (invertg) . filter p . map g
-- ^ approximate meaning: if the filter is on a value that we need only to filter it, then we could have not done that transformation in the first place
-- seems obviously true.

-- reverse . concat = concat . reverse . map reverse
-- ^ either concat a bunch of lists and then reverse the result, or reverse the small lists, then reverse their order and then concat everything.
-- seems true. with infinite or undefined lists, everything will blow up anyway, so...
-- [[1,2],[3,4]] ~> [4,3,2,1] or
-- [[1,2],[3,4]] ~> [[2,1],[4,3]] ~> [[4,3],[2,1]] ~> [4,3,2,1]
-- the left side looks simpler.

-- filter p . concat = concat . map (filter p)
-- ^ filtering or concating first doesn't matter.
-- this seems true.
