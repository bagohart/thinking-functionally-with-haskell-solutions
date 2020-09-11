{-
    []:xs = xs             # if xs :: [[a]], then [] : xs != xs is usually not true, e.g. for xs = [[1]]
                           # because [] : [[1]] = [[], [1]] != [[1]]
    []:xs = [[],xs]        # no. again, consider xs = [[1]]
                           # []:[[1]] = [[],[1]] != [[],[[1]]]
    xs:[] = xs             # no. Consider xs = 1
                           # 1:[] = [1] != 1
    xs:[] = [xs]           # this looks correct, since [xs] is just syntactic sugar for xs:[]
    xs:xs = [xs,xs]        # no. For xs = []
                           # [] : [] = [[]] != [[],[]]
    [[]] ++ xs = xs        # no. For xs = [[1]]
                           # [[]] ++ xs = [[], [1]] != [[1]]
    [[]] ++ xs = [[], xs]  # no. For xs = [[1]]
                           # [[]] ++ xs = [[], [1]] != [[], [[1]]]
    [[]] ++ [xs] = [[],xs] # Correct:
                           # [[]] ++ [xs] = [] : [xs] = [] : xs : []
                           # and
                           # [[],xs] = [] : [xs] = [] : xs : []
                           # evaluate to the same desugared form.
    [xs] ++ [] = [xs]      # Correct. By definition of ++ we obtain
                           # [xs] ++ [] = (xs : []) ++ [] = [] ++ (xs : []) = xs : []
                           # and the right side can be desugared to
                           # [xs] = xs : []

    null = (==[]) doesn't compile because for == on lists we need the requirement Eq a.
    But we can define null also on lists without this requirement (i.e. all lists), which is obviously more useful.
-}
