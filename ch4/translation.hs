{-
    map f xs = [f x | x <- xs]
    [f x | x <- xs] ~> [f x | x <- xs, True] ~> [f x | x <- xs, True]
    ~>  let ok x = [f x | True]
            ok _ = []
        in concat (map ok xs)
-}
