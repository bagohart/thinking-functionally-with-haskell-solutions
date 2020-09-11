{-
    concat . map concat = concat . concat
    [[[1,2], [3,4], [5,6]], [[7,8], [9,10], [11,12]] 
    (left side)
    ~> [ [1,2,3,4,5,6], [7,8,9,10,11,12]]
    ~> [1,2,3,4,5,6,7,8,9,10,11,12]

    OR (right side)
    [[1,2], [3,4], [5,6], [7,8], [9,10], [11,12] 
    ~> [1,2,3,4,5,6,7,8,9,10,11,12]

    filter p . map f = map f . filter (p . f)

    filter p . map f                # alternative definition for filter
    = concat . map (test p) . map f	    # functor join together
    = concat . map (test p . f)         # test p . f = map f . test (p . f) <- kinda obvious, but which law proves it?
    = concat . map (map f . test (p . f))   # functor split
    = concat . map (map f) . map (test (p . f)) # concat can happen after transforming
    = map f . concat . map (test (p . f)) # alternative definition for filter
    = map f . filter (p . f)
  
-}
