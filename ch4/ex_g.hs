length :: [a] -> Int
length xs = loop (0,xs)
    where loop (n,[]) = n
          loop (n,x:xs) = loop (n+1,xs)

{-
    length [1,2,3] = loop (0, [1,2,3])
    = loop (0+1, [2,3])
    = loop ((0+1)+1, [3])
    = loop (((0+1)+1)+1, [])
    = ((0+1)+1)+1
    = (1+1)+1
    = 2+1
    = 3
    looks like the sum is built up in memory with this definition
    it should be possible to prevent that by some eager evaluation thingy, so the sum is always
    compressed down to one certain number, then it wouldn't take linear (n) amount of space.
    I don't know which original definition of length the task refers to, so...
-}
