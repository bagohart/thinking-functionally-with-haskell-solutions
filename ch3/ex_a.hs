{-|

-2 + 3         # 1
3 + -2         # missing parentheses around -2
3 + (-2)       # 1
subtract 2 3   # 1
2 + subtract 3 # subtract needs 2 arguments

-}

subtract' :: Int -> Int -> Int
subtract' = flip (-)
