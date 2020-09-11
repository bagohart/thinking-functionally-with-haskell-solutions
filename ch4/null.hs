null' :: [a] -> Bool
null' = (==[])

-- This doesn't compile, since == on lists assumes that you can compare its elements.
