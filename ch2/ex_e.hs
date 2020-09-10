first :: (a -> Bool) -> [a] -> Maybe a
first p xs |    null xs = Nothing
           |    p x = Just x
           |    otherwise = first p (tail xs)
           where x = head xs

-- there exist two functions with the signature Maybe a -> Maybe a
-- the important insight here is that a can be anything,
-- so the function cannot behave differently depending on the a thingy
firstFunction :: Maybe a -> Maybe a
firstFunction = id

secondFunction :: Maybe a -> Maybe a
secondFunction x = Nothing
