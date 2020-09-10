first :: (a -> Bool) -> [a] -> a
first p xs |    null xs = error "Empty list OH NOEZ"
           |    p x = x
           |    otherwise = first p (tail xs)
           where x = head xs
