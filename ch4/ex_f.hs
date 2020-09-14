data List a = Nil | Snoc (List a) a deriving (Show)

head' :: List a -> a
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs
head' Nil = error "list is empty NOOOOOO"

last :: List a -> a
last (Snoc _ x) = x
last Nil = error "list is empty NOOOOOO"

toList :: [a] -> List a
toList = go . reverse
    where
        go [] = Nil
        go (x:xs) = Snoc (go xs) x

fromList :: List a -> [a]
fromList = reverse . go
    where
        go Nil = []
        go (Snoc xs x) = x : (go xs)

-- reverse :: List a -> List a
-- reverse Nil = Nil
-- reverse (Snoc xs x) = 
