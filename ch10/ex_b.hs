add3PM :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
add3PM (Just a) (Just b) (Just c) = Just $ a + b + c
add3PM _ _ _ = Nothing

test1 = add3PM (Just 1) (Just 2) (Just 3)

add3MM :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
add3MM m1 m2 m3 = do
    a <- m1
    b <- m2
    c <- m3
    pure $ a + b + c

test2 = add3MM (Just 1) (Just 2) (Just 3)

-- Applicative ?
add3AS :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
add3AS m1 m2 m3 = (\x y z -> x + y + z) <$> m1 <*> m2 <*> m3

test3 = add3AS (Just 1) (Just 2) (Just 3)

-- More Applicative ...
add3ASLOL :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
add3ASLOL m1 m2 m3 = (+) <$> ((+) <$> m1 <*> m2) <*> m3

test4 = add3ASLOL (Just 1) (Just 2) (Just 3)
-- whatever.

