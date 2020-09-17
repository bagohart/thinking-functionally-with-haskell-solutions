data Nat = Zero | Succ Nat deriving (Show)
data NEList a = One a | Cons a (NEList a) deriving (Show)

foldN :: (b -> b) -> b -> Nat -> b
foldN _ e Zero = e
foldN f e (Succ n) = f (foldN f e n)

foldL :: (a -> b -> b) -> (a -> b) -> NEList a -> b
foldL _ g (One x) = g x
foldL f g (Cons x xs) = f x (foldL f g xs)

{-
    Q: what is wrong with foldr1?
        The book hints that it is not general enough:
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

In contrast to foldr, the types are the same.
This is weird.
Maybe something like this would be better:
foldr1' :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldr1' _ g [x] = g x
foldr1' f g (x:xs) = f x (foldr1' f g xs)
-}
