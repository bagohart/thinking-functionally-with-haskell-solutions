sequence_0 :: Monad m => [m a] -> m ()
sequence_0 = foldr (>>) done

sequence0 :: Monad m => [m a] -> m [a]
sequence0 [] = return []
sequence0 (ma:mas) = do x <- ma
                        xs <- sequence0 mas
                        return $ x : xs

-- or, using foldr:
sequence1 :: Monad m => [m a] -> m [a]
sequence1 = foldr op (return [])
    where op ma mla = do x <- ma
                         xs <- mla
                         return $ x : xs

mapM_0 :: Monad m => (a -> m b) -> [a] -> m ()
mapM_0 f xs = sequence_ $ fmap f xs

mapM0 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM0 f xs = sequence0 $ fmap f xs

foldM0 :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM0 _ e [] = return e
foldM0 f e (x:xs) = do y <- foldM0 f e xs
                       f y x

-- the sample solution proposes a different implementation which could have different results
-- because it does a left fold o_O
foldM1 :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM1 _ e [] = return e
foldM1 f e (x:xs) = do y <- f e x
                       foldM1 f y xs

-- Can I show a difference between foldM0 und foldM1?
lf = foldM0 (\x y -> [x,x+y,x-y]) 0 [1..2]
rf = foldM1 (\x y -> [x,x+y,x-y]) 0 [1..2]
-- Yes, these are obviously different.

for_0 :: Monad m => [a] -> (a -> m b) -> m ()
for_0 = flip mapM_0
