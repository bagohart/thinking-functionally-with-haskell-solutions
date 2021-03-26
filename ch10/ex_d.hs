-- instance Monad m => Functor m where 
-- fmap :: (a -> b) -> m a -> m b
-- fmap f = \ma -> do x <- ma
--                    f x

-- fmap for Monads, express using return  and >>=
liftM0 :: Monad m => (a -> b) -> m a -> m b
liftM0 f = (>>= (return . f))
-- or, less pointfree and less section-y:
-- liftM0 f ma = ma >>= (return . f)

testLiftM = liftM0 (+2) (Just 2)

join0 :: Monad m => m (m a) -> m a
join0 = (>>= id)

testJoin = join0 (Just (Just 2))

-- liftM -> map
-- join -> concat

(>>=!) :: Monad m => m a -> (a -> m b) -> m b
(>>=!) ma f = join0 $ liftM0 f ma

testBind = (Just 2) >>=! (Just . (+2))
