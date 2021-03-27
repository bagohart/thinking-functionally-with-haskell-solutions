import Control.Monad.ST
import Control.Monad
import Data.STRef


-- Write a fib using a single STRef

-- Recall the version from the text:
fibST0 :: Int -> ST s Integer
fibST0 n = do a <- newSTRef 0
              b <- newSTRef 1
              repeatFor n
                  (do x <- readSTRef a
                      y <- readSTRef b
                      writeSTRef a y
                      writeSTRef b $! (x+y))
              readSTRef a

repeatFor :: Monad m => Int -> m a -> m ()
repeatFor n = foldr (>>) done . replicate n

done :: Monad m => m ()
done = pure ()

newFib0 n = runST $ (fibST0 n)

theFibs0 = map newFib0 [1..20]

-------
-- ... a and b are actually always written together, so all I need to do is probably to put them into a tuple...
fibST :: Int -> ST s Integer
fibST n = do t <- newSTRef (0,1)
             repeatFor n
                (do (x,y) <- readSTRef t
                    writeSTRef t $! (y,x+y))
             (x,y) <- readSTRef t
             return x

newFib1 n = runST $ (fibST n)

theFibs1 = map newFib1 [1..20]

-- ... looks good.
-- Can i code golf this a bit more?
fibST' :: Int -> ST s Integer
fibST' n = do t <- newSTRef (0,1)
              replicateM_ n (readSTRef t >>= \(x,y) -> writeSTRef t $! (y,x+y))
              readSTRef t >>= return . fst

newFib1' n = runST $ (fibST' n)

theFibs1' = map newFib1' [1..20]

-- it works. hm. oh well.

-- Also the sample solution tells me that what I wrote doesn't run in constant space, since the $! doesn't force the tuple
-- any further than head normal form, so I need to `seq` both y and x+y. uh oh.
