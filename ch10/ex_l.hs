import Control.Monad.State.Lazy
import Control.Monad.ST
import Control.Monad
import Data.STRef

gcd0 (x,y) | x == y = x
           | x < y = gcd0 (x,y-x)
           | x > y = gcd0 (x-y,y)

-- Task 1: Rewrite this using the State Monad...
-- That begs the question... what is the State?
-- I think (x,y) are both the state
gcd1 (x,y) = fst $ evalState (computeIt >> get) (x,y)

computeIt :: State (Int,Int) ()
-- computeIt = undefined
computeIt = do (x,y) <- get
               if x == y then return ()
                         else if x < y 
                              then put (x,y-x) >> computeIt
                              else put (x-y,y) >> computeIt
                
-- This... works. Also it seems pretty pointless.
-- I do recursion, but instead of just passing the values as arguments, I put them into the state, recurse, get them out of the state. c'mon lol.

-- Well, let's ST this thingy.
gcd2 (x,y) = fst $ runST $ do s <- newSTRef (x,y)
                              computeIt2 s
                              readSTRef s

computeIt2 :: STRef s0 (Integer,Integer) -> ST s0 ()
computeIt2 s = do (x,y) <- readSTRef s
                  if x == y then return ()
                            else if x < y 
                                 then writeSTRef s (x,y-x) >> computeIt2 s
                                 else writeSTRef s (x-y,y) >> computeIt2 s

-- This is pretty neat actually.
-- It means I can pass around the STRef, although I can't just take it out of the ST.

-- The sample solution works a bit differently: it uses 2 STRefs, and doesn't pass the result via the state.
-- (I'll try to do it like that without looking...)
gcd3 (x,y) = runST $ do a <- newSTRef x
                        b <- newSTRef y
                        recurrr a b

recurrr :: STRef s0 Integer -> STRef s0 Integer -> ST s0 Integer
recurrr a b = do x <- readSTRef a
                 y <- readSTRef b
                 if x == y then return x
                           else if x < y then writeSTRef a x >> writeSTRef b (y-x) >> recurrr a b
                                         else writeSTRef a (x-y) >> writeSTRef b y >> recurrr a b

-- I guess that's a bit clearer, but the two STRef's seem just bloat. How about this:
gcd4 (x,y) = runST $ do r <- newSTRef (x,y)
                        loop r

loop :: STRef s0 (Integer,Integer) -> ST s0 Integer
loop r = do (x,y) <- readSTRef r
            if x == y then return x
                      else if x < y then writeSTRef r (x,y-x) >> loop r
                                    else writeSTRef r (x-y,y) >> loop r

-- I guess I prefer this version.
-- Also I could probably force evaluation of the things with seq but whatever.
-- It is always evaluated in the next step anyway.
