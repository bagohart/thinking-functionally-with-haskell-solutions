{-# LANGUAGE RankNTypes #-}

import Control.Monad.State.Lazy
import Control.Monad.ST
import Data.STRef

-- this seems to not be in prelude anymore
-- probably its purpose was to let you write something like
-- main = blabla >> done
done :: Monad m => m ()
done = pure ()

putStrLn1 :: String -> IO ()
putStrLn1 xs = foldr (>>) done (map putChar xs)
                >> putChar '\n'

getLine1 :: IO String
getLine1 = getChar >>= f
    where f x = if x == '\n' then return []
                             else getLine1 >>= g
                                 where g xs = return (x:xs)
                                       -- This... looks a bit convoluted. lol.

getLine2 :: IO String
getLine2 = getChar >>= \x ->
    if x == '\n'
       then return []
       else getLine2 >>= \xs ->
           return (x:xs)

getLine3 :: IO String
getLine3 = do x <- getChar
              if x == '\n'
                 then return []
                 else do xs <- getLine3
                         return (x:xs)

-- or use explicit {} layout to make the inner do thingy clearer.
getLine4 :: IO String
getLine4 = do {x <- getChar;
              if x == 'n'
                 then return []
                 else do {xs <- getLine4;
                         return (x:xs)}}

-- here is an example why unsafePerformIO is dangerous:
-- int :: Int
-- int = x - y
--      where x = runIO readInt
--            y = runIO readInt
--            Q: is x or y evaluated first? o____O

-- undefined >> return  0 :: IO Int
-- ^ This... raises an error, although the second action doesn't need the first one.
-- So IO actions are performed in order, always, because they're, like, strict.

-- incorrectBecauseLastStatementMustBeAnAction = do x <- getChar

-- didn't find this exercise later, so here it is:
(<=<!) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<!) h g = \x -> g x >>= h

surprise = evalState (undefined >> return 0) 1

fibST :: Int -> ST s Integer
fibST n = do a <- newSTRef 0
             b <- newSTRef 1
             repeatFor n
                 (do x <- readSTRef a
                     y <- readSTRef b
                     writeSTRef a y
                     writeSTRef b $! (x+y))
             readSTRef a

-- this is like
-- a,b = 0,1
-- for i in (0..n)
--  a,b = b,a+b
-- return a
--
-- I'm not sure this is an improvement...

repeatFor :: Monad m => Int -> m a -> m ()
repeatFor n = foldr (>>) done . replicate n

newFib n = runST $ (fibST n)

theFibs = map newFib [1..20]

-- what's up with this? it doesn't compile, even with RankNTypes enabled...?_?
-- list2 :: [forall a. a -> a]
-- list2 = undefined
