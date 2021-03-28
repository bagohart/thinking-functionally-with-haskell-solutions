{-# LANGUAGE InstanceSigs #-}

import Data.Bifunctor

newtype Parser a = Parser (String -> Maybe (a,String))

runParser (Parser p) = p

-- this looks like a deterministic parser.
instance Functor Parser where 
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (Parser p) = Parser $ \s -> fmap (first g) (p s)

instance Applicative Parser where 
    pure :: a -> Parser a
    pure x = Parser $ \s -> Just (x,s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p) <*> (Parser q) = Parser $ \s -> (p s) >>= next -- this looks a bit weird, but whatever
        where next (f,s') = fmap (first f) (q s')

instance Monad Parser where 
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser p) f = Parser $ \s -> (p s) >>= next
        where next (a,s') = (runParser (f a)) s'
              -- the sample solution here is complete broken and doesn't even use the >>= instance for Maybe wtf????
