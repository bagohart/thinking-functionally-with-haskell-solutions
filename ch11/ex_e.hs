import Control.Monad
import Control.Applicative

newtype Parser a = Parser (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s

-- Parser / apply are mutual inverses and "witness the isomorphism" yay.

parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- book is too old and doesn't tell me how to implement functor and Applicative so I'll just hack this together
-- although I could also rely on >>= of course.
instance Functor Parser where 
    fmap f (Parser p) = Parser $ \s -> map op (p s)
        where op (a,s) = (f a, s)

instance Applicative Parser where 
    pure x = Parser (\s -> [(x,s)])
    (Parser p) <*> (Parser q) = Parser $ \s -> [(f y,s'') | (f,s') <- p s, (y,s'') <- q s']
-- I hope this is correct o_O

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    p >>= q = Parser (\s -> [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s'])

failP = Parser (\s -> [])
-- new stuff now: MonadPlus
-- also Haskell has changed since it seems and I need this...

instance Alternative Parser where 
    empty = failP
    (Parser p) <|> (Parser q) = Parser $ \s -> let ps = p s in
                                               if null ps then q s else ps
       -- this... isn't even new I just copy&pasted this ._.

instance MonadPlus Parser where 
    mzero = empty
    mplus = (<|>)
    -- this is so useless
