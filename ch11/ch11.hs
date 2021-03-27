import Data.Char

-- type Parser a = String -> a
-- this is like read. It consumes the whole input.

-- type Parser a = String -> (a,String)
-- this is more flexible, but it's not allowed to fail.

-- type Parser a = String -> [(a,String)]
-- this can fail and/or parse different things.
-- it's as ReadS in the prelude o_O
--
-- reads instances should be deterministic parsers, but I don't think this is enforced...
--
-- Aaand we can't make this into a Monad, because it is only a type synonym.

-- data Parser a = String -> [(a,String)]
-- ^ This is also bad because it's not isomorphic and we get Parser bottom as extra thingy. lol.

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

-- parser the first character, whatever it is, if there is one.
getc :: Parser Char
getc = Parser f
        where f [] = []
              f (c:cs) = [(c,cs)]

-- parses the first character if it satisfies a condition
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- getc
           if p c then return c
                  else failP

failP = Parser (\s -> [])
-- without P this definition yields a clash with fail from Control.Monad.Fail -_-

-- type signature in the book seems to be wrong.
guard :: Bool -> Parser ()
guard True = return ()
guard False = failP

-- then we can write
sat' p = do c <- getc
            guard $ p c
            return c

-- the implementations of the following things seem more complicated than necessary, so I simplified them...
char :: Char -> Parser ()
char x = sat (==x) >> return ()

string :: String -> Parser ()
string [] = return ()
string (x:xs) = char x >> string xs

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = convert <$> sat isDigit
        where convert d = fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
(Parser p) <|> (Parser q) = Parser $ \s -> let ps = p s in
                                           if null ps then q s else ps

lowers :: Parser String
lowers = do c <- lower
            cs <- lowers
            return (c:cs)
        <|> return ""

wrong :: Parser Int
wrong = digit <|> addition

addition :: Parser Int
addition = do m <- digit
              char '+'
              n <- digit
              return (m + n)

-- consider
-- apply wrong "1+2"

better :: Parser Int
better = addition <|> digit

-- Problem:
-- apply better "1"
-- parses the 1 twice, which is wasteful.
-- We could factor it out:
best :: Parser Int
best = digit >>= rest
rest m = do char '+'
            n <- digit
            return (m+n)
        <|> return m

many :: Parser a -> Parser [a]
many p = do x <- p
            xs <- many p
            return (x:xs)
        <|> none

none :: Parser [a]
none = return []
-- none is different from fail because
-- failP = Parser (\s -> [])
-- and none = Parser (\s -> [([],s)])
-- which is different in that it doesn't actually fail.
-- It just doesn't parse anything and has a result of [] which ... why not.

lowers2 = many lower

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol :: String -> Parser ()
symbol xs = space >> string xs

-- more general than symbol. actually symbol seems a bit weird defined for leading space...
token :: Parser a -> Parser a
token p = space >> p

-- token p <|> token q = token (p <|> q)
-- ^ the right hand side is faster, otherwise they're equivalent

some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            return (x:xs)

-- now dependent on some
many2 :: Parser a -> Parser [a]
many2 p = optional (some p)

optional :: Parser [a] -> Parser [a]
optional p = p <|> none

-- "   1234"
natural :: Parser Int
natural = token nat

-- "1234"
nat :: Parser Int
nat = do ds <- some digit
         return (foldl1 shiftl ds)
    where shiftl m n = 10*m+n

-- "   -123"
-- "-123"
-- "    123"
-- "123"
int :: Parser Int
int = do symbol "-"
         n <- natural
         return (-n)
      <|> natural

-- the last version is still inefficient because the space is parsed twice for e.g. "   123"
int2 :: Parser Int
int2 = do space
          f <- minus
          n <- nat
          return (f n)
      where minus = (char '-' >> return negate) <|> return id
-- this looks magic because there is a function returned from the parser.
-- Note that this is actually applicative thingy:
int2' = space *> (minus <*> nat)
          where minus = (char '-' >> return negate) <|> return id
-- I like it.

-- LIST PARSING
ints :: Parser [Int]
ints = bracket (manywith (symbol ",") int)

-- "[thingy]"
bracket :: Parser a -> Parser a
bracket p = do symbol "["
               x <- p
               symbol "]"
               return x

-- ""
-- "thingy,thingy"
-- the first argument is the separator
manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = optional (somewith q p)

-- "thingy,thingy"
somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do x <- p
                  xs <- many (q >> p)
                  return (x:xs)

