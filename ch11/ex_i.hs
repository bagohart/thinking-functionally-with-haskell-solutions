import Data.Char

-- what is a fully parenthesized expression?_?
-- what about (((((((3)))))))?
-- ...
-- the last parser seems to allow this, since you can derive
-- expr -> term -> factor -> (expr) -> (term) -> (factor) -> (nat)
-- or even more ((()))...
-- the first grammar doesn't allow it.
-- I don't get why I would have to design anything here. We already built the parser?___?
--
-- ... Taking a glance at the sample solution, it seems that this wants me to adapt the first grammar...

newtype Parser a = Parser (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where 
    fmap f (Parser p) = Parser $ \s -> map op (p s)
        where op (a,s) = (f a, s)

instance Applicative Parser where 
    pure x = Parser (\s -> [(x,s)])
    (Parser p) <*> (Parser q) = Parser $ \s -> [(f y,s'') | (f,s') <- p s, (y,s'') <- q s']

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

-- Grammars
data Expr = Con Int | Bin Op Expr Expr deriving (Show,Eq)
data Op = Plus | Minus deriving (Eq,Show)

expr :: Parser Expr
expr = token (constant <|> paren binary)
constant = do n <- nat
              return $ Con n
binary = do e1 <- expr
            p <- op
            e2 <- expr
            return $ Bin p e1 e2
op = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

-- exercise
paren :: Parser Expr -> Parser Expr
paren p = do symbol "("
             x <- p
             symbol ")"
             return x

-- Before grammar:
-- Expr -> Nat | ( Expr op Expr )
-- New:
-- Expr -> Nat | (Nat) | ( Expr op Expr )
-- this still only allows one level of nesting, but whatever

exprMP :: Parser Expr
exprMP = token (constant' <|> paren exprMP <|> paren binary')
constant' = do n <- nat
               return $ Con n
binary' = do e1 <- exprMP
             p <- op'
             e2 <- exprMP
             return $ Bin p e1 e2
op' = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

-- this does seem to work. I don't like it though ._.
