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

-- Grammars
data Expr = Con Int | Bin Op Expr Expr deriving (Eq)
-- data Op = Plus | Minus deriving (Eq,Show)
-- ^ replaced below with more things

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

-- I'll skip the broken left recursion lol.
-- Left recursion can be avoided by transforming it into right recursion, and then the grammar looks weird.
-- Here, we use another approach which looks more like regex o_O i.e. expr = term {op term}*
expr2 = token (term >>= rest2)
term = token (constant <|> paren expr2)
-- e1 for rest2 is an accumulating parameter, taking all that has been parsed so far.
rest2 e1 = do p <- op
              e2 <- term
              rest2 (Bin p e1 e2)
           <|> return e1

-- parser for +-*/ expressions
data Op = Plus | Minus | Mul | Div deriving (Eq,Show)

expr3 = token (term3 >>= rest3)
rest3 e1 = do p <- addop
              e2 <- term3
              rest3 (Bin p e1 e2)
           <|> return e1
term3 = token (factor >>= more3)
more3 e1 = do p <- mulop
              e2 <- factor
              more3 (Bin p e1 e2)
            <|> return e1
factor = token (constant <|> paren expr3)

-- exercise
addop = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
mulop = (symbol "*" >> return Mul) <|> (symbol "/" >> return Div)

-- showing the things
-- this should satisfy
-- (parse expr3 . show) e = e
-- instance Show Expr where 
--     show (Con n) = show n
--     show (Bin op e1 e2) =
--         "(" ++ show e1 ++
--             " " ++ showop op ++
--                 " " ++ show e2 ++ ")"
-- showop Plus  = "+"
-- showop Minus = "-"
-- ^ this first try works, but it takes, like, worst case quadratic, which is, like, bad.
-- try again with magic built-in functions that use accumulating parameters.
instance Show Expr where 
    show e = shows e ""
        where 
        shows (Con n) = showString (Prelude.show n) -- why can't this disambiguate the right function by type signature?
        shows (Bin op e1 e2) = showParen True (shows e1 . showSpace . showsop op . showSpace . shows e2)
showsop Plus = showChar '+'
showsop Minus = showChar '-'
showsop Mul = showChar '*'
showsop Div = showChar '/'
showSpace = showChar ' '
-- now this is like, linear. it uses showChar, showString, showParen.
-- not sure what this does. it looks a bit like CPS maybe ?_?

-- those were a lot of parentheses. let's try to reduce them.
show2 e = shows False e ""
    where 
    shows b (Con n) = showString (show n)
    shows b (Bin op e1 e2) = showParen b (shows False e1 . showSpace . showsop op . showSpace . shows True e2)

-- now also for things with * and / where everything is harder...
prec :: Op -> Int
prec Mul = 2
prec Div = 2
prec Plus = 1
prec Minus = 1

show3 e = myShowsPrec 0 e ""

-- assumption: parent of e is a compound expression with an operator of precedence p
myShowsPrec :: Int -> Expr -> ShowS
myShowsPrec _ (Con n) = showString (show n)
myShowsPrec p (Bin op e1 e2) = showParen (p>q)
                                (myShowsPrec q e1 . showSpace . showsop op . showSpace . myShowsPrec (q+1) e2)
    where q = prec op
-- this is a lot of magic and I'm not sure this is even always correct. I can't find a counterexample though.

---- now ex (l) ...
isMulOp Mul = True
isMulOp Div = True
isMulOp _ = False

showsF :: (Op -> Bool) -> Expr -> ShowS
showsF _ (Con n) = showString (show n)
showsF f (Bin op e1 e2) = showParen (f op) (showsF f1 e1 . showSpace . showsop op . showSpace . showsF f2 e2)
    where f1 x = (isMulOp op && (not . isMulOp) x) -- closes over the operator in the current level, and defers decision on next lower level
          f2 x = (isMulOp op || (not . isMulOp) x)

showL e = showsF (const False) e -- we never need parentheses on the top level!

-- this... seems to work.
-- It also looks a bit magic although it's actually merely a quite direct encoding of the given rules. uh oh.
-- At my first attempt I tried to enforce the parentheses top down which should also be possible.
-- But a bit uglier. but still, let's try this:

isMulTopLevel (Con _) = False
isMulTopLevel (Bin Mul _ _) = True
isMulTopLevel (Bin Div _ _) = True
isMulTopLevel (Bin _ _ _) = False

isCon (Con _) = True
isCon _ = False

showMy :: Expr -> ShowS
showMy (Con n) = showString (show n)
showMy (Bin op e1 e2) = showParen left (showMy e1) . showSpace . showsop op . showSpace . showParen right (showMy e2)
    where left = (not . isCon) e1 && (isMulOp op && (not . isMulTopLevel) e1)
          right = (not . isCon) e2 && (isMulOp op || (not . isMulTopLevel) e2)

-- this works, too, and it is even more direct and not hard to understand.
-- The disadvantage is that I need to "peek down" into the next structure, which then happens multiple times,
-- so this is probably a bit slower.
-- The sample solution approach just defers the decision to (or not to) introduce parentheses one more step.
