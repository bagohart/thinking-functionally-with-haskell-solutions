-- import qualified Prelude
import Prelude hiding ((<>))

type Layout = String

-- possible Layouts
type Doc = [Layout] -- "shallow embedding"

-- final function
-- line width, possible layouts, chosen layout
pretty :: Int -> Doc -> Layout
pretty = undefined

-- diagnostic tool for the user
layouts :: Doc -> [Layout]
layouts = id

-- (<++>) is like <>, but lifted on [Layout]:
(<++>) :: [Layout] -> [Layout] -> [Layout]
(<++>) xss yss = [xs ++ ys | xs <- xss, ys <- yss]

-- layouts (x <> y) = layouts x <++> layouts y
-- layouts nil = [""]
-- layouts (text s) = [s]
-- layouts line = ["\n"]
-- layouts (nest i x) = map (nestl i) (layouts x) -- nesting can be done on docs, or on final Strings.
-- layouts (group x) = layouts (flatten x) ++ layouts x -- group adds a single layout, and it is without line breaks. 

nestl :: Int -> Layout -> Layout
nestl i = concat . map (indent i) 
-- nestl i = concat (map indent i) -- <- book. This is broken here, see errata list. mapping on single Int o_O

-- flatten (x <> y) = flatten x <> flatten y
-- flatten nil = nil -- empty docuent has no newlines.
-- flatten (text s) = text s -- text removes newlines
-- flatten line = text " " -- flatten replaces newline with space
-- flatten (nest i x) = flatten x -- adding even more indentation doesn't prevent flatten from finding the things it already found before
-- flatten (group x) = flatten x -- adding another layout without linebreaks doesn't change anything. this seems a bit mysterious.

-- put spaces after a newline unless it's not a newline
indent :: Int -> Char -> Layout
indent i c = if c == '\n' then c : replicate i ' ' else [c]

-- must be associative
(<>) :: Doc -> Doc -> Doc
(<>) = (<++>)

-- identity for (<>)
nil :: Doc
nil = [""]

-- string without newlines
text :: String -> Doc
text s = [s]

-- aka text is a homomorphism from string concatenation to document concatenation
-- text (s ++ t) = text s <> text t
-- text "" = nil

-- explicit newline, forbidden in input string, so we don't have to inspect the contents of all text
-- (not sure why we can avoid it now - I thought I need the length of the text always. maybe it becomes clearer later.)
line :: Doc
line = ["\n"]

-- insert i spaces _AFTER_ every newline (reason later...)
nest :: Int -> Doc -> Doc
nest i = map (nestl i)

-- nest i (x <> y) = nest i x <> nest i y
-- nest i nil = nil
-- nest i (text s) = text s -- since s has no newlines
-- nest i line = line <> text (replicate i ' ') -- since line is only a single newline
-- nest i (nest j x) = nest (i+j) x -- nesting twice is like nesting once, but more. looks a lot like fmap.
-- nest 0 x = x -- nesting with nothing is a noop
-- nest i (group x) = group (nest i x) -- grouping adds a layout with no line breaks. not sure what this means here.

-- add an extra layout that consists of a single line of text with no line breaks
group :: Doc -> Doc
group x = flatten x ++ x

-- not public, but needed internally and for laws
-- convert into Document with single layout, where each (newline+indents) is replaced with a single space
flatten :: Doc -> Doc
flatten x = [flattenl (head x)] -- this looks suspicious.

-- drop all newlines with subsequent spaces
flattenl :: Layout -> Layout
flattenl [] = []
flattenl (c:cs)
    | c == '\n' = ' ' : flattenl (dropWhile (== ' ') cs)
    | otherwise = c : flattenl cs

-- this violates the law
-- flatten (x <> y) = flatten x <> flatten y
-- with
-- x = line
-- y = text "  hello"
-- Then
-- flatten (x <> y) = ["hello"]
-- flatten x <> flatten y = ["  hello"]
-- But we'll just ignore that u_u

shape :: Layout -> [Int]
shape = map length . lines

-- map shape (layouts doc) <- yields a list in lexicographically decreasing order.

data CExpr = Expr String | If String CExpr CExpr deriving (Show,Eq)

cexpr :: CExpr -> Doc
cexpr (Expr p) = text p
cexpr (If p x y) = group (group (text "if " <> text p <>
                                line <> text "then " <>
                                nest 5 (cexpr x)) <>
                            line <> text "else " <>
                            nest 5 (cexpr y))

expr1 :: CExpr
expr1 = If "wealthy" (If "happy" (Expr "lucky you") (Expr "tough")) (If "in love" (Expr "content") (Expr "miserable"))
