import Data.Foldable

-- Construct f,e,xs such that
-- foldl f e xs != foldl' f e xs
-- Idea: this holds obviously always except when f is not strict. Reminder.:
-- foldl f e xs = ((((0 • x1) • x2) • x3) • ... • xn)
-- Now construct f such that _ • xn = 

e :: Bool
e = undefined

xs :: [Bool]
xs = [True]

-- this is a right-shortcircuiting or.
f :: Bool -> Bool -> Bool
f _ True = True
f x False = x

r1 = foldl f e xs
r2 = foldl' f e xs

-- even simpler, use a constant function:
e2 = undefined
xs2 = [True]
f2 _ _ = True

r1' = foldl f2 e2 xs2
r2' = foldl' f2 e2 xs2

-- the sample solution proposes to move the undefined into the definition of f. okay...
