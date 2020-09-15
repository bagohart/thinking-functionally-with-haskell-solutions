type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
        all nodups (cols g) &&
        all nodups (boxs g)

-- this seems quite inefficient but the book says maybe it's not
nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/=x) xs && nodups xs

-- A list of rows is... a Matrix again
rows :: Matrix a -> Matrix a
rows = id

-- hm this transposes the Matrix actually
-- (exercise: what happens with empty matrix?)
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

{-
   This looks a bit weird, but it works out:
   cols [[1,2,3], [4,5,6], [7,8,9]]
   = zipWith (:) [1,2,3] (cols [[4,5,6], [7,8,9]])
   = zipWith (:) [1,2,3] (zipWith (:) [4,5,6] (cols [7,8,9]))
   = zipWith (:) [1,2,3] (zipWith (:) [4,5,6] [[7],[8],[9]])
   = zipWith (:) [1,2,3] [4:[7], 5:[8], 6:[9]]
   = [1:4:[7],2:5:[8],3:6:[9]]
   I guess one can read this as "combine elements in this row with... elements in other rows"
   which is sort of what transposing is
-}

-- this is quite obvious... but only by picture.
boxs :: Matrix a -> Matrix a
boxs =  map ungroup . ungroup .
        map cols .
        group . map group

-- splits a list into groups of three
-- I'm pretty sure there's a more general stl function for this that is better
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs:group (drop 3 xs)

-- we just reinvented concat. y-yay...
ungroup :: [[a]] -> [a]
ungroup = concat

completions :: Grid -> [Grid]
completions = expand . choices


expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp
-- map cp [Row [Digit]] computes a list of possible choices for each row
-- the outer cp computes combinations of possible choices for each row
-- o_O

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
choice d = if blank d then digits else [d]

-- cartesian product. use this on a row of [Digit] to get all possible rows
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp xss

-- check to see that the second definition does indeed work
-- cp [xs] = cp (xs:[])
--         = [x:ys | x <- xs, ys <- cp []]
--         = [x:ys | x <- xs, ys <- [[]]]
--         = [x:[] | x <- xs]
--         = [[x] | x <- xs]
