-- v2 from ch 5.3

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

-- idea: remove a choice from a cell, if it is already a singleton entry in its row, column and box,
-- i.e. if this value is already determined to appear somewhere near, and would collide with this choice
-- this seems like the first obvious thing that anyone would do to remove choices
prune :: Matrix [digit] -> Matrix [Digit]
prune = undefined

-- filter (p . f) = map f . filter p . map f
-- ^ only if f . f = id ! but then it is sorta obvious...
-- filter (p . f) . map f = map f . filter p
-- ^ same idea... filtering or mapping first doesn't really matter, and the filtering does not transform the data...
-- Obviously, the second law follows from the first since if
-- a = b then
-- a . l = b . l
-- and here a = filter (p . f) and b = map f . filter p . map f
-- and with l = map f we get
-- a . l = filter (p . f) . map f = map f . filter p . map f . map f =
-- map f . filter p . map (f . f) = map f . filter p . map id = map f . filter p 
-- ... to be continued

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
    where fixed = [d | [d] <- row] -- all singleton entries in this row, i.e. all fixed choices

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x] -- a fixed choice, don't remove it
remove ds xs = filter (`notElem` ds) xs -- no fixed choice here, so let's remove the possibilities, which ARE fixed elsewhere
