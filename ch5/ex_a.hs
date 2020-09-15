type Matrix a = [Row a]
type Row a = [a]

-- test data
m :: Matrix Int
m = [[1,2],[3,4]]

n :: Matrix Int
n = [[5,6],[7,8]]

addOneToMatrix :: Matrix Int -> Matrix Int
addOneToMatrix = map (map (+1))

sumMatrixElements :: Matrix Int -> Int
sumMatrixElements = sum . (map sum)
-- or: sum . concat

-- the sample solution on this is much nicer.
-- probably I accidentally discovered some law about zipWith and map or something
addTwoMatrices :: Matrix Int -> Matrix Int -> Matrix Int
addTwoMatrices m n = map (uncurry (zipWith (+))) (zip m n)
-- here is some rather voodoo-ish looking way of transforming this into pointfree style,
-- which is non-obvious here because zip has two parameters
-- but this doesn't make anything more readable.
-- this seems to have to do something with the (.).(.) construction aka owl operator o_O
-- addTwoMatrices = (.) ((map (uncurry (zipWith (+)))) . zip)

-- the sample solution uses a list comprehension to get rid of the explicit lambda here
-- but I think it's not so bad
times :: Matrix Int -> Matrix Int -> Matrix Int
times x y = map (\r -> map (scalarProduct r) cs) x
    where
        cs = cols y

{-
this is how matrix multiplication works:
       5 6
       7 8

1 2   19 22
3 4   43 50
-}

-- use this to compute a single entry
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct x y = sum $ (zipWith (*)) x y

-- transposes a matrix, from the example
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)
