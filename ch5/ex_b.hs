type Matrix a = [Row a]
type Row a = [a]

m :: Matrix Int
m = [[1,2,3],[4,5,6],[7,8,9]]

m2 :: Matrix Int
m2 = [[11,12,13],[14,15,16],[17,18,19]]

n :: Matrix Int
n = [[5,6],[7,8]]

-- [[],[]] <- looks like a 2-dimensional matrix, but... actually 2x0 ???
-- [] <- looks like a vector, except maybe not. actually it does not have rows, but the non-existent rows
-- could have many columns, so... lol.
-- the sample solution states that thus it must be 0xn and somehow this has to do anything with the next task.
-- hm. ok. so that's the missing mysterious connection.

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
-- transpose [xs] = [[x] | x <- xs]
transpose (xs:xss) = zipWith (:) xs (transpose xss)

{-
If I don't repeat the thing as above, this is what happens:
transpose [[1,2],[3,4]] = zipWith (:) [1,2] (transpose [3,4])
= zipWith [1,2] : (zipWith (:) [3,4] (transpose []))
= zipWith [1,2] : (zipWith (:) [3,4] [[]])
= zipWith [1,2] : ([[3]])
= [[1],[3]]
-}

{-
    transpose2 [[1,2],[3,4]] = map head [[1,2],[3,4]]:transpose2 (map tail [[1,2],[3,4]])
    = map head [[1,2],[3,4]]:transpose2 (map tail [[1,2],[3,4]])
    = [1,3] : transpose2 ([[2],[4]])
    = [1,3] : (map head [[2],[4]]:transpose2 [[],[]])
    = [1,3] : [2,4] : []
    = [[1,3],[2,4]]
    Note: head [] blows up, so map head [[],[]] blows up.
    so the case of the array consisting of empty arrays must be caught.
    in the following, assume that the Matrix is well-formed.
-}
transpose2 :: [[a]] -> [[a]]
transpose2 xss = if null (head xss) then [] else map head xss:transpose2 (map tail xss)
-- the sample solution adds that this could be done with a pattern ([]:xss) if the order is correct.

