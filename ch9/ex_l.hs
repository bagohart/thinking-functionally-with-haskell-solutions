type Matrix a = [[a]]

data Torus a = Cell a (Torus a) (Torus a)
                      (Torus a) (Torus a)

elem  (Cell a u d l r) = a
up    (Cell a u d l r) = u
down  (Cell a u d l r) = d
left  (Cell a u d l r) = l
right (Cell a u d l r) = r

instance Show a => Show (Torus a) where 
    show (Cell x _ _ _ _) = show x

matrix = [[1,2,3],[4,5,6],[7,8,9]]

-- ... I should probably try this exercise AFTER reading the section about doubly linked lists...
-- ok, this seems like the double linked list, except now it has even more dimensions. uh oh.
-- Actually, this looks very close to the DLL example, except now the array is nested, which may make
-- everything harder and much more mysterious.
-- I probably need some sort of zipWith5 because I have more things to combine: 
-- The original matrix and 4 directions.
-- It probably needs to be lazy, too?_?

zipWith5 f (x:xs) ~(a:as) ~(b:bs) ~(c:cs) ~(d:ds) = f x a b c d : zipWith5 f xs as bs cs ds
zipWith5 _ _ _ _ _ _ = []

-- the starting element of the torus should probably be the top left element.
-- How can I do the rotating around?
rotr xs = [last xs] ++ init xs -- put the last element to the front
rotl xs = tail xs ++ [head xs] -- put the first element at the end

mkTorus :: Matrix a -> Torus a
mkTorus mss = head (head xss) -- this should give me the top left element of the matrix.
    where xss = zipWith5 (zipWith5 Cell)
                  mss
                  (rotr xss) (rotl xss)
                  (map rotr xss) (map rotl xss)
-- xss is all the Toruses
-- rotr xss / rotl xss rotates the rows of the matrix down/up
-- map rotr xss / map rotl xss rotates the colums right/left
-- all of this looks magic, but it seems to work.
-- the double zipWith5 is pretty mindblowing.
-- how do you even visualize this thing?
-- you feed the outer zipWith5 a list of lists, so the inner zipWith5 gets a bunch of lists to zip.
-- uh oh.
--
-- the chapter notes point to some papers which explore the cyclic thingy in more depth.
-- maybe this is helpful for getting even more mind-blown.

-- try this in ghci with
-- mkTorus matrix
-- left it
-- right it
-- down it
-- up it
-- etc.
