data Move = Paper | Rock | Scissors deriving (Eq,Show)
type Round = (Move,Move)

score :: Round -> (Int,Int)
score (x,y) | x `beats` y = (1,0)
            | y `beats` x = (0,1)
            | otherwise   = (0,0)

Paper `beats` Rock = True
Rock `beats` Scissors = True
Scissors `beats` Paper = True
_ `beats` _ = False

-- returns infinite list
rounds :: (Strategy,Strategy) -> [Round]
rounds = undefined

match :: Int -> (Strategy,Strategy) -> (Int,Int)
match n = total . map score . take n . rounds
    where total rs = (sum (map fst rs), sum (map snd rs))

-- list of moves of the opponent, in reverse order
type Strategy1 = [Move] -> Move

copy :: Strategy1
copy ms = if null ms then Rock else head ms

smart1 :: Strategy1
smart1 ms = if null ms then rock
                       else pick (foldr count (0,0,0) ms)

count :: Move -> (Int,Int,Int) -> (Int,Int,Int)
count Paper (p,r,s) = (p+1,r,s)
count Rock (p,r,s) = (p,r+1,s)
count Scissors (p,r,s) = (p,r,s+1)

pick :: (Int,Int,Int) -> Move
pick (p,r,s)
  | m < p = Scissors
  | m < p+r = Paper
  | otherwise = Rock
    where m = rand (p+r+s)

rand :: Int -> Int
rand n = fst $ randomR (0,n-1) (mkStdGen n)

rounds1 :: (Strategy1,Strategy1) -> [Round]
rounds1 (p1,p2) = map head $ tail $ iterate (extend (p1,p2)) []

extend (p1,p2) rs = (p1 (map snd rs), p2 (map fst rs)) : rs

type Strategy2 = [Move] -> [Move]

copy2 :: Strategy2
copy2 ms = Rock : ms

smart2 :: Strategy2
smart2 ms = Rock : map pick (stats ms)
    where stats = tail . scanl (flip count) (0,0,0)

-- dafuq
rounds2 :: (Strategy2,Strategy2) -> [Round]
rounds2 (p1,p2) = zip xs ys
                    where xs = p1 ys
                          ys = p2 xs

cheat ms = map trump ms

trump Paper = Scissors
trump Rock = Paper
trump Scissors = Rock

devious :: Int -> Strategy2
devious n ms = take n (copy2 ms) ++ cheat (drop n ms)

dozy ms = repeat undefined

police p ms = ms' where ms' = p (synch ms ms')
synch (x:xs) (y:ys) = (y `seq` x) : synch xs ys

rounds2secure (p1,p2) = zip xs ys
                    where xs = police p1 ys
                          ys = police p2 xs

-- the book doesn't even tell me how to run this? dafuq?
