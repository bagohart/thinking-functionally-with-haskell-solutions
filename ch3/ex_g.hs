data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero            = "Zero"
    show (Succ Zero)     = "Succ Zero"
    show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

instance Eq Nat where
    Zero   == Zero   = True
    Zero   == Succ n = False
    Succ n == Zero   = False
    Succ n == Succ m = (n == m)


instance Num Nat where
    m + Zero     = m
    m + Succ n   = Succ (m+n)

    m * Zero     = Zero
    m * (Succ n) = m * n + m

    abs n           = n
    signum Zero     = Zero
    signum (Succ n) = Succ Zero

    m - Zero        = m
    Zero - Succ n   = Zero
    Succ m - Succ n = m - n

    fromInteger x
        | x <= 0 = Zero
        | otherwise = Succ (fromInteger (x-1))

-- new stuff following

instance Ord Nat where
    Zero <= n = True
    Succ n <= Zero = False
    Succ n <= Succ m = n <= m

-- the sample solution just defines <, which seems like (a bit) more work

-- idea: find next smaller number by iterative multiplication, then subtract to get remainder
divMod :: Nat -> Nat -> (Nat,Nat)
divMod x y = (d, x - (d*y))
    where d = divFloor x y

divFloor :: Nat -> Nat -> Nat
divFloor x y = until (\n -> (n+1)* y > x) Succ Zero

-- this seems to work. the sample solution has a recursive solution that looks like a gcd thingy
