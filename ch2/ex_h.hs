type CIN = String

addSum :: CIN -> CIN
addSum eightDigits = eightDigits ++ show checkSum
    where
        checkSum = (sum . (map getDigit)) eightDigits

getDigit :: Char -> Int
getDigit c = read [c]

valid :: CIN -> Bool
valid cin = let mainDigits = take 8 cin; checkDigits = drop 8 cin in
    sum (map getDigit mainDigits) == (read checkDigits :: Int) &&
    length checkDigits == 2

-- ^ this works, but the solution reminded me that I could just reuse addSum in valid o_O
