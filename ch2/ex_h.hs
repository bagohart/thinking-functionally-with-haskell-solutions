type CIN = String

addSum :: CIN -> CIN
addSum eightDigits = eightDigits ++ show checkSum
    where
        checkSum = (sum . (map getDigit)) eightDigits

getDigit :: Char -> Int
getDigit c = read [c]

-- todo: 08 vs 8 as checksum
valid :: CIN -> Bool
valid cin = let mainDigits = take 8 cin; checkDigits = drop 8 cin in
    sum (map getDigit mainDigits) == (read checkDigits :: Int) &&
    length checkDigits == 2
