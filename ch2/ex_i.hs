import Data.Char

palindrome :: IO ()
palindrome = do {   putStrLn "Enter a string:";
                    input <- getLine;
                    if isPalindrome input then
                        putStrLn "Yes"
                    else
                        putStrLn "No"
                }

isPalindrome :: String -> Bool
isPalindrome text = normalize text == (normalize . reverse) text
    where normalize = map toLower . filter isAlpha

-- this version should be a bit more efficient, since the normalization happens only once.
isPalindrome2 :: String -> Bool
isPalindrome2 text = normalized == reverse normalized
    where normalized = (map toLower . filter isAlpha) text
