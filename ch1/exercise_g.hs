-- this is not pretty yet, but it seems to work correctly
import Data.Char

song n = if n == 0 then ""
         else song (n-1) ++ "\n" ++ verse n

verse n = line1 n ++ line2 n ++ line3 n ++ line4 n 

line2 n = "Went to mow a meadow\n"

line4 = line2

line1 n | n == 1 = "One man" ++ suffix
        | n > 1  = convert n ++ " men" ++ suffix
    where suffix = " went to mow\n"

line3 n = titlecase ((enumerate n) ++ " and his dog\n")

titlecase :: String -> String
titlecase [] = []
titlecase (x:xs) = toUpper x : xs

enumerate :: Int -> String
enumerate 1 =  "one man"
enumerate n = map toLower (convert n) ++ " men, " ++ enumerate (n-1)

convert :: Int -> String
convert n
    | n == 2 = "Two"
    | n == 3 = "Three"
    | n == 4 = "Four"
    | n == 5 = "Five"
    | n == 6 = "Six"
    | n == 7 = "Seven"
    | n == 8 = "Eight"
    | n == 9 = "Nine"
    
