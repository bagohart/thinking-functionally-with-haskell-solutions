type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (day, month, year) = show day ++ daySuffix day ++ " " ++ showMonth month ++ ", " ++ show year

showMonth :: Int -> String
showMonth m | m == 1    = "January"
            | m == 2    = "February"
            | m == 3    = "March"
            | m == 4    = "April"
            | m == 5    = "May"
            | m == 6    = "June"
            | m == 7    = "July"
            | m == 8    = "August"
            | m == 9    = "September"
            | m == 10   = "October"
            | m == 11   = "November"
            | m == 12   = "December"
            | otherwise = "This ain't no month"

daySuffix :: Int -> String
daySuffix day
            | rem == 1  = "st"
            | rem == 2  = "nd"
            | rem == 3  = "rd"
            | otherwise = "th"
        where
            rem = day `mod` 10

