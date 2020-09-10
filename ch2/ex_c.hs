-- 1. toUpper
-- 2. unwords concatenates all words with separating spaces. Therefore, both equations hold if separated on spaces, but since words also separates on tabs...
-- 3. try x : xs

import Data.Char

modernise :: String -> String
modernise = unwords . (map (\xs -> (toUpper (head xs)) : (tail xs))) . words
