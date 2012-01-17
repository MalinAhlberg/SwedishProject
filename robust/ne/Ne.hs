module Ne where
import Data.Char

isName :: [String] -> [String]
isName (x:xs)  = replace (x:xs) 
isName []      = []

isNameStart :: String -> Bool
isNameStart xs = isUpper (head xs) || isNameSpec xs

isNameSpec = (`elem` ["von","af"])

replace :: [String] -> [String]
replace (x:y:xs) | isUpper (head x) && isCombiner y = "X" : replace (dropName xs)
replace (x:xs)   | isNameSpec x     = "X" : dropName xs
                 | isUpper (head x) = "X" : replace (dropName xs)
                 | otherwise        = x : replace xs
replace [] = []

dropName (x:xs)   | isUpper (head x) = dropName xs
                  | isNameSpec x     = dropName xs
                  | otherwise        = x:xs
dropName []  = []

isCombiner = (`elem` ["&","och"])
