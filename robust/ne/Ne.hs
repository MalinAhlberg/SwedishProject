module Ne where
import Data.Char
import PGF
import Control.Arrow

{- Beginning of sentence? Look up in morpho and mark as name if not present?
   Animate/inanimate? Look at rest of sentence, or make alternatives. (vote down GenNP when parsing)
   Nominative/genitive? Put as both if ends with s?
   -}
isName :: Morpho -> [String] -> [String]
isName m (x:xs)  | null (lookupMorpho m (lower x)) = replace (x:xs) -- first word not in dict -> is name
                 | otherwise                       = (x:replace xs)
{-= replace (lower x:xs) -}

isName m []      = []

lower (x:xs) = (toLower x:xs)
lower y      = y

--isNameStart :: String -> Bool
--isNameStart xs = isUpper (head xs) || isNameSpec xs

isNameSpec = (`elem` ["von","af"])

replace :: [String] -> [String]
--replace (x:y:xs) | isUpper (head x) && isCombiner y =
replace (x:xs)  | isNameSpec x  || looksNamish x = 
       let (s,rest) = dropName ('-', xs)
       in  gfName s : replace xs
                | otherwise        = x : replace xs
replace [] = []

dropName :: (Char,[String]) -> (Char,[String])
dropName (c,x:xs)   | isUpper (head x) = second ("":) $ dropName (last x,xs)
                    | isNameSpec x     = second ("":) $ dropName ('-',xs)
                    | otherwise        = (c,x:xs)
dropName (c,[])  =  (c,[])

isCombiner = (`elem` ["&"])

looksNamish :: String -> Bool
looksNamish s = isUpper (head s) && notPn s

notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder"])

gfName 's' = "X2"  -- when the name ends with an s, it might be genitive
gfName _   = "X1"

{-
tagName :: Morpho -> [(Tag,String)] -> [String]
tagName m  = isName 
-}
