module Ne where
import Data.Char
import PGF
import Control.Arrow

{- Beginning of sentence? Look up in morpho and mark as name if not present?
   Animate/inanimate? Look at rest of sentence, or make alternatives. (vote down GenNP when parsing)
   Nominative/genitive? Put as both if ends with s?
   -}
  
type NameTag = String

isName :: Morpho -> [String] -> [(String,NameTag)]
isName m (x:xs)  | null (lookupMorpho m (lower x)) = replace 0 (x:xs) -- first word not in dict -> is name
                 | otherwise                       = ((x,""):replace 0 xs)
{-= replace (lower x:xs) -}

isName m []      = []

lower (x:xs) = (toLower x:xs)
lower y      = y

--isNameStart :: String -> Bool
--isNameStart xs = isUpper (head xs) || isNameSpec xs

isNameSpec = (`elem` ["von","af"])

replace :: Int -> [String] -> [(String,NameTag)]
replace n (x:xs)  | isNameSpec x  || looksNamish x = 
                                      let (s,tgd,rest) = dropName ('-', xs)
                                      in  (x,gfName n s) : tgd ++ replace (n+1) rest 
                | otherwise  = (x,"") : replace n xs
replace n [] = []

-- To remove second parts of names (Malin M Ahlberg -> X)
-- Leaves an empty string instead  (Malin M Ahlberg -> ["X","xx","xx"])
dropName :: (Char,[String]) -> (Char,[(String,NameTag)],[String])
dropName (c,x:xs)   | looksNamish x || isNameSpec x
                        =  let (c',tgd,str) = dropName ('-',xs)
                           in  (c',(x,"xx"):tgd,str)
                    | otherwise     = (c,[],x:xs)
dropName (c,[])  =  (c,[],[])

isCombiner = (`elem` ["&"])

looksNamish :: String -> Bool
looksNamish s = isUpper (head s) && notPn s

notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder"])

gfName n 's' = "X"++show n  -- when the name ends with an s, it might be genitive
gfName n _   = "Y"++show n

