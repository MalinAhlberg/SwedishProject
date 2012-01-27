module Ne where
import Data.Char
import PGF
import Control.Arrow

{- Same as ../ne/NE.hs but for tagged sentences
   Beginning of sentence? Look up in morpho and mark as name if not present?
   Animate/inanimate? Look at rest of sentence, or make alternatives. (vote down GenNP when parsing)
   Nominative/genitive? Put as both if ends with s?
   -}
  
type NameTag = String
type Tag = String

isName :: Morpho -> [(String,Tag)] -> [(String,NameTag)]
isName m ((x,t):xs)  
    -- first word is untagged and not in dict -> could be name
     | t=="" && null (lookupMorpho m (lower x)) = replace 0 ((x,t):xs) 
     | otherwise                                = ((x,t):replace 0 xs)
isName m []      = []

lower (x:xs) = (toLower x:xs)
lower y      = y


isNameSpec :: (String,Tag) -> Bool
isNameSpec (s,t) = t=="" && (s `elem` ["von","af"])

replace :: Int -> [(String,Tag)] -> [(String,NameTag)]
replace n (x:xs)  | isNameSpec x  || looksNamish x = 
                                      let (s,tgd,rest) = dropName ('-', xs)
                                      in  (fst x,gfName n s) : tgd ++ replace (n+1) rest 
                  | otherwise  = x : replace n xs  -- if already is tagged, or not a name
replace n [] = []

-- To remove second parts of names (Malin M Ahlberg -> X)
-- Leaves an empty string instead  (Malin M Ahlberg -> ["X","xx","xx"])
dropName :: (Char,[(String,Tag)]) -> (Char,[(String,NameTag)],[(String,Tag)])
dropName (c,x:xs)   | looksNamish x || isNameSpec x
                        =  let (c',tgd,str) = dropName ('-',xs)
                           in  (c',(fst x,"xx"):tgd,str)
                    | otherwise     = (c,[],x:xs)
dropName (c,[])  =  (c,[],[])

isCombiner = (`elem` ["&"])

looksNamish :: (String,Tag) -> Bool
looksNamish (s,t) = isUpper (head s) && notPn s && t==""

notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder"])

gfName n 's' = "X"++show n  -- when the name ends with an s, it might be genitive
gfName n _   = "Y"++show n

