module Ne where
import Data.Char
import PGF
import Control.Arrow

{- Same as ../ne/NE.hs but for tagged sentences
   Animate/inanimate? Look at rest of sentence, or make alternatives. (vote
   down GenNP when parsing)
   Nominative/genitive? Put as both if ends with s?
 -}
  
type NameTag = String
type Tag = String


-- Puts the gf name (eg. X0) in first place and the old name in second
isName :: Morpho -> [(String,Tag,Tag)] -> [(String,NameTag,Tag)]
isName m ((x,t,tb):xs)  
    -- first word is untagged and not in dict -> could be name
     | t=="" && null (lookupMorpho m (lower x)) = replace 0 ((x,t,tb):xs) 
     | otherwise                                = (x,t,tb):replace 0 xs
isName m []      = []

lower (x:xs) = toLower x:xs
lower y      = y


isNameSpec :: (String,Tag,Tag) -> Bool
isNameSpec (s,t,tb) = t=="" && (s `elem` ["von","af"])

replace :: Int -> [(String,Tag,Tag)] -> [(String,NameTag,Tag)]
replace n (x:xs)  | isNameSpec x  || looksNamish x = 
                                      let (s,tgd,rest) = dropName ('-', xs)
                                      in  (gfName n s,word x,tbtag x) : tgd 
                                          ++ replace (n+1) rest 
                  | otherwise  = x : replace n xs  -- if already is tagged, or not a name
replace n [] = []

word  (w,t,tb) = w
tbtag (w,t,tb) = tb 


-- To remove second parts of names (Malin M Ahlberg -> X)
-- puts 'xx' instead  (Malin M Ahlberg -> ["X","xx","xx"])
dropName :: (Char,[(String,Tag,Tag)]) -> (Char,[(String,NameTag,Tag)],[(String,Tag,Tag)])
dropName (c,x:xs)   | looksNamish x || isNameSpec x
                        =  let (c',tgd,str) = dropName ('-',xs)
                           in  (c',("xx",word x,tbtag x):tgd,str)
                    | otherwise     = (c,[],x:xs)
dropName (c,[])  =  (c,[],[])

isCombiner = (`elem` ["&"])

looksNamish :: (String,Tag,Tag) -> Bool
looksNamish ([],t,tb) = False  -- for compounds which should not be analysed
looksNamish (s,t,tb) = isUpper (head s) && notPn s && t==""

notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder","Ni"])

gfName n 's' = 'X' : show n  -- when the name ends with an s, it might be genitive
gfName n _   = 'Y' : show n

