module WordGuess where
import Data.List
import Data.Maybe
import Data.Function
import Data.Ord
import Control.Monad.Writer
import System.Environment (getArgs)
import System.FilePath.Posix   
import Debug.Trace

type GFCode a = Writer Code a
data Code  = Code {abstract :: String, concrete :: String} 
data Prefixed = Pref {pref :: String, word :: String, whole :: String}
                 --(base  ,word  ,ext   )
type WordAnalyse = (String,String,String)

instance Monoid Code where  
  mempty = Code "" "" 
  (Code a1 c1) `mappend` (Code a2 c2) = Code (a1 ++ a2) (c1++ c2) 

instance Show Code where
  show c = "abstract:\n"++ abstract c++ "\n concrete:\n"++concrete c

emptyCode = Code "" ""

testlist = ["arbeta","arbetar","lekte","leker","vill"]

main1 = do
  (name:_) <- getArgs
  let vs    = take 100 testlist
  guessRound 0 vs name mempty

{- guessRound :: RoundNr -> Verbs -> GrammarFile -> SavedCode -> IO ...
tries to guess the declination of the verbs -}
guessRound :: Int -> [String] -> FilePath -> Code -> IO ([[String]],[Prefixed])
guessRound n list name saveCode = do
   let named = processVs list
   let words = map head named
   let (pref,code) = case n of
                          0 -> ([],execWriter $ run words)
                          _ -> runWriter $ runRetry saveCode words n
   writeGF code name
   return (map (map snd) named,pref)

  

writeGF :: Code -> FilePath -> IO ()
writeGF (Code a c) name = do 
    writeFile (addExtension name "gf")     abs
    writeFile (name++"Swe.gf")             conc
  where abs  = absHeader name a ++ a ++ "\n}"
        conc = concHeader name c++ c ++ "\n}"
writeNames :: [[String]] -> FilePath -> IO ()
writeNames strs path  = writeFile (addExtension (path++"Names") "hs") file
 where file  = namsHeader path++ toList (map toList strs)

toList :: [String] -> String
toList x =  "["++ intercalate "," x ++"]"

-- returns a list of verblist. Each verblist consists of pairs where the first is the
-- suggested name for the gf code and the second the verb form. Only the head of each
-- list is used in the gf grammar, the rest is used for testing
processVs :: [String] -> [[(String,String)]]
processVs = mkLemmaList
          . groupBy ((==) `on` wordStem) . sortBy (comparing wordStem) 
          . map cutExtension

processVGrouped :: [[String]] -> [[(String,String)]]
processVGrouped = mkLemmaList . map (map cutExtension)

mkLemmaList = map (giveName . map forgetEndings . sortBy bestEnd)

cutExtension :: String -> WordAnalyse
cutExtension word = (base,word,ext)
  where ext  = head $ filter (`isSuffixOf` word) extensions
        base = take (length word - length ext) word

giveName :: [(String,String)] -> [(String,String)]
giveName ws = zip names forms
  where forms = map snd ws
        word  = fst $ head ws
        names = map nrNames (zip (replicate (length forms) word) [0..]) 
        nrNames (x,y) =  normalize x++"_"++show y

forgetEndings :: WordAnalyse -> (String,String)
forgetEndings (base,word,ending) = (base,word)

-- add 's';  föddes, sågs ..
bestEndN :: WordAnalyse -> Int
bestEndN (_,_,"er")  = 0
bestEndN (_,_,"ar")  = 1
bestEndN (_,_,"r")   = 2
bestEndN (_,_,"dde") = 3
bestEndN (_,_,"tte") = 4   
bestEndN (_,_,"de")  = 5
bestEndN (_,_,"te")  = 6
bestEndN (_,_,"a")   = 8 
bestEndN _           = 7

bestEnd :: WordAnalyse -> WordAnalyse -> Ordering
bestEnd a b = comparing bestEndN a b

run :: [(String,String)] -> GFCode () 
run = mapM_ (writeVerb False) 

runRetry :: Code -> [(String,String)] -> Int -> GFCode [Prefixed] 
runRetry saveCode vs n = 
  tell saveCode >> mapM (writeVerbRetry n) vs >>= return . catMaybes 

runNames :: [[(String,String)]] -> [[String]]
runNames = map (map (addFnutts . snd))

toCode :: String -> String -> Code
toCode a c = Code (a++"\n") (c++"\n")

writeVerb :: Bool -> (String,String) -> GFCode ()
writeVerb retry (n,v) = tell $ writeVerbF func n [v] 
 where func = if retry then "(hoer" else "regV (guess" 

writeVerbF :: String -> String -> [String] -> Code
writeVerbF func n vs = let name = n++"_V" in
   toCode (name ++" : V ;") (name++" = "++func++" "++fnuttWords vs++");")  

-- could be smarter
writeVerbRetry :: Int -> (String,String) -> GFCode (Maybe Prefixed)
writeVerbRetry 1 (n,v) =
   case maybePrefixed v of
        Just p  -> return $ Just (Pref p (v \\ p) v)
        Nothing -> writeVerbRetry 2 (n,v) 
writeVerbRetry 2 (n,v) = writeVerb False (n,makeTo2de v) >> return Nothing

maybePrefixed :: String -> Maybe String
maybePrefixed = listToMaybe . (\w -> filter (`isPrefixOf` w) prefixes)

addPrefixed :: [String] -> Code
addPrefixed (a:str) = writeVerbF "(irregV " (normalize a) str

makeTo2de :: String -> String 
makeTo2de = (++"er") . wordStem . cutExtension
wordStem :: WordAnalyse -> String
wordStem (a,_,_) = a
addFnutts :: String -> String
addFnutts s = "\""++s++"\""
-------- Formating ---------------

fnuttWords :: [String] -> String
fnuttWords = unwords . map (\w -> "\""++w++"\"")
normalize :: String -> String
normalize ('å':xs) = "aa"++normalize xs
normalize ('ä':xs) = "ae"++normalize xs
normalize ('ö':xs) = "oe"++normalize xs
normalize ( x:xs ) = x:normalize xs
normalize []       = []

prefixes = ["av","an","om","be","bi","över","uppe","upp","under","till","klar",
            "inne","in","före","för","fram","fort","om","åter","vid","ut","ur",
            "upp","undan","på","åt","samman","sam","mot","med","god","genom",
            "åter","från","er"]

extensions =
  ["a","ar","er","as","ats","ades","des","ade","dde","tt","at","de","te"]++
  ["ende","nde","t",""]    -- "" is worst case
extensionsNouns = 
  ["or","er","ar","arna","orna","erna","en","et",""] -- plus s på allt
namsHeader name =     
  "module "++ name++"Names where "++"\n"++"namelist = "

absHeader name code = let lex = "IrregSweAbs" -- "Lexicon" in -- "DictSweAbs"
                          fun = if null code then "" else "fun" in
  "--# -path=.:/home/malin/GF/lib/src/swedish:prelude:alltenses \n"++
  "abstract "++name++" = "++lex++" ** { \n "++fun++"\n"

concHeader name code = let lex = "IrregSwe" -- "LexiconSwe" in -- "DictSwe"
                           lin = if null code then "" else "lin" in
  "--# -path=.:/home/malin/GF/lib/src/swedish/:/home/malin/GF/lib/src/scandinavian:/home/malin/GF/lib/src/common:/home/malin/GF/lib/src/abstract:../../prelude\n"++
  "concrete "++name++"Swe of "++name++
  " = "++lex++" ** open Prelude, ParadigmsSwe, GuessSwe,StructExtraSwe in { \n"++
  "flags coding=utf8 ;\n"++lin++"\n"


