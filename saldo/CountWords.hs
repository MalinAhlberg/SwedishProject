module Main where
import XMLHelp

import Data.Tree 
import Data.Maybe
import Data.Char
import Data.List
import Data.Function
import PGF hiding (parse,Tree)
import Control.Monad
import Control.Monad.Writer
import Control.Applicative

import Debug.Trace

type WWriter = Writer ([Fail],[(String,String)],[Diff]) ()
data Diff    = D {lemma  :: String, posTag :: String
                 ,gfCat :: String, sID    :: String}
  deriving (Eq,Ord)
instance Show Diff where
  show (D w p c i) = w++"\t"++c++"\t"++p++"\t"
data Fail = F {word' :: String, posT :: String, sid :: String}
  deriving (Eq,Ord)
instance Show Fail where
  show (F w p i) = w++"\t"++p++"\t"++i

talbanken = "../Talbanken05_20060604/FPS/P.tiger.xml"
pgfFile   = "../gf/Big.pgf"
test = "../mapping/test2.xml"
testpgf = "../gf/BigTest.pgf"
lang :: Language
lang = read  "BigSwe" --"BigTestSwe" --

main = do 
 putStrLn $ "Parsing talbanken "++talbanken++" ..."
 trees <- head <$> parse talbanken
 putStrLn $ "Reading PGF "++pgfFile++" ..."
 pgf   <- readPGF pgfFile
 putStrLn $ "Testing words ..."
 let (f,n,d) = execWriter $ getUnknown pgf (buildMorpho pgf lang) trees 
 putStrLn $ "Done! Writing the files NamesT,FailsT,DiffsT"
 writeFile "NamesT"  $ process fst n
 writeFile "FailsT"  $ process word' f
 writeFile "DiffsT"  $ process lemma  d
 
process :: (Show a, Eq b, Ord a) => (a -> b) -> [a] -> String
process x  = unlines . map show 
           . sortBy (compare `on` fst )  
           . map (\a -> (length a,take 1 a))
           . groupBy ((==) `on` x) . sort 

getUnknown :: PGF -> Morpho -> [(String,Tree String)] -> WWriter 
getUnknown pgf morpho = mapM_ (uncurry $ searchSentence pgf morpho)
  where searchSentence :: PGF -> Morpho -> String -> Tree String -> WWriter
        searchSentence pgf m id (Node pos [Node w []]) = check pgf m id pos w
        searchSentence pgf m id (Node pos ts) = mapM_ (searchSentence pgf m id) ts
        -- something weird with pos and w order.. Believe it is fixed now
        check :: PGF -> Morpho -> String -> String -> String -> WWriter
        check pgf morpho id pos w
           | any (`isPrefixOf` pos) ["PN","MN"] = tellName (w,id)
           | otherwise                          = do
             let-- ok  = findWord w 
                 (ok,cats) = findWord (map toLower w)
             unless ok $ tellFail (w,pos,id)
             when (ok && isJust cats) $ tellDiff (w,pos,(fromJust cats),id)
          where findWord w = 
                 let xs = [cat | (lemma,an) <- lookupMorpho morpho w
                          ,let cat = maybe "" (showType []) (functionType pgf lemma)]
                 in (not $ null xs, listToMaybe $ filter (not . (`isSameCat` pos)) xs)


                isSameCat ('V':_) v  = take 1 (tail v) == "V"
                isSameCat ('N':_) n  = take 1 (tail n) == "N"
                isSameCat "Adv"   ad = take 2 ad == "AB"
                isSameCat "AJ"    a  = take 2 a  == "AJ"
                isSameCat "Pep"   pr = take 2 pr == "PR"
                isSameCat _       _  = True
tellName :: (String,String) -> WWriter 
tellName x = tell ([],[x],[])  -- write to special file
tellFail (w,p,i)   = tell ([F w p i],[],[])
tellDiff (w,p,c,i) = tell ([],[],[D w p c i])


testa  str = do
  pgf <- readPGF testpgf
  let language = lang --Just language = readLanguage "BigTestSwe"
      morpho        = buildMorpho pgf language
  return [(lemma,an,cat) | (lemma,an) <- lookupMorpho morpho str
                   ,let cat = maybe "" (showType []) (functionType pgf lemma)]

format :: (String,String) -> String
format (a,b)  = a++"\t"++b
