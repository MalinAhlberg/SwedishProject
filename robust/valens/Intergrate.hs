import Data.List
import Data.List.Utils
import Data.Maybe
import Debug.Trace
import PGFHelp
import PGF


main = do
  dict <- extractLex id dict
  pgf  <- readPGF pgfFile
  let Just language = readLanguage "ValLex"
      morpho        = buildMorpho pgf language
      keeps = map (removeDuplicates morpho pgf) dict
  writeFile "SmallerDict.gf" $ unlines keeps

removeDuplicates :: Morpho -> PGF -> (String,String) -> String
removeDuplicates morpho pgf (lem,line) | isVerb lem = removeVerb
                                       | otherwise = line
 where removeVerb =
        let lst = [ (lemma,cat) 
                  | (lemma,an) <- lookupMorpho morpho (toStr lem)
                  , let cat = maybe "" (showType []) (functionType pgf lemma)
                  ]
            rem = null lst
        in if trace ((toStr lem)++" gives "++show lst) rem then line else ""
   
toStr = backTranslate . takeWhile (/='_')
verb  = fromJust $ readType "V"
dict  = "../../saldo/DictSwe.gf"
pgfFile   = "ValLexAbs.pgf"
isVerb = ("V" `isSuffixOf`)
backTranslate str = foldr ($) str 
  [replace a b | (a,b) <- [("ae","ä"),("aa","å"),("oe","ö")]] 
