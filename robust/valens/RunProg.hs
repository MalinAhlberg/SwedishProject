import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Map as M hiding (map, delete, filter)
import Data.Ord
import qualified Data.ByteString.UTF8 as BU
import Debug.Trace
import System.IO
import PGF

import ParseLexin

inputTest = "littlelexin.txt"
lexin = "../../saldo/valencies/lexin.txt"

main = do
  inp <- lines <$> readFile lexin 
  let res = unlines $ map show $ getIt inp 
  --putStrLn res
  writeFile "preps.five" res


getIt :: [String] -> [(String,VerbType)]
getIt (x:y:xs) = --trace ("parse "++x) $
    case parseValency x of
         Right val -> concatMap (combine y) val ++ getIt xs
         Left  _   -> getIt (y:xs)
getIt _ = []         

combine :: String -> VerbType -> [(String,VerbType)]
combine ys (VT t a p) = case parseWords ys of
    Right xs -> map (\(w,ag) -> (BU.toString w,VT t (ag++a) p)) xs
    Left  _  -> printErr $ "fail on"++ys

testa = do
  pgf <- readPGF "../../gf/BigTest.pgf"
  let morpho = buildMorpho pgf (read "BigTestSwe")
  lex <- mkLexMap
  mkLexicon ("tänka",VT {vtype = V2, argument = [], preps = [Just (BU.fromString "på"),Nothing]})
            morpho pgf lex

mkLexicon :: (String,VerbType) -> Morpho -> PGF -> Map String String -> IO (Maybe (Code,Code))
mkLexicon (w,arg) morpho pgf lex = do
  case lookupInDict w morpho pgf lex of
      Just forms -> do 
              putStrLn $ show forms
              let w'  = wrapFunctions forms (argument arg)
                  w'' = addPrep (vtype arg) w' (tidy $ preps arg)
              return $ Just (w'',mkName w'' (vtype arg))
      _                 -> return Nothing 
 where wrapFunctions w (Refl  :xs) = wrapFunctions ("reflV ("++ w++")") xs
       wrapFunctions w (Part p:xs) = wrapFunctions ("partV ("++ w++") "++ BU.toString p++")") xs
       wrapFunctions w [] = w

       {- The order in which things are done assumes that particles and 
          reflexive objects cannot occure after the prepositions -}
       addPrep V3     w [p1,p2] = "mkV3 ("++ w++") "++ toPrep p1 ++ toPrep p2
                                  --,": V3 ")
       addPrep (VV b) w [p]     = w ++"** {c2 = mkComplement ["++ part 
                                   ++inf b++"]" ++"; lock_VV = <>} ;"
                                  --,": VV")
          where part = maybe "" BU.toString p
       addPrep V2     w [p]     = mkVerb2 "mkV2"  w p
       addPrep V2S    w [p]     = mkVerb2 "mkV2S" w p
       addPrep V2Q    w [p]     = mkVerb2 "mkV2Q" w p
       addPrep V2A    w [p]     = mkVerb2 "mkV2A" w p
       addPrep VS     w _       = mkVerb  "mkVS"  w 
       addPrep VQ     w _       = mkVerb  "mkVQ"  w
       addPrep V      w _       = w 
       addPerd v      w x       = printErr $ "oops non-exhaustive pattern!" ++  show v ++ w ++show x
       mkVerb  f w   = f ++" ("++w++") "
       mkVerb2 f w p = mkVerb f w ++toPrep p


       toPrep Nothing       = "noPrep"
       toPrep (Just p)      = "mkPrep ("++BU.toString p++")"
       inf True  = " att"
       inf _     = ""
       tidy = delete Nothing

       mkName :: String -> V -> Code
       mkName s v = s --undefined 

lookupInDict :: String -> Morpho -> PGF -> Map String String -> Maybe Code
lookupInDict str morpho pgf dict = 
  let allLemmas = [ l | (l,a) <- lookupMorpho morpho str 
                      ,let cat = maybe "" (showType []) (functionType pgf l)
                      ,cat=="V"]
      lemma = listToMaybe $ smallestLemma allLemmas
  in maybe (trace ("could not find verb "++str) Nothing)
           (trace (show lemma) (extractLemma dict)) lemma
 where smallestLemma :: [CId] -> [CId]
       smallestLemma = sortBy (comparing rate)
       rate :: CId -> Int
       rate = length . filter (=='_') . showCId

extractLemma :: Map String String -> CId -> Maybe Code
extractLemma lex w  
  | Just a <- M.lookup (showCId w) lex =
            let (l,code) = span (=='=') a
                (f,_)    = span (==';') code
            in trace f (Just f )
  | otherwise =  Nothing

                       
mkLexMap :: IO (Map String String)
mkLexMap = do map <- ex "../../gf/BigTestSwe.gf"
              trace (show map) (return map)
  --abs <- ex bigLexAbs newLexAbs
--  return (cnc,abs)
 where ex file = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> (liftM mkMap $ readFile file)
       mkMap :: String -> (Map String String)
       mkMap lex = M.fromList [(head ws, l) | l <- lines lex
                                                 , let ws = words l
                                                 , not $ Data.List.null ws]
 
type Code = String

printErr s = trace s $ []
