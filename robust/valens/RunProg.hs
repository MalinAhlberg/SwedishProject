import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Map as M hiding (map, delete, filter)
import Data.Ord
import qualified Data.Text as T 
import Debug.Trace
import System.IO
import PGF

import ParseLexin

inputTest = "littlelexin.txt"
lexin = "../../saldo/valencies/lexin.txt"

mainLexin = do
  lex <- getLexin
  let res = unlines $ map show lex
  --putStrLn res
  writeFile "preps.six" res

getLexin = do
  inp <- lines <$> readFile {-"testlexin.txt" --inputTest --} lexin 
  return $ getIt inp

getIt :: [String] -> [(String,VerbType)]
getIt (x:y:xs) = --trace ("parse "++x) $
    case parseValency x of
         Right val -> concatMap (combine y) val ++ getIt xs
         Left  _   -> getIt (y:xs)
getIt _ = []         

combine :: String -> VerbType -> [(String,VerbType)]
combine ys (VT t a p) = case parseWords ys of
    Right xs -> map (\(w,ag) -> (T.unpack w,VT t (ag++a) p)) xs
    Left  _  -> printErr $ "fail on"++ys

testa = do
  pgf <- readPGF "../../gf/BigTest.pgf"
  let morpho = buildMorpho pgf (read "BigTestSwe")
  lex <- mkLexMap
  mkLexicon morpho pgf lex
              ("tänka",VT {vtype = V2, argument = []
              , preps = [Just (T.pack "på"),Nothing]})


main = do
   putStrLn "Parsing lexin..."
   vs <- getLexin
   trace (show vs) $ putStrLn "Reading pgf..."
   pgf <- readPGF pgfFile
   putStrLn "Building morpho..."
   let morpho = buildMorpho pgf lang
   putStrLn "Creating map of lemmas..."
   lex <-  mkLexMap
   putStrLn "Extracting new dictionary..."
   mapM (\v -> mkLexicon morpho pgf lex v >>= writeCode) vs
   cleanDicts
   return ()

-- to avoid name clashes
cleanDicts :: IO ()
cleanDicts = do
  abs <- getAndOrder newabs
  cnc <- getAndOrder newcnc
  writeFile "NewAbs.gf" abs
  writeFile "NewCnc.gf" cnc
  writeGF $!! (abs,cnc)

 where getAndOrder :: FilePath -> IO String
       getAndOrder file = do
            lst <- extractLex tail file   -- should remove replicate code
            let sortIt = concatMap addIndicies . group . sort
            return $ unlines $ map format $ sortIt lst
       addIndicies [x] = [x]
       addIndicies xs = addI 1 xs
       -- TODO do not add index at the very end (V21 ?)
       --      empty files before rewriting
       addI n (x:xs)  = first (++show n) x : addI (n+1) xs
       addI _ []      = []
       format (l,c)   = " "++l++"\t"++c


writeCode = maybe (return ()) (writeGF . formatGF)
writeGF (abs,cnc) = do
  appendFile newabs abs
  appendFile newcnc cnc

formatGF :: (Code,CId,V) -> (Code,Code)
formatGF (code,name,v) = let entry = showCId name
                         in (entry++" : "++show v++";\n"
                            ,entry++" = "++code++";\n")

mkLexicon :: Morpho -> PGF -> Map String String -> (String,VerbType) 
             -> IO (Maybe (Code,CId,V))
mkLexicon morpho pgf lex (w,arg) = do
  let v = vtype arg
  case lookupInDict w morpho pgf lex of
      Just (cid,forms) -> do 
              putStrLn $ show forms
              let w'  = wrapFunctions forms (argument arg)
                  w'' = addPrep (vtype arg) w' (tidy $ preps arg)
              return $ Just (w'',mkName cid v,v)
      _                 -> return Nothing 

 {- TODO  We should avoid double reflexives etc, so the identifiers should  be 
   checked for this ('_sig_' or '_till', and the wrapFunction modified
   to make up for duplication (Could also look at type, VR is reflexive -}
 where wrapFunctions w (Refl  :xs) = wrapFunctions ("reflV ("++ w++")") xs
       wrapFunctions w (Part p:xs) = wrapFunctions ("partV ("++ w++") \""
                                     ++ T.unpack p++"\")") xs
       wrapFunctions w [] = w

       {- The order in which things are done assumes that particles and 
          reflexive objects cannot occure after the prepositions -}
       addPrep V3     w [p1,p2] = put ["mkV3 (",w,") ",toPrep p1,toPrep p2]
                                  --,": V3 ")
       addPrep (VV b) w [p]     = put [w,"** {c2 = mkComplement [",part 
                                        ,inf b,"]","; lock_VV = <>} ;"]
                                  --,": VV")
          where part = maybe "" ((++"\""). ('\"':). T.unpack) p
       addPrep V2     w [p]     = mkVerb2 "mkV2"  w p
       addPrep V2S    w [p]     = mkVerb2 "mkV2S" w p
       addPrep V2Q    w [p]     = mkVerb2 "mkV2Q" w p
       addPrep V2A    w [p]     = mkVerb2 "mkV2A" w p
       -- TODO VA with "som" are not real VAs!!
       addPrep VA     w  p      = mkVerb2 "mkVA"  w (list2maybe p)
       addPrep VS     w _       = mkVerb  "mkVS"  w 
       addPrep VQ     w _       = mkVerb  "mkVQ"  w
       addPrep V      w _       = w 
       addPrep v      w x       = printErr $ "oops non-exhaustive pattern!" ++  show v ++ w ++show x
       mkVerb  f w   = f ++" ("++w++") "
       mkVerb2 f w p = mkVerb f w ++toPrep p
       put = unwords

       toPrep Nothing       = "noPrep"
       toPrep (Just p)      = "(mkPrep \""++ T.unpack p++"\")"
       inf True  = " att"
       inf _     = ""
       tidy = delete Nothing
       list2maybe :: [Maybe a] -> Maybe a
       list2maybe (x:_) = x
       list2maybe []    = Nothing

mkName :: CId -> V -> CId
mkName s v = let name = takeWhileList (not . ("_V" `isPrefixOf`)) $ showCId s --init $ init (showCId s) -- drops '_V', do properly
                    in mkCId $ name ++ "_"++showV v
    where showV (VV _) = "VV"
          showV v      = show v

lookupInDict :: String -> Morpho -> PGF -> Map String String -> Maybe (CId,Code)
lookupInDict str morpho pgf dict = 
   -- no error message when the verb has the wrong type!
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

extractLemma :: Map String String -> CId -> Maybe (CId,Code)
extractLemma lex w  
  | Just a <- M.lookup (showCId w) lex =
            let (l,code) = span (/='=') a
                (f,_)    = span (/=';') $ drop 1 code
            in trace f $ Just (w,f)
  | otherwise =  Nothing

                       
mkLexMap :: IO (Map String String)
mkLexMap = liftM M.fromList $ extractLex id cncFile
              
extractLex :: ([String] -> [String]) -> FilePath -> IO [(String,String)]
extractLex f file = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8 
  liftM mkMap $ readFile file
 where mkMap :: String -> [(String,String)]
       mkMap lex = [(head ws, unwords (f ws)) | l <- lines lex
                                 , let ws = words l
                                 , not $ Data.List.null ws]

cncFile = "../../saldo/DictSwe.gf"
pgfFile = "../../saldo/DictSweAbs.pgf"
lang :: Language
lang = read "DictSwe"
newabs = "TestAbs.gf"
newcnc = "TestCnc.gf"
 
($!!) f a = f (deepseq a a)

type Code = String

printErr s = trace s $ []
