import Control.Monad
import Control.Applicative
import Data.List
import qualified Data.ByteString.UTF8 as B
import Debug.Trace

import ParseLexin

main = do
  inp <- lines <$> readFile "littlelexin.txt" --"../../saldo/valencies/lexin.txt"
  let res = unlines $ map show $ getIt inp --[v++": "++show arg | (v,arg) <- getIt inp]
  putStrLn res
  writeFile "v2-3.one" res


getIt :: [String] -> [(String,VerbType)]
getIt (x:y:xs) = trace ("parse "++x) $
    case parseValency x of
         Right val -> concatMap (combine y) val ++ getIt xs
         Left  _   -> getIt (y:xs)
getIt _ = []         

combine :: String -> VerbType -> [(String,VerbType)]
combine ys (VT t a p) = case parseWords ys of
    Right xs -> map (\(w,ag) -> (B.toString w,VT t (ag++a) p)) xs
    Left  _  -> printErr $ "fail on"++ys

mkLexicon :: (String,VerbType) -> IO Code
mkLexicon (w,arg) = do
  lem <- lookupInDict w
  let w'  = wrapFunctions w (argument arg)
      w'' = addPrep w (preps arg)
  return w''
 where wrapFunctions w (Refl  :xs) = wrapFunctions ("reflV ("++ w++")") xs
       wrapFunctions w (Part p:xs) = wrapFunctions ("partV ("++ w++") "++ B.toString p++")") xs
       wrapFunctions w [] = w
       addPrep w [Just p]  = "mkV2 ("++ w++") mkPrep ("++B.toString p++")"
       addPrep w [Nothing]   = "dirV2 ("++w++")"
       lookupInDict = undefined 
type Code = String

printErr s = trace s $ []
