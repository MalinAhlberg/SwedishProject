import Control.Monad
import Control.Applicative
import Data.List
import qualified Data.ByteString.UTF8 as B
import Debug.Trace

import ParseLexin

-- implement for v3 as well now :D
main = do
  inp <- lines <$> readFile "littlelexin.txt" --"../../saldo/valencies/lexin.txt"
  let res = unlines [v++": "++show arg | (v,arg) <- getIt inp]
  putStrLn res
  writeFile "v2.two" res


getIt :: [String] -> [(String,V2Arg)]
getIt (x:y:xs) = trace ("parse "++x) $
    case parseValency x of
         Right v2 -> (combine y v2) ++ getIt xs
         Left  _  -> getIt (y:xs)
getIt _ = []         

combine :: String -> V2Arg -> [(String,V2Arg)]
combine ys (a,p) = case parseWords ys of
    Right xs -> map (\(w,ag) -> (B.toString w,(ag++a,p)) xs
    Left  _  -> printErr $ "fail on"++ys

mkLexicon :: (String,V2Arg) -> Code
mkLexicon (w,arg) = do
  lem <- lookupInDict w
  let w'  = wrapFunctions w (fst arg)
      w'' = addPrep w (snd arg)
 where wrapFunctions w (Refl p:xs) = wrapFunctions ("reflV ("++ w++")") xs
       wrapFunctions w (Part p:xs) = wrapFunctions ("partV ("++ w++") "++ p++")")
       wrapFunctions w [] = w
       addPrep w [p]  = "mkV2 ("++w++") mkPrep ("++p++")"
       addPrep w []   = "dirV2 ("++w++")"
        

printErr s = trace s $ []
