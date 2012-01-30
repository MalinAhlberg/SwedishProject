module Compound where
import qualified Data.HashMap as M
import Data.Text (Text,pack,unpack,isPrefixOf)
import Data.Maybe
import Control.Arrow

import ParseSaldo

-- Freely translated after compound.py

main = try "glasskålar havregrynsgröt gatubokmaskin jättefina gatuhörnstriumf"


try str = do
   lex <- getSaldo 
   print $ map (compound lex) (words str) 



prefixesSuffixes :: String -> [(String,String)]
prefixesSuffixes w = [(take n w,drop n w) | n <- [1..length w-1]]

exception = (`elem`
   [ "il", "ör", "en", "ens", "ar", "ars"
   ,"or", "ors", "ur", "urs", "lös", "tik", "bar"
   ,"lik", "het", "hets", "lig", "ligt", "te", "tet", "tets"
   ,"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"
   ,"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x"
   ,"y", "z", "ä"])


-- ("glas", "skål") --> ("glas", "skål"), ("glass", "skål")
sandhi :: Bool -> (String,Text) -> (String,Text) -> [(String,Text)]
sandhi b (prefix,e) (suffix,_) 
  | last prefix == head suffix && last prefix `elem` "bdfgjlmnprstv"
    = [(prefix,e),(prefix++[last prefix],e)]
  | b && last prefix == 's'      -- drops binde-s
    = [(prefix,e),(init prefix,e)]
  | otherwise = [(prefix,e)] 

type Lex = M.Map Text [(Text,Text)]

compound :: Lex -> String -> [[(String,String)]]
compound lex = map (map (second unpack)) . compound' False lex
compound' :: Bool -> Lex -> String -> [[(String,Text)]]
compound' b saldoLex w = concat $ concat
  [ map (f (sandhi b) . ((p,fromJust p'):)) ss
        | (p,s) <- prefixesSuffixes w, not (exception s)
        , let p' = isInLexPre saldoLex p
        , isJust p'
        , let ok = isInLex saldoLex s
        , let xs = compound' True saldoLex s 
        , let ss = if isJust ok then [[(s,fromJust ok)]] else xs]

f g (a:b:xs) = map ((++ concat (f g xs)) .  (:[b])) (g a b)
f _ [x]       = [[x]]
f _ []        = []

isInLex, isInLexPre :: Lex -> String -> Maybe Text
isInLex lex w =  fmap (snd . head) $ M.lookup (pack w) lex
isInLexPre lex w = let forms = concat $ maybeToList $ M.lookup (pack w) lex
                       isOk  = listToMaybe . filter ((pack "c" `isPrefixOf`) . fst)
                   in fmap snd $ isOk forms

  -- tag should not be: (`elem` ["cm","ci","c"] ). Make explicit if other 
  -- tags starting on 'c' is added in Saldo

