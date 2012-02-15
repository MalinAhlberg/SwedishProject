module Compound where
import qualified Data.HashMap as M
import Data.Text (Text,pack,unpack,isPrefixOf)
import Data.Char
import Data.Maybe
import Control.Arrow

import ParseSaldo

-- Freely translated after compound.py by Martin Hammarstedt (and Marcus
-- Forsberg?)

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
sandhi :: Bool -> String -> String -> [String]
sandhi b prefix suffix 
  | last prefix == head suffix && last prefix `elem` "bdfgjlmnprstv"
    = [prefix,prefix++[last prefix]]
  | b && last prefix == 's'      -- drops binde-s
    = [prefix,init prefix]
  | otherwise = [prefix] 

type Lex = M.Map Text [(Text,Text)]

compound :: Lex -> String -> [[String]]
compound lex = compound' False lex
compound' :: Bool -> Lex -> String -> [[String]]
compound' b saldoLex w = concat 
  [ [pr:sf | pr <- pre, sf <- ss]
        | (p,s) <- prefixesSuffixes w, not (exception s)
        --, let p' = isInLexPre saldoLex p
        --, isJust p'
        , let ps  = sandhi b p s
        , let pre = catMaybes (map (isInLexPre saldoLex) ps)
        , pre /= []
        , let ok = isInLex saldoLex s
        , let xs = compound' True saldoLex s 
        , let ss = if isJust ok then [[s]] else xs]

comb :: Bool -> String -> [String] -> [[String]]
comb b p (s:ss) =  [p':s:ss | p' <- sandhi b p s]

{-
f g (a:b:xs) = map ((++ concat (f g xs)) .  (:[b])) (g a b)
f _ [x]       = [[x]]
f _ []        = []
-}

--isInLex, isInLexPre :: Lex -> String -> Maybe Text
isInLex lex w =  fmap (snd . head) $ M.lookup (pack w) lex
isInLexPre lex w = let forms = concat $ maybeToList $ M.lookup (pack w) lex
                       isOk  = listToMaybe . filter ((pack "c" `isPrefixOf`) . fst)
                   in if isJust $ isOk forms then getInfForm forms else Nothing -- fmap snd $ isOk forms
  where getInfForm ((_,x):xs) = Just $ takeWhile isAlpha $ unpack x

  -- tag should not be: (`elem` ["cm","ci","c"] ). Make explicit if other 
  -- tags starting on 'c' is added in Saldo

