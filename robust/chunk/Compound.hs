module Compound where
import qualified Data.HashMap as M
import Data.Text (Text,pack,unpack,isPrefixOf)
import Data.Char
import Data.Maybe

import ParseSaldo

-- Freely translated after compound.py by Martin Hammarstedt (and Marcus
-- Forsberg?)

main :: IO ()
main = try "glasskålar havregrynsgröt gatubokmaskin jättefina gatuhörnstriumf"


try :: String -> IO ()
try str = do
   lex <- getSaldo 
   print $ map (compound lex) (words str) 



prefixesSuffixes :: String -> [(String,String)]
prefixesSuffixes w = [(take n w,drop n w) | n <- [1..length w-1]]

exception :: [Char] -> Bool
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
        , let ps  = sandhi b p s
        , let pre = catMaybes (map (isInLexPre saldoLex) ps)
        , pre /= []
        , let ok = isInLex saldoLex s
        , let xs = compound' True saldoLex s 
        , let ss = if isJust ok then [[s]] else xs]

isInLex :: Lex -> String -> Maybe Text
isInLex lex w =  fmap (snd . head) $ M.lookup (pack w) lex
isInLexPre :: Lex -> String -> Maybe String
isInLexPre lex w = let forms = concat $ maybeToList $ M.lookup (pack w) lex
                       isOk  = listToMaybe . filter ((pack "c" `isPrefixOf`) . fst)
                   in if isJust $ isOk forms then getInfForm forms else Nothing 
  where getInfForm ((_,x):xs) = Just $ takeWhile isAlpha $ unpack x


  -- tag should not be: (`elem` ["cm","ci","c"] ). Make explicit if other 
  -- tags starting on 'c' is added in Saldo

