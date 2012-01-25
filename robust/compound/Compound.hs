module Compound where

--annotate outPrefix outSuffix word msd model = 

prefixes_suffixes :: String -> [(String,String)]
prefixes_suffixes w = [(take n w,drop n w) | n <- [1..length (w-1)]]

exception = (`elem`
   [ "il", "ör", "en", "ens", "ar", "ars"
   ,"or", "ors", "ur", "urs", "lös", "tik", "bar"
   ,"lik", "het", "hets", "lig", "ligt", "te", "tet", "tets"
   ,"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"
   ,"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x"
   ,"y", "z", "ä"])


-- ("glas", "skål") --> ("glas", "skål"), ("glass", "skål")
sandhi :: String -> String -> [(String,String)]
sandhi prefix suffix 
  | last prefix == head suffix && last prefix `elem` "bdfgjlmnprstv"
    = [(prefix,suffix),(prefix++[last prefix],suffix)]
  | otherwise = [(prefix,suffix)] 

--compound :: Lex -> String -> [[(String,String)]]
compound saldoLex w  = 
  [ [p,s] | (p,s) <- prefixes_suffixes w, not (exception s)
                  , isInLex saldoLex p
                  , isInLex saldoLex s]
  ++
  [ map (\(x:xs) -> map (second tail ys) . (sandhi p x)) ys  | (p,s) <- prefixes_suffixes w, not (exception s)
                 , let xs = compound saldoLex s]

f :: String -> (String,[(String,String)]) -> (String,[(String,String)])
f a (a',bs) = (a',sandhi a a'++bs)

g :: [a] -> [a]
g (a:b:xs) = sandhi a b ++ g (b:xs)
g xs       = xs

isInLex = undefined
