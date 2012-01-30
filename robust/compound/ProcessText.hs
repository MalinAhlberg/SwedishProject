module ProcessText where
import PGF
import Data.List
import Data.Char
import Data.Ord
import ParseSaldo
import Control.Arrow
import Compound
import Ne

-- compound: kattpälsen  -> [("",katt_nn),("pälsen",päls_nn)]
-- ne      : Johan Ros   -> [("X1",Johan),("",Ros)]
simplify :: Lex -> Morpho -> [(String,Tag,Tag)] -> [(String,NameTag,Tag)]
simplify lex morpho (a:str) 
         | null (lookupMorpho morpho w) && newAnalyse
            = sms ++ analyse lex morpho str
         | otherwise = isName morpho (a : compoundStr lex morpho str)

   where w   = (\(a,b,c) -> lower a) a
         a'  = (\(a,b,c) -> (lower a,b,c)) a
         sms = compoundUnknown lex morpho a'
         newAnalyse = length sms > 1

simplify lex morpho [] = []

analyse lex morpho str = 
          let ss = compoundStr lex morpho str
          in  isName morpho ss

compoundStr lex morpho = concatMap (compoundUnknown lex morpho)

compoundUnknown lex morpho (w,t,tb)
         | hasCapital w                       = [(w,t,tb)]
         | not (null (lookupMorpho morpho w)) = [(w,t,tb)]
         | not (null comps)                   = intersperse bind (tagCmp $ bestCmp comps)
         | otherwise                          = [(w,t,tb)]
             where comps  = compound lex w
                   tagCmp :: [(String,Tag)] -> [(String,Tag,Tag)]
                   tagCmp xs = map addEmptyTag (init xs)
                                   ++ lastWordCmp (last xs)
                   addEmptyTag (a,b) = ("",b,tb)
                   bind   = ("","&+","-")
                   lastWordCmp (a,b) = [(a,b,tb)]
                   bestCmp = head . sortBy (comparing length) -- färst delar!
                   hasCapital (x:xs) = isUpper x
                   hasCapital _      = True

trysimpl str = do
  putStrLn "parse pgf"
  pgf  <- readPGF "../../gf/BigTest.pgf"
  putStrLn "building morpho"
  let morpho = buildMorpho pgf (read "BigTestSwe")
  putStrLn "getting saldo"
  lex <- getSaldo
  return $ simplify lex morpho (map (\x -> (x,"","")) $ words str)
 
