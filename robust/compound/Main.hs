import XMLHelp hiding (words,id,Tag)
import Ne
import PGF
import ProcessText
import ParseSaldo

import Debug.Trace
import Data.List
import Data.Char
import Data.Ord
import Data.Function
import Control.Arrow
import Control.Applicative

{- gets 98.8% of the PNs
   + there were 1035 not PNs (but may be correct anyway)
   Without morpho-check (toLower all), get 91.6%
   + there were 673 not PNs  
   -}

  {- TODO
     Change tags to show gender/case when parsing is done
   -}
tb = "../../Talbanken05_20060604/FPS/P.tiger.xml"

main = evalNE False "../ne/100.xml" "evalAll1c.txt"
--main = evalNE True "1.xml" "evalAll1c.txt"

test str = do
  pgf  <- readPGF "../../gf/BigTest.pgf"
  let morpho = buildMorpho pgf (read "BigTestSwe")
  return $ isName morpho (map (\x -> (x,"","")) (words str))

testxml = do
  sent <- mainF "en.xml" (return . map getSentenceTagged)
  print sent

--neEval = evalNE $ curry (sortResult . eval . uncurry compareT)

--evalNE :: ([[String]] ->  [(Id,[(Tag,String)])] -> String) -> FilePath -> FilePath -> IO ()
evalNE :: Bool -> FilePath -> FilePath -> IO ()
evalNE small inp out = do 
  putStrLn "parse pgf"
  pgf  <- readPGF smallpgf
  putStrLn "building morpho"
  let morpho = buildMorpho pgf smallLang
  putStrLn "getting saldo"
  lex <- getSaldo
  putStrLn "extracting sentences"
  sent <- concat <$> mainF inp (return . map getSentenceTagged)
  putStrLn "finding names"
  let output = concatMap ({-onlyTags .-} simplify lex morpho . emptyTagged . str) sent 
  writeFile "tmpres"  $ unlines $ map show output
  trace (show (output,sent)) $ return ()
  writeFile out $  sortResult $ eval {-$ compareT-} output -- sent
 where --onlyTags  = map (\(a,b,c) -> (a,b,)  --- remove ne-tags
       --onlyWords = map snd  --- remove tb-tags
       str       = snd      --- remove tb-index
       emptyTagged = map (\(t,w) -> (w,"",t))
       smallpgf = if small  then "../../gf/BigTest.pgf"
                            else "../../gf/Big.pgf"
       smallLang = if small then read "BigTestSwe"
                            else read "BigSwe"

{-
compareT :: [[(String,Tag)]] -> [(Id,[(Tag,String)])] -> [(Tag,Tag,String)]
compareT ntags tbtags = concat $ zipWith (\x (i,y) -> 
                               zipWith (\a (b,c) -> (a,b,c)) x y) ntags tbtags
                               -}

-- The empty strings from dropName will disappear here (in a nice way) 
-- since there not tagged as X1, neither as PN. (If they were, it would be
-- ok anyway.
{-
eval :: [(Tag,Tag,String)] -> ([(Tag,String)],[String])
eval ((nt,tb,str):xs) = let (tagged,untagged) = eval xs
                        in  trace (show (nt,tb,str)) $ if hasNameTag nt 
                            then ((tb,str):tagged,untagged)
                            else if tb=="PN" then (tagged,str:untagged)
                                 else (tagged,untagged)
                                 -}

eval :: [(String,NameTag,Tag)] -> ([(Tag,String)],[String])
eval ((str,nt,tb):xs) = let (tagged,untagged) = eval xs
                        in  trace (show (nt,tb,str)) $ if hasNameTag str 
                            then ((tb,nt):tagged,untagged)
                            else if tb=="PN" then (tagged,str:untagged)
                                 else (tagged,untagged)
eval [] = ([],[])

hasNameTag (n:t:[])    = n=='x'&& t=='x' || ((n=='X' || n=='Y') && isDigit t)
hasNameTag (n:t:t1:[]) = (n=='X' || n=='Y') && isDigit t && isDigit t1
hasNameTag _           = False


sortResult :: ([(Tag,String)],[String]) -> String
sortResult (ns,pns) = 
   let as = gather (take 2) (map fst ns)
       bs = gather id pns
       str1 = "Tagged as names:\n"++unlines (map format as)
       str2 = "Untagged PNs ("++show (length pns)++"):\n" 
               ++ unlines (map show bs)
   in str1++"\n\n"++str2++"\nToo tagged:\n++"
          ++unwords [s++" "++t | (t,s) <-ns, not ("PN" `isPrefixOf` t)]
  where gather f = reverse . sortBy (comparing snd) 
                 . map (head' &&& length) . groupBy ((==) `on` f) 
                 . sort
        format (t,n) = show n ++ " tagged as "++t
        head' []     = ""
        head' (x:xs) = x
