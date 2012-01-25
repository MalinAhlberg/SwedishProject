import XMLHelp hiding (words,id)
import Ne
import PGF

import Data.List
import Data.Function
import Control.Arrow
import Control.Applicative

{- gets 98.8% of the PNs
   + there were 1035 not PNs (but may be correct anyway)
   Without morpho-check (toLower all), get 91.6%
   + there were 673 not PNs  
   -}

maini :: FilePath -> IO ()
maini out = do
  putStrLn "parse pgf"
  pgf  <- readPGF "../../gf/Big.pgf"
  putStrLn "building morpho"
  let morpho = buildMorpho pgf (read "BigSwe")
  putStrLn "extracting sentences"
  sent <- mainF tb (return . map getSentence')
  putStrLn "finding names"
  let output = map (second (isName morpho)) $ concat sent
  writeFile out $ unlines $ map show output

tb = "../../Talbanken05_20060604/FPS/P.tiger.xml"

test xs = do
  putStrLn "parse pgf"
  pgf  <- readPGF "../../gf/Big.pgf"
  putStrLn "building morpho"
  let morpho = buildMorpho pgf (read "BigSwe")
  return $ isName morpho (words xs)

testxml = do
  sent <- mainF "100.xml" (return . map getSentenceTagged)
  print sent

evalNE :: FilePath -> IO ()
evalNE out = do 
  putStrLn "parse pgf"
  pgf  <- readPGF "../../gf/Big.pgf"
  putStrLn "building morpho"
  let morpho = buildMorpho pgf (read "BigSwe")
  putStrLn "extracting sentences"
  sent <- concat <$> mainF tb (return . map getSentenceTagged)
  putStrLn "finding names"
  let output = map ((isName morpho). map snd . snd) sent -- differnte type now, with pos!
  writeFile out $ sortResult $ eval $ compareT output sent

compareT :: [[String]] -> [(Id,[(Tag,String)])] -> [(Tag,Tag,String)]
compareT ntags tbtags = concat $ zipWith (\x (i,y) -> 
                               zipWith (\a (b,c) -> (a,b,c)) x y) ntags tbtags

eval :: [(Tag,Tag,String)] -> ([(Tag,String)],[String])
eval ((nt,tb,str):xs) = let (tagged,untagged) = eval xs
                        in  if nt =="X1" || nt == "X2" then ((tb,str):tagged,untagged)
                            else if tb=="PN" then (tagged,str:untagged)
                                 else (tagged,untagged)
eval [] = ([],[])

sortResult :: ([(Tag,String)],[String]) -> String
sortResult (ns,pns) = 
   let as = gather (take 2) (map fst ns)
       bs = gather id pns
       str1 = "Tagged as names:\n"++unlines (map format as)
       str2 = "Untagged PNs ("++show (length pns)++"):\n" 
               ++ unlines (map show bs)
   in str1++"\n\n"++str2++"\nToo tagged:\n++"
          ++unwords [s++" "++t | (t,s) <-ns, not ("PN" `isPrefixOf` t)]
  where gather f = reverse . sortBy (compare `on` snd) 
                 . map (\x -> (head x,length x)) . groupBy ((==) `on` f) 
                 . sort
        format (t,n) = show n ++ " tagged as "++t
