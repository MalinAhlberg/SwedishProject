import Data.List
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Text.Printf
import PGF

use = do
  f <- (words . removeParent) <$> readFile "AllTBNewNaked"
  let mapp   =  filter isBanned . map (head &&& length) . group . sort 
      count  =  mapp f
      all    :: Double
      all    =  toEnum $ sum $ map snd count
      vals   =  map (\(k,v) -> (k,toEnum v / all)) count
      format =  \(k,v) -> k++" "++ printf "%f" v
  writeFile "ProbsNew" $ unlines $ map format vals

isBanned = (`notElem` ["MassNP","?","john_PN"]) . fst -- "s11" "johan_PN" etc
removeParent = filter (`notElem` "()") 

test str = do
  pgf   <- readPGF "../gf/BigTest.pgf"
  probs <- readProbabilitiesFromFile "Probs" pgf
  --putStrLn $ showProbabilties probs
  let pgf' = setProbabilities probs pgf 
      lang = read "BigTestSwe"
      ex1  = parse pgf  lang (fromJust $ readType "Utt") str
      ex2  = parse pgf' lang (fromJust $ readType "Utt") str
  putStrLn "old:\n"
  putStrLn $ unlines $ map (showExpr []) ex1
  putStrLn "\nnew:\n"
  putStrLn $ unlines $ map (showExpr []) ex2


