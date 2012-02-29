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

showThem = do
  pgf   <- readPGF "../robust/chunk/BigParse.pgf"
  probs <- readProbabilitiesFromFile "ProbsNewSpec" pgf
  putStrLn $ showProbabilities probs


--rankTreesByProbs :: PGF -> [Expr] -> [(Expr,Double)]
test str = do
  pgf   <- readPGF "../gf/BigTest.pgf"
  probs <- readProbabilitiesFromFile "ProbsNewSpec" pgf
  let pgf' = setProbabilities probs pgf 
      lang = read "BigTestSwe"
      ex1  = parseAndRank pgf  str lang
      ex2  = parseAndRank pgf' str lang
  putStrLn "old:\n"
  putStrLn $ unlines $ map showTreeProb  ex1
  putStrLn "\nnew:\n"
  putStrLn $ unlines $ map showTreeProb ex2

showTreeProb :: (Expr,Double) -> String
showTreeProb (e,d) = showExpr [] e ++" "++show d

parseAndRank :: PGF -> String -> Language -> [(Expr,Double)]
parseAndRank pgf str lang = rankTreesByProbs pgf  
                          $ parse pgf  lang (fromJust $ readType "Utt") str

