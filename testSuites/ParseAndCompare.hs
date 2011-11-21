import Parsing
import Data.Function
import Data.Maybe
import Control.Monad
import Control.Arrow
import System.Environment (getArgs)
import System.Process
import Prelude hiding (compare)

-- To create a new standard
create = do
  putStrLn "will make a new standard of treebankTest in newTreebank"
  run "treebankTest.xml" False "newTreebank.txt"
  putStrLn "Done"

main = do
  arg <- getArgs
  case take 1 arg of
       ["C"] -> create
       _   -> test arg

test args = do
  let file = fromMaybe "treebankTest.xml" (listToMaybe args)
  c <- run file False "tmp.tmp"
  correct <- readFile "newTreebank.txt"
  have    <- readFile "tmp.tmp"
  compare correct have
  --rawSystem "rm" ["tmp.tmp"]
  dat <- readProcess "date" [] []
  appendFile "grammarLog.txt" $ dat ++ show c ++"\n\n"



compare = zipWithM_ rate `on` parseResult
  where rate (i,n) (j,m) | i /= j = putStrLn $ color red 
                                       $ "Not the same sentence! "++show i++" "++show j
                         | n == m = putStrLn $ color green $ "Sentence "++show i++" ok."
                         | n == 0 = putStrLn $ color pink $ "Sentence "++show i++"can now be parsed."
                         | n <  m = putStrLn $ color pink 
                                       $ "Sentence "++show i++" can be parsed in "++show m++" ways!"
                                          ++ " (before only "++show n++")"

                         | n >  m = putStrLn $ color blue  $ "Sentence "++show i++" can be parsed in less ways."


parseResult :: String -> [(String,Int)]
parseResult = map (second read . read) . lines


