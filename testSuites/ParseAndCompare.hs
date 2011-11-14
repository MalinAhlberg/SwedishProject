import Parsing
import Data.Function
import Control.Monad
import Control.Arrow
import System.Environment (getArgs)
import System.Process
import Prelude hiding (compare)

main = do
  (file:arg) <- getArgs
  run file False "tmp.tmp"
  correct <- readFile "treebank.txt"
  have    <- readFile "tmp.tmp"
  compare correct have
  rawSystem "rm" ["tmp.tmp"]


compare = zipWithM_ rate `on` parseResult
  where rate (i,n) (j,m) | i /= j = putStrLn $ color red 
                                       $ "Not the same sentence! "++show i++" "++show j
                         | n <  m = putStrLn $ color turkos 
                                       $ "Sentence "++show i++" can be parsed in "++show m++" ways!"
                                          ++ " (before only "++show m++")"

                         | n == m = putStrLn $ color green $ "Sentence "++show i++" ok."
                         | n >  m = putStrLn $ color red   $ "Sentence "++show i++" not ok."


parseResult :: String -> [(String,Int)]
parseResult = map (second read . read) . lines
