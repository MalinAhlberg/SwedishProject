import System.Environment    (getArgs)
import Control.Applicative
import Data.Maybe

import SaldoXML

main = do
  (input:keepFile:_) <- getArgs
  mdict <- parseDictList input
  let dict = case mdict of
                  Just d -> dict
                  Nothing -> error "Could not parse input"
  keeps <- lines <$> readFile keepFile
  let vs  = [map snd tab | (e,tab) <- dict, e `elem` [""]] 
  let out = unlines (map unwords vs)
  writeFile "VerbList.txt" out

