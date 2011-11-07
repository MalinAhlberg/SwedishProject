import System.Environment    (getArgs)
import Control.Applicative
import Data.Maybe
import Data.ByteString.Char8 (pack)

import SaldoXML

main = do
  (app:input:keepFile:_) <- getArgs
  putStrLn "Parse dictionary ..."
  mdict <- parseDictList input
  let dict = case mdict of
                  Just d -> d
                  Nothing -> error "Could not parse input"
  putStrLn "Dictionary parsed, reading word file ..."
  keeps <- lines <$> readFile keepFile
                          -- do not keep compounding forms
  let vs  = [[form | (tag,form) <- table tab, last form /= '-', tag /= pack "c"] 
                   | (e,tab) <- dict, e `elem` keeps] 
  let out = unlines (map unwords vs)
  putStrLn "Writing file VerbList.txt ..."
  (if app=="A" then appendFile else writeFile) "VerbList.txt" out

