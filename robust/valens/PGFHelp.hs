module PGFHelp where
import PGF
import System.IO
import Control.Monad

extractLex :: ([String] -> [String]) -> FilePath -> IO [(String,String)]
extractLex f file = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8 
  liftM mkMap $ readFile file
 where mkMap :: String -> [(String,String)]
       mkMap lex = [(head ws, unwords (f ws)) | l <- lines lex
                                 , let ws = words l
                                 , not $ null ws]


