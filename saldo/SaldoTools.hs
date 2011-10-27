module SaldoTools where
import Data.List
import Data.Char
import System.FilePath.Posix
import System.Directory

import Saldoer

skipList :: Maybe [String]
skipList = Nothing

splits []  = []
splits xs = let (part,rest) = splitAt 200000 xs
                (end,rest') = span ((/=) "<LexicalEntry>" . dropWhites) rest
            in (part++end):splits rest'       

writeFiles :: [[String]] -> Int -> IO [FilePath]
writeFiles [] _ = return []
writeFiles (x:xmls) n = do
     putStrLn ("Writing part "++show n)
     let name = ("saldoPart"++show n++".xml")
     writeFile name cont
     names <- writeFiles xmls (n+1)
     return $ name:names
  where cont =  mk $  unlines  x
        mk f  | "<?xml version" `isPrefixOf` f = f  ++ end
              | null xmls                      = head ++ f
              | otherwise                      = head ++ f ++ end
        head = "<Lexicon>"
        end  = "</Lexicon>"


initGFFiles tot = do
  writeFile ("saldo"++tot++".gf")   $ absHeader "Tot" ""
  writeFile ("saldo"++tot++"Cnc.gf") $ concHeader "Tot" ""

endGFFiles tot = do
  appendFile ("saldo"++tot++".gf") "}"
  appendFile ("saldo"++tot++"Cnc.gf") "}"
  

dropWhites = takeWhile (not . isSpace) . dropWhile isSpace

noOfParts :: IO [Int]
noOfParts = do
  fs <- getDirectoryContents "."
  return $ sort [ read $ dropWhile (not . isNumber) p | p <- map dropExtensions fs
                   ,"saldoPart" `isPrefixOf` p ]
