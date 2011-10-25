
import Data.List
import Control.Monad
import Saldoer
import SaldoXML
import Data.Char

import System.Environment
import Data.Maybe
import Control.Applicative

{-
main :: IO ()
main = extract "saldoPart1.xml" 60
  -}
main = do 
  arg <- fmap (fmap read . listToMaybe) getArgs 
  (fpaths,skip) <- case arg of
    Nothing -> do 
	saldom <- readFile  "saldom.xml"
	let parts = splits $ lines saldom
	putStrLn "read saldo. Writing partitions"
	fpaths <- writeFiles parts 0
	putStrLn "written all files. Extracting ..."
        return (fpaths,0)
    Just skip -> do
        -- Remember magic constant 15. Use getDirectoryContents to fix
        return (["saldoPart"++show n++".xml" | n <- [skip..15]],skip)
  initGFFiles 
  zipWithM extract fpaths [skip..]
  putStrLn "extraction complete.. Completing files ..."
--  combine files
  endGFFiles
  putStrLn "Lexicon salodTot created!"
--  combine files
  return ()

combine :: [[GrammarInfo]] -> IO ()
combine xs = do
   let tot = concat xs
   printGF' tot "Total"

splits :: [String] -> [[String]]
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
          -- does not work for last file, make better with interval maybe
        mk f  | "<?xml version" `isPrefixOf` f = f  ++ end
              | null xmls                      = head ++ f
              | otherwise                      = head ++ f ++ end
        head = "<Lexicon>"
        end  = "</Lexicon>"


initGFFiles = do
  writeFile "saldoTot.gf" $ absHeader "Tot"
  writeFile "saldoTotCnc.gf"    $ concHeader "Tot"

endGFFiles = do
  appendFile "saldoTot.gf" "}"
  appendFile "saldoTotCnc.gf" "}"
  

dropWhites = takeWhile (not . isSpace) . dropWhile isSpace
