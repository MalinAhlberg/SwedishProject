import Control.Monad
import Saldoer
import SaldoTools

import System.Environment
import Data.Maybe

  
main :: IO ()
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
    Just skip -> 
        -- Remember magic constant 15. Use getDirectoryContents to fix
        return (["saldoPart"++show n++".xml" | n <- [skip..15]],skip)
  initGFFiles "Tot"
  zipWithM_ (extract skipList "Tot") fpaths [skip..]
  putStrLn "extraction complete.. Completing files ..."
--  combine files
  endGFFiles "Tot"
  putStrLn "Lexicon salodTot created!"
--  combine files
  return ()

