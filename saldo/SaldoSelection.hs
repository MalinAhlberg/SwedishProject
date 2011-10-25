module Main where
import System.Environment
import Control.Monad

import SaldoTools
import Saldoer

main = do
  args <- getArgs
  skips <- case args of
                (skfile:_) -> do
                         f <- readFile skfile
                         return $ Just $ lines f
                []         -> return Nothing
  initGFFiles "Sel"
  --ns <- noOfParts
  let ns = [14..15]
  putStrLn $ "Will extract from files "++ unwords (map show ns)
  let parts = ["saldoPart"++show n++".xml" | n <- ns]
  zipWithM (extract skips "Sel") parts ns
  putStrLn "extraction complete.. Completing files ..."
  endGFFiles "Sel"
  putStrLn "Lexicon salodSel created!"
  return ()



