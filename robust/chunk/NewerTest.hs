import NewerChunk
import Types
import qualified Format as Form
import PGF
import Data.Tree


try fil = do
  pgf <- readPGF pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  let inp = map snd input 
  --putStrLn $ "input: "++show inp
  res   <- mapM (\inp -> parseText inp pgf lang startType) inp
  writeFile "testet.txt" $ unlines $ map (showExpr []) res

startType = text 
lang = read "BigTestSwe"
pgfFile = "../../gf/BigTest.pgf"
