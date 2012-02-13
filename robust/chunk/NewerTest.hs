{-# LANGUAGE TupleSections #-}
import NewerChunk
import Types
import qualified Format as Form
import PGF
import Data.Tree
import Control.Monad


try fil = do
  pgf <- readPGF pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  --let inp = map snd input 
  --putStrLn $ "input: "++show inp
  res   <- sequence [liftM (i,) $ parseText inp pgf lang startType | (i,inp) <- input]
  writeFile "testet.txt" $ unlines $ map showRes res
 where showRes (i,expr) = i++"\n"++ unlines (map (showExpr []) expr)

startType = text 
lang = read "BigTestSwe"
pgfFile = "../../gf/BigTest.pgf"
