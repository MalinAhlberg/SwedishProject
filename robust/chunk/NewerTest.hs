{-# LANGUAGE TupleSections #-}
module Main where
import NewerChunk
import Types
import qualified Format as Form
import PGF
import Data.Tree
import Control.Monad
import System.IO

main = do hSetBuffering stdout LineBuffering
          try "EvalSuite1.xml"

try fil = do
  pgf <- readPGF pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  --let inp = map snd input 
  --putStrLn $ "input: "++show inp
  sequence [parseText inp pgf lang startType >>= writeToFile i | (i,inp) <- input]
  --writeFile "testetE1.txt" $ unlines $ map showRes res
 where showRes (i,expr) = i++"\n"++ unlines (map (showExpr []) expr)
       writeToFile i x = appendFile "testetE1.txt" $ showRes (i,x) ++"\n"

startType = text 
lang = read "BigTestSwe"
pgfFile = "../../gf/BigTest.pgf"
