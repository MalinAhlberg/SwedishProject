{-# LANGUAGE TupleSections #-}
module Main where
import NewerChunk
import ProcessTree
import ParseSaldo
import MkLex
import Types
import qualified Format as Form
import PGF
import Data.Tree
import Data.Tree.Zipper
import Control.Monad
import Control.Monad.Writer
import System.IO

main = do hSetBuffering stdout LineBuffering
          try "EvalSuite1.xml"

tryAll fil = do
  putStrLn $ "Start, reading saldo ..."
  lex <- getSaldo 
  putStrLn $ "Reading pgf ... "
  pgf <- readPGF pgfBigFile
  putStrLn $ "Building morpho ... "
  let morpho = buildMorpho pgf langBig
  putStrLn $ "Parsing xml ... "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "Processing the tree "
  let ziptrees        = map (getFirstWord . fromTree . snd) input
      (newsTrees,lms) = runWriter (mapM (processTree True lex morpho 0) ziptrees)
  lex <- mkLexMap
  Just (pgf',newLang) <- mkDir lex lms 
  newPgf <- readPGF pgf' 
  putStrLn $ "Parsing the tree "
  mapM (processAndWrite newPgf newLang) $ zip (map fst input) (map toTree ziptrees)
 where processAndWrite pgf lang (i,tree) = do
         res <- parseText tree pgf lang startType
         appendFile "supertest.txt" $ showRes (i,res) ++"\n"
        where ziptree = getFirstWord $ fromTree tree




try fil = do
  pgf <- readPGF pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  --let inp = map snd input 
  --putStrLn $ "input: "++show inp
  sequence [parseText inp pgf lang startType >>= writeToFile i | (i,inp) <- input]
  --writeFile "testetE1.txt" $ unlines $ map showRes res
 where writeToFile i x = appendFile "testNew.txt" $ showRes (i,x) ++"\n"

showRes (i,expr) = i++"\n"++ unlines (map (showExpr []) expr)
       
startType = text 
pgfFile = "../../gf/BigParse.pgf"
pgfBigFile = "../../gf/BigValLexAbs.pgf"
langBig, lang :: Language
langBig = read "BigValLex"
lang = read "BigParseSwe"
