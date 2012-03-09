{-# LANGUAGE TupleSections #-}
module Main where
import NewerChunk
import ProcessTree
import ParseSaldo
import MkLex
import Types
import qualified Format as Form
import PGF
import Data.List
import Data.Tree
import Data.Tree.Zipper
import Control.Monad
import Control.Monad.Writer
import System.IO
import System.TimeIt

main = do hSetBuffering stdout LineBuffering
          timeIt $ tryAll "hardtests.xml"

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
      (newsTrees,res) = runWriter (mapM (processTree True lex morpho 0) ziptrees)
      (lms,names)     = res
  writeFile "namesEx" $ formatNames names
  lex <- mkLexMap
  Just (pgf',newLang) <- mkDir lex lms 
  newPgf <- readPGF pgf' 
  probs <- readProbabilitiesFromFile "ProbsNewSpec" newPgf
  let goodPgf = setProbabilities probs newPgf
  putStrLn $ "Parsing the tree "
  mapM (processAndWrite goodPgf newLang) $ zip (map fst input) (map toTree newsTrees)
 where processAndWrite pgf lang (i,tree) = do
         res <- parseText tree pgf lang startType
         appendFile "disambigtest3.txt" $ showRes (i,res) ++"\n"
        where ziptree = getFirstWord $ fromTree tree


tryMedium fil = do
  putStrLn $ "Start, reading saldo ..."
  lex <- getSaldo 
  putStrLn $ "Reading pgf ... "
  pgf <- readPGF "BigParse.pgf"
  let plang = read "BigParseSwe"
      morpho = buildMorpho pgf plang
  putStrLn $ "Parsing xml ... "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "Processing the tree "
  let ziptrees        = map (getFirstWord . fromTree . snd) input
      (newsTrees,res) = runWriter (mapM (processTree True lex morpho 0) ziptrees)
      (lms,names)     = res
  writeFile "namesEx" $ formatNames names
  putStrLn $ "Lemmas requested: "++show (map head $ group $ sort lms)
  putStrLn $ "Names exchanged "++show   (map head $ group $ sort names)
  putStrLn $ "Parsing the trees "
  mapM (processAndWrite pgf plang) $ zip (map fst input) (map toTree newsTrees)
 where processAndWrite pgf lang (i,tree) = do
         res <- parseText tree pgf lang startType
         appendFile "reservtest.txt" $ showRes (i,res) ++"\n"
        where ziptree = getFirstWord $ fromTree tree


try fil = do
  pgf <- readPGF pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  sequence [parseText inp pgf lang startType >>= writeToFile i | (i,inp) <- input]
 where writeToFile i x = appendFile "testNew.txt" $ showRes (i,x) ++"\n"

showRes (i,expr) = i++"\n"++ unlines (map (showExpr []) expr)
formatNames = let format (i,name) = show i ++ "\t"++name
              in  unlines . map format
       
startType = text 
pgfFile = "BigParse.pgf"
pgfBigFile = "ExtractPGF.pgf" --BigParse with all lexicon
langBig, lang :: Language
langBig = read "BigParseSwe"
lang = read "BigParseSwe"

