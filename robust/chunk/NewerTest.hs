{-# LANGUAGE TupleSections #-}
module Main where
import NewestChunk
import ProcessTree
import ParseSaldo
import MkLex
import Types
import qualified Format as Form
import PGF hiding (Tree)
import qualified Data.HashMap as HM
import Data.List
import qualified Data.Text.Internal as Text
import Data.Tree
import Data.Tree.Zipper
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import System.IO
import System.TimeIt

main = do hSetBuffering stdout LineBuffering
          timeIt $ tryAll "hardtests.xml" --tryAll "stest.xml"


tryAll fil = do
  putStrLn $ "Start, reading saldo ..."
  saldo <- getSaldo 
  putStrLn $ "Reading pgf ... "
  pgf <- readPGF pgfBigFile
  putStrLn $ "Building morpho ... "
  let morpho = buildMorpho pgf langBig
  putStrLn $ "Parsing xml ... "
  input <- splitFile . concat <$> Form.parse fil
  pgfs <- mapM (extractDicts saldo morpho) [x | x <- zip input [0..]]  
  mapM processPartition pgfs

 where processAndWrite :: PGF -> Language -> (String,Tree String) -> IO ()
       processAndWrite pgf lang (i,tree) = do
          res <- parseText tree pgf lang startType
          appendFile "nicetest.txt" $ showRes (i,res) ++"\n"
        where ziptree = getFirstWord $ fromTree tree

       extractDicts :: Saldo -> Morpho -> ([(String,Tree String)],Int) -> IO ([String],[TreePos Full String],Maybe (FilePath,Language))
       extractDicts saldo morpho (input,i) = do
          putStrLn $ "Processing the tree "
          let ziptrees        = map (getFirstWord . fromTree . snd) input
              (newsTrees,res) = runWriter (mapM (processTree True saldo morpho 0) ziptrees)
              (lms,names)     = res
          writeFile ("namesEx"++show i) $ formatNames names
          lex <- mkLexMap 
          pgfs <- mkDir lex lms i 
          return (map fst input, newsTrees,pgfs)

       processPartition :: ([String],[TreePos Full String],Maybe (FilePath,Language)) -> IO ()
       processPartition (ids,newsTrees,Just (pgf,newLang)) = do
          putStrLn $ "using pgf file "++pgf
          newPgf <- readPGF pgf 
          probs  <- readProbabilitiesFromFile "ProbsNewSpec" newPgf
          let goodPgf = setProbabilities probs newPgf
          putStrLn $ "Parsing the tree "
          mapM_ (processAndWrite goodPgf newLang) $ zip ids (map toTree newsTrees)

       splitFile :: [(String,Tree String)] -> [[(String,Tree String)]]
       splitFile = unfoldr (\xs -> if null xs then Nothing else Just (take 10 xs,drop 10 xs))


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
  pgf <- readPGF "pgfs/BigParse0.pgf" --pgfFile
  putStrLn $ "created pgf etc "
  input <- fmap concat $ Form.parse fil
  putStrLn $ "reading input "
  sequence [parseText inp pgf lang startType >>= writeToFile i | (i,inp) <- input]
 where writeToFile i x = appendFile "smalltests" $ showRes (i,x) ++"\n"

showRes (i,expr) = i++"\n"++ unlines (map (showExpr []) expr)
formatNames = let format (i,name) = show i ++ "\t"++name
              in  unlines . map format


type Saldo = HM.Map Text.Text [(Text.Text, Text.Text)]

       
startType = phrText 
pgfFile = "BigParse.pgf"
pgfBigFile = "ExtractPGF.pgf" --BigParse with all lexicon
langBig, lang :: Language
langBig = read "BigParseSwe"
lang = read "BigParseSwe"

