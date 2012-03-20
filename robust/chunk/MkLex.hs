{-# LANGUAGE PatternGuards #-}
module MkLex where
import PGF
import Data.List
import Control.Monad
import Control.DeepSeq
import System.Process
import System.TimeIt
import System.Exit
import qualified Data.TrieMap as M
import ReadCommand

type LexMap = (Lex,Lex)
type Lex    = M.TMap String String

mkDir ::LexMap -> [Lemma] -> Int -> IO (Maybe (FilePath,Language))
--mkDir lexs lem i | i <193 = return $ Just ("pgfs/"++"BigParse"++show i++".pgf",newLang)
mkDir lexs lem i = do
   let lems = map head $ group $ sort lem
   putStrLn $ "Lemmas wanted "++show lems
   putStrLn "Extracting new lexicon..."
   (t,(cnc,abs)) <- timeItT $ extractLemmas lexs lems 
   putStrLn $ "Compiling new grammar, extraction: "++ show t
   (t',(ex,res,err)) <- timeItT $ readProcessWithExitCode' "." "gf"
                              ["--make",parseGF] []
   putStrLn $ "compilation: "++ show t'
   case ex of
        ExitSuccess   -> do putStrLn "Done"
                            let thisPGF = "pgfs2/"++"BigParse"++show i++".pgf"
                            x <- readProcess "mv" ["BigParse.pgf",thisPGF] []
                            putStrLn $ "Moved the pgf "++x
                            return   $ Just (thisPGF,newLang)
        ExitFailure _ -> do putStrLn "PGF error"
                            putStrLn err
                            return Nothing

extractLemmas :: LexMap -> [Lemma] -> IO (FilePath,FilePath)
extractLemmas (cnc,abs) ws = do
  writeHeaders
  mapM_ (extract newLexCnc cnc) ws
  mapM_ (extract newLexAbs abs) ws
  writeEnd 
  return (newLexCnc,newLexAbs)
  where
       extract newFile lex w
               | Just a <- M.lookup (showCId w) lex =
                         liftM Just $ appendFile newFile (a++"\n")
       extract _ _ _ = return Nothing
       writeHeaders  = writeGF writeFile [headerCnc,headerAbs]
       writeEnd      = writeGF appendFile (repeat "}\n")
       writeGF     f = zipWithM_ f [newLexCnc,newLexAbs]

       headerCnc = "--# -path=.:abstract:alltenses:swedish:common:scandinavian\n"
                   ++"concrete TestLex of TestLexAbs = CatSwe **\n"             
                   ++"open  Prelude, CommonScand, ParadigmsSwe, IrregSwe in {\n"
                   ++" flags optimize=values ; coding=utf8 ;\n"
                   ++"lin\n \n"
                   ++"frysa_V = mkV \"frysa\" \"fryser\" \"frys\" \"frös\" \"frusit\" \"frusen\" ;\n"
 
       headerAbs = "--# -path=.:abstract:prelude:alltenses\n"
                   ++"abstract TestLexAbs = Cat ** {\n fun \n"
                   ++"frysa_V : V ;\n "


mkLexMap :: IO LexMap 
mkLexMap = do 
  cnc <- ex bigLexCnc
  abs <- ex bigLexAbs
  return (cnc,abs)
 where ex file = liftM mkMap $ readFile file
       mkMap :: String -> Lex
       mkMap lex = M.fromList [force (head ws, l) | l <- lines lex
                                                 , let ws = words l
                                                 , not $ null ws]
       force x = x `deepseq` x

parseGF   = "BigParseSwe.gf"
bigLexCnc = "../../gf/BigValLex.gf"
bigLexAbs = "../../gf/BigValLexAbs.gf"
newLexCnc = "TestLex.gf"
newLexAbs = "TestLexAbs.gf"
newPGF    = "BigParse.pgf"
newLang   = read "BigParseSwe"
       
specelem :: String -> [Lemma] -> Bool
specelem w = elem w . map showCId
