module MkLex where
import PGF
import Control.Monad
import System.Process
import qualified Data.Map as M

type LexMap = (Lex,Lex)
type Lex    = M.Map String String

mkDir :: LexMap -> [String] -> Language -> PGF -> IO (FilePath,Language)
mkDir lexs ws lang pgf = do
--   putStrLn "Reading pgf..."
--   pgf <- readPGF bigPGF  
   let morpho = buildMorpho pgf lang
       allLemmas = map fst $ concatMap (lookupMorpho morpho) ws
   putStrLn "Extracting new lexicon..."
   (cnc,abs) <- extractLemmas lexs allLemmas
   putStrLn "Compiling new grammar..."
   readProcess "gf" ["--make",bigGF] []
   putStrLn "Done"
   return (newPGF,newLang)

extractLemmas :: LexMap -> [Lemma] -> IO (FilePath,FilePath)
extractLemmas (cnc,abs) ws = do
  writeHeaders
  mapM_ (extract newLexCnc cnc) ws
  mapM_ (extract newLexAbs abs) ws
  writeEnd
  return (newLexCnc,newLexAbs)
  where
 --             mapM_ (extract newFile) lex
       extract newFile lex w
               | Just a <- M.lookup (showCId w) lex = appendFile newFile (a++"\n")
--       extract newFile l@(x:xs)
--               | x `specelem` ws = appendFile newFile (unwords l++"\n")
       extract _ _ _ = return ()
       writeHeaders  = writeGF writeFile [headerCnc,headerAbs]
       writeEnd      = writeGF appendFile (repeat "}\n")
       writeGF f = zipWithM_ f [newLexCnc,newLexAbs]

       headerCnc = "--# -path=.:abstract:alltenses:swedish:common:scandinavian\n"
                   ++"concrete TestLex of TestLexAbs = CatSwe **\n"
                   ++"open  Prelude, CommonScand, ParadigmsSwe, IrregSwe in {\n"
                   ++" flags optimize=values ; coding=utf8 ;"
                   ++"lin\n"
       headerAbs = "--# -path=.:abstract:prelude:alltenses\n"
                   ++"abstract TestLexAbs = Cat ** {\n fun \n"
                   --smarter algorithm! have lexicon is lexical order

mkLexMap :: IO LexMap 
mkLexMap = do 
  cnc <- ex bigLexCnc newLexCnc
  abs <- ex bigLexAbs newLexAbs
  return (cnc,abs)
 where ex file newFile = liftM mkMap $ readFile file
       mkMap :: String -> Lex
       mkMap lex = M.fromList [(head ws, l) | l <- lines lex
                                                 , let ws = words l
                                                 , not $ null ws]

bigGF     = "BigParseSwe.gf"
bigLexCnc = "DictSwe.gf"
bigLexAbs = "DictSweAbs.gf"
newLexCnc = "TestLex.gf"
newLexAbs = "TestLexAbs.gf"
newPGF    = "BigParse.pgf"
newLang   = read "BigParseSwe"
       
specelem :: String -> [Lemma] -> Bool
specelem w = elem w . map showCId

-- use this, sort the lexicon (once), the input (should be fast)
keep :: [[String]] -> [String] -> FilePath -> IO ()
keep (a:as) (b:bs) newFile
      | null a    = keep as (b:bs) newFile
      | head a==b = appendFile newFile (unwords a++"\n")
                    >> keep as bs newFile
      | b>head a  = keep as (b:bs) newFile
      | head a>b  = keep (a:as) bs newFile
keep _   _   _    = return ()

-- implement a fast nubbing to get better time
nubIt = undefined
