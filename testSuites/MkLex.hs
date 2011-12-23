module MkLex where
import PGF
import Control.Monad
import System.Process


mkDir :: [String] -> Language -> FilePath -> IO (FilePath,Language)
mkDir ws lang bigPGF = do
   pgf <- readPGF bigPGF  
   let morpho = buildMorpho pgf lang
       allLemmas = map fst $ concatMap (lookupMorpho morpho) ws
   (cnc,abs) <- extractLemmas allLemmas
   rawSystem "gf --make" [cnc] -- must wait for process!
   return (newPGF,newLang)

extractLemmas :: [Lemma] -> IO (FilePath,FilePath)
extractLemmas ws = do
  writeHeaders
  ex bigLexCnc newLexCnc
  ex bigLexAbs newLexAbs
  writeEnd
  return (newLexCnc,newLexAbs)

 where ex file newFile = do
             lex <- liftM ((map words) . lines) $ readFile file
             mapM_ (extract newFile) lex
       extract newFile l@(x:xs)
               | x `specelem` ws = appendFile newFile (unwords l++"\n")
       extract _ _ = return ()

       writeHeaders  = writeGF writeFile [headerCnc,headerAbs]
       writeEnd      = writeGF appendFile (repeat "}\n")

       writeGF f = zipWithM_ f [newLexCnc,newLexAbs]

       headerCnc = "--# -path=.:abstract:alltenses:swedish:common:scandinavian\n"
                   ++"concrete TestLex of TestLexAbs = CatSwe **\n"
                   ++"open  Prelude, CommonScand, ParadigmsSwe, IrregSwe in {\n"
                   ++"lin\n"
       headerAbs = "--# -path=.:abstract:prelude:alltenses\n"
                   ++"abstract TestLexAbs = Cat ** {\n"
                   --smarter algorithm! have lexicon is lexical order

bigLexCnc = "DictSwe.gf"
bigLexAbs = "DictSweAbs.gf"
newLexCnc = "TestLex.gf"
newLexAbs = "TestLexAbs.gf"
newPGF    = "TestLex.pgf"
newLang   = read "TestLex"
       
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
