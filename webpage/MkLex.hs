module MkLex where
import PGF
import Control.Monad
import System.Process
import System.Exit
import qualified Data.HashMap as M

type LexMap = (Lex,Lex)
type Lex    = M.Map String String

mkDir :: LexMap -> [String] -> Language -> PGF -> IO (Maybe (FilePath,Language))
mkDir lexs ws lang pgf = do
--   putStrLn "Reading pgf..."
--   pgf <- readPGF bigPGF  
   putStrLn $ "Lemmas wanted "++show ws
   let morpho = buildMorpho pgf (read "DictSwe")
       allLemmas = map fst $ concatMap (lookupMorpho morpho) ws
   putStrLn "Extracting new lexicon..."
   putStrLn $ "Lemmas "++show allLemmas
   (cnc,abs) <- extractLemmas lexs allLemmas
   putStrLn "Compiling new grammar..."
   (ex,res,err) <- readProcessWithExitCode "gf" ["--make",bigGF] []
   case ex of
        ExitSuccess   -> do putStrLn "Done"
                            return $ Just (newPGF,newLang)
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
       writeGF f = zipWithM_ f [newLexCnc,newLexAbs]

       headerCnc = "--# -path=.:abstract:alltenses:swedish:common:scandinavian\n"
                   ++"concrete TestLex of TestLexAbs = CatSwe **\n"
                   ++"open  Prelude, CommonScand, ParadigmsSwe, IrregSwe in {\n"
                   ++" flags optimize=values ; coding=utf8 ;\n"
                   ++"lin\n \n"
                   ++"ta_med_V3 = dirV3 (reflV (partV"
                   ++"(mkV \"ta\" \"tar\" \"ta\" \"tog\" \"tagit\" \"tagen\") \"med\"))"
                   ++"(mkPrep \"till\");\n"
                   ++"akta_sig_V2 = mkV2 (reflV (mkV \"aktar\")) (mkPrep \"för\") ;\n"
                   ++"faa_VV = mkV \"få\" \"får\" \"få\" \"fick\" \"fått\" \"fådd\" ** "
                   ++"{c2 = mkComplement [] ; lock_VV = <>} ;\n"
 
       headerAbs = "--# -path=.:abstract:prelude:alltenses\n"
                   ++"abstract TestLexAbs = Cat ** {\n fun \n"
                   ++ "ta_med_V3 : V3 ;\n"
                   ++ "akta_sig_V2 : V2 ;\n"
                   ++"faa_VV : VV ;\n "


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
