{-# LANGUAGE PatternGuards #-}
module MkLex where
import PGF
import Control.Monad
import Control.DeepSeq
import System.Process
import System.TimeIt
import System.Exit
import qualified Data.TrieMap as M
import ReadCommand
import Log as L

type LexMap = (Lex,Lex)
type Lex    = M.TMap String String
type UserId = String

mkDir :: UserId -> LexMap -> [String] -> Language -> PGF -> IO (Maybe (FilePath,Language))
mkDir id lexs ws lang pgf = do
   L.log $ "mkDir for user "++show id
   logA id $ "Lemmas wanted "++show ws
   let morpho = buildMorpho pgf (read "DictSwe")
       allLemmas = map fst $ concatMap (lookupMorpho morpho) ws
   logA id "Extracting new lexicon..."
   logA id $ "Lemmas "++show allLemmas
   logA id "Extracing begins..."
   (t,(cnc,abs)) <- timeItT $ extractLemmas id lexs allLemmas
   logA id $ "Compiling new grammar, extraction: "++ show t
   (t',(ex,res,err)) <- timeItT $ readProcessWithExitCode' id "gf"
                              ["--new-comp","-i",inDir ".." id,"--make",inDir ".." bigGF] []
   logA id $ "compilation: "++ show t'
   case ex of
        ExitSuccess   -> do putStrLn "Done"
                            return $ Just (inDir id newPGF,newLang)
        ExitFailure _ -> do putStrLn "PGF error"
                            putStrLn err
                            return Nothing

extractLemmas :: UserId -> LexMap -> [Lemma] -> IO (FilePath,FilePath)
extractLemmas id (cnc,abs) ws = do
  writeHeaders id
  mapM_ (extract id newLexCnc cnc) ws
  mapM_ (extract id newLexAbs abs) ws
  writeEnd id
  return (newLexCnc,newLexAbs)
  where
       extract id newFile lex w
               | Just a <- M.lookup (showCId w) lex =
                         liftM Just $ appendFile (inDir id newFile) (a++"\n")
       extract _ _ _ _ = return Nothing
       writeHeaders id = writeGF id writeFile [headerCnc,headerAbs]
       writeEnd  id    = writeGF id appendFile (repeat "}\n")
       writeGF id f = zipWithM_ f [inDir id newLexCnc,inDir id newLexAbs]

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

inDir id path = id++"/"++path

mkLexMap :: IO LexMap 
mkLexMap = do 
  cnc <- ex bigLexCnc newLexCnc
  abs <- ex bigLexAbs newLexAbs
  return (cnc,abs)
 where ex file newFile = liftM mkMap $ readFile file
       mkMap :: String -> Lex
       mkMap lex = M.fromList [force (head ws, l) | l <- lines lex
                                                 , let ws = words l
                                                 , not $ null ws]
       force x = x `deepseq` x

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


