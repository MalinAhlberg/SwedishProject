import Prelude hiding (lex)
import System.IO
import System.Cmd
import System.Exit
import System.Directory
import Data.Char
import Data.Maybe
import Data.List
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.IORef
import qualified Data.Map as Map
import Text.Read.Lex(Lexeme(..),lex)
import Text.ParserCombinators.ReadP
import Control.Monad.Writer
import Control.Arrow
import Debug.Trace

import qualified PGF as PGF

import SaldoXML
import UTF8

type Convert = WriterT ([String],[String]) IO
type GrammarFile = [(String, String, [[String]], String,Paradigm )]
type Paradigm = [(String,[String],String)]


inputFile = "lillsaldo.xml"

main = do
  hSetBuffering stdout NoBuffering
  putStr $ "Reading "++inputFile++" ... "
  res   <- parseDict inputFile 
  saldo <- case res of
             Just s   -> return s 
             Nothing  -> fail $ "cannot read "++inputFile 
  putStrLn ""

  (cnc_file,err) <- runWriterT $ createGF saldo
  
  (tmp_saldo,err') <- runWriterT compileGF
  err'' <- execWriterT (loop tmp_saldo saldo cnc_file)
  let msg =  "\n Messages:\n "++ unlines (fst err++fst err'++fst err'')
  let ers =  "\n Failing:\n " ++ unlines (snd err++snd err'++snd err'')
  writeFile "Messages.txt" msg
  writeFile "Errors.txt" msg

  where
    loop tmp_saldo saldo cnc_file = do

      (cnc_file',changes) <- updateGF tmp_saldo saldo cnc_file
      tmp_saldo <- compileGF
      if changes == 0
        then return ()
        else loop tmp_saldo saldo cnc_file'

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF saldo = do
  io $ putStr "Creating GF source for SALDO ... "
  io $ print [lemma | (lemma,E pos tab) <- Map.toList saldo, lemma == "vilkatt_N"]
  let cnc_file = [(id,gf_cat,[[snd $ head' "createGF" table]],"",paradigms) 
                  | (id,E pos table) <- Map.toList saldo
                     , (fm_cat,gf_cat,_,paradigms) <- catMap, fm_cat == pos]
  printGF cnc_file
  report "Lexicon created"
  io $ putStrLn ""
  return cnc_file

-------------------------------------------------------------------
-- Compare the current GF version with SALDO
-------------------------------------------------------------------
updateGF tmp_saldo saldo cnc_file = do
  report "will update"
  io $ putStr ("Reading "++tmp_saldo++" ... ")
  pgf <- io $ PGF.readPGF tmp_saldo
  io $ putStrLn ""

  io $ putStr "Updating GF source for SALDO ... "
  count <- io $  newIORef 0
  cnc_file' <- liftM catMaybes $ mapM (check count saldo pgf) cnc_file
  printGF cnc_file'
  io $ putStrLn ""
  c <- io $ readIORef count
  let update = ("updated "++show c++" words")
  io $ putStrLn update
  report update
  return (cnc_file',c)

check count saldo gf entry@(id,cat,lemmas,_,paradigms) = 
  case Map.lookup id saldo of
       Just (E p t) -> checkWord count id gf cat t entry
       Nothing      -> report ("unknown id in SALDO: " ++ id) >> return Nothing

checkWord count id gf cat t entry = do 
  let gf_t     = concat $ PGF.tabularLinearizes gf (read "saldoCnc")
                                               (read (mkGFName id cat))
      paramMap = head' "check" [map | (_,gf_cat,map,_) <- catMap
                                      , gf_cat == cat]
  checkForms count paramMap t gf_t entry

checkForms count paramMap fm_t gf_t entry@(id,cat,lemmas,_,paradigms)
  | null diffs = return $ Just entry
  | otherwise  = do c <- io $ readIORef count
                    io $ writeIORef count $! c+1
                    report "word fail"
                    report (show [(lookup f fm_t,lookup g gf_t) | (f,g) <- paramMap])
                    getNextLemma $!  entry 
  where
    diffs = [(fm_p,fm_v,gf_p,gf_v) | (fm_p,gf_p) <- paramMap
                                   , fm_vs      <- [lookup' fm_p fm_t]
                                   , Just gf_v  <- [lookup gf_p gf_t]
                                   , Just fm_v  <- [isDiff gf_v fm_vs]]
   -- if there is no information about the form in saldo, we chose to accept it
   -- should probably add 'variant {}' or similair
    isDiff  _ [] = Nothing
    isDiff  x xs | x `elem` xs = Nothing
                 | otherwise   = Just $ head xs
 
    getNextLemma x@(id,cat,lemmas,_,[]         ) =
             tellFailing ("No more paradigms to choose from: " ++id)
             >> io (putStrLn "AAAAAAAAAA")
             >> return Nothing
    getNextLemma (id,cat,lemmas,_,(_,xs,a):ps) = do
        report $ "working on "++id
        report $ "next paradigm: " ++ show xs
        report $ "to chose from: "++show ps
        let forms = map getLemma xs
        when (Nothing `elem` forms) $ report $ "all forms for "++id++" not found"
        return $ Just (id,cat,[catMaybes forms],a, ps)
      where
        getLemma gf_p =  case lookup fm_p fm_t of
                          Just ""   -> Nothing
                          Just fm_v -> Just fm_v
                          x         -> Just "variant {}" --trace ("Wooo: variant?"++ show fm_p) $ Nothing 
          where
            fm_p = head' ("getLemma "++gf_p++"."++" Word: "++id) [fm_p | (fm_p,gf_p') <- paramMap, gf_p'==gf_p]
-------------------------------------------------------------------
-- Compile with GF
-------------------------------------------------------------------

compileGF = do
  io $ putStrLn "Compile generate/saldoCnc.gf ... "
  res <- io $ rawSystem "gf" ["--batch", "--make", "generate/saldoCnc.gf", "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return () 
    ExitFailure code -> report ("compiliation failed with error code "++show code)
  (fpath,h) <- io $ openTempFile "." "tmp.pgf"
  io $ hClose h
  io $ copyFile "saldo.pgf" fpath
  report "compilation done"
  return fpath


-------------------------------------------------------------------
-- Generate GF identifier
-------------------------------------------------------------------

mkGFName id' cat = name++"_"++cat
  where
       dash2us '-' = '_'
       dash2us x = x
       num x = if isDigit (head' "isDigit" x) then 'x':x else x
       name =  num $ dropWhile (== '_') $ transform_letters 
                   $ map dash2us $ dropWhile (== '_') $ undot (decodeUTF8 id')
       transform_letters = concat . map trans
       trans '\229' = "aa"
       trans '\197' = "AA"
       trans '\228' = "ae"
       trans '\196' = "AE"
       trans '\224' = "a"
       trans '\225' = "a"
       trans '\232' = "e"
       trans '\233' = "e"
       trans '\234' = "e"
       trans '\231' = "c"
       trans '\252' = "u"
       trans '\246' = "oe"
       trans '\241' = "n"       
       trans '\214' = "OE"
       trans '\183' = "'"
       trans x   = [x]
       undot [] = []
       undot ('.':'.':xs) = '_' : undot xs 
       undot     ('.':xs) = '_' : undot xs
       undot       (x:xs) = x:undot xs


-------------------------------------------------------------------
-- Mapping from SALDO categories/params to GF Resource Library
-------------------------------------------------------------------

catMap = 
  [ (pack "ab", "Adv",map (first pack) advParamMap,  advParadigmList )
  , (pack "av", "A",  map (first pack) adjParamMap,  adjParadigmList )
  , (pack "vb", "V",  map (first pack) verbParamMap, verbParadigmList)
  , (pack "nn", "N",  map (first pack) nounParamMap, nounParadigmList)
  ]

advParamMap =
  [("pos", "s")]

advParadigmList =
  [("mkAdv", ["s"], "")
  ]

a1 = "s (AF (APosit (Strong (GSg Utr))) Nom)"
a2 = "s (AF (APosit (Strong (GSg Utr))) Gen)"
a3 = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
a4 = "s (AF (APosit (Strong (GSg Neutr))) Gen)"
a5 = "s (AF (APosit (Strong GPl)) Nom)"
a6 = "s (AF (APosit (Strong GPl)) Gen)"
a7 = "s (AF (APosit (Weak Sg)) Nom)"
a8 = "s (AF (APosit (Weak Sg)) Gen)"
a9 = "s (AF (APosit (Weak Pl)) Nom)"
a10 = "s (AF (APosit (Weak Pl)) Gen)"
a11 = "s (AF ACompar Nom)"
a12 = "s (AF ACompar Gen)"
a13 = "s (AF (ASuperl SupStrong) Nom)"
a14 = "s (AF (ASuperl SupStrong) Gen)"
a15 = "s (AF (ASuperl SupWeak) Nom)"
a16 = "s (AF (ASuperl SupWeak) Gen)"



adjParamMap =
  [("pos indef sg u nom",      a1 )
  ,("pos indef sg u gen",      a2 )
  ,("pos indef sg n nom",      a3 )
  ,("pos indef sg n gen",      a4 )
  ,("pos indef pl nom",        a5 )
  ,("pos indef pl gen",        a6 )
  ,("pos def sg no_masc nom",  a7 )
  ,("pos def sg no_masc gen",  a8 )
  ,("pos def pl nom",          a9 )
  ,("pos def pl gen",          a10)
  ,("komp nom",                a11)
  ,("komp gen",                a12)
  ,("super indef nom",         a13)
  ,("super indef gen",         a14)
  ,("super def no_masc nom",   a15)
  ,("super def no_masc gen",   a16)
  ]


adjParadigmList =
  [ ("mkA", [a1], "") , ("mkA", [a1, a3], "")
  , ("mkA", [a1, a11, a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a7, a5 , a11, a13, a15], "")
  ]


v1  = "s (VF (VPres Act))"  
v2  = "s (VF (VPres Pass))" 
v3  = "s (VF (VPret Act))"  
v4  = "s (VF (VPret Pass))" 
v5  = "s (VF (VImper Act))" 
v6  = "s (VI (VInfin Act))" 
v7  = "s (VI (VInfin Pass))" 
v8  = "s (VI (VSupin Act))" 
v9  = "s (VI (VSupin Pass))" 
v10 = "s (VI (VPtPret (Strong (GSg Utr)) Nom))"  
v11 = "s (VI (VPtPret (Strong (GSg Utr)) Gen))"  
v12 = "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
v13 = "s (VI (VPtPret (Strong (GSg Neutr)) Gen))" 
v14 = "s (VI (VPtPret (Strong GPl) Nom))"        
v15 = "s (VI (VPtPret (Strong GPl) Gen))"        
v16 = "s (VI (VPtPret (Weak Sg) Nom))"           
v17 = "s (VI (VPtPret (Weak Sg) Gen))"           
v18 = "s (VI (VPtPret (Weak Pl) Nom))"           
v19 = "s (VI (VPtPret (Weak Pl) Gen))"

--"s (VF (VImper Pass))")   "part")
verbParamMap =
  [("pres ind aktiv",               v1 )  
  ,("pres ind s-form",              v2 ) 
  ,("pret ind aktiv",               v3 )  
  ,("pret ind s-form",              v4 ) 
  ,("imper",                        v5 ) 
  ,("inf aktiv",                    v6 ) 
  ,("inf s-form",                   v7 ) 
  ,("sup aktiv",                    v8 ) 
  ,("sup s-form",                   v9 ) 
  ,("pret_part indef sg u nom",     v10)  
  ,("pret_part indef sg u gen",     v11)  
  ,("pret_part indef sg n nom",     v12)
  ,("pret_part indef sg n gen",     v13) 
  ,("pret_part indef pl nom",       v14)        
  ,("pret_part indef pl gen",       v15)        
  ,("pret_part def sg no_masc nom", v16)           
  ,("pret_part def sg no_masc gen", v17)           
  ,("pret_part def pl nom",         v18)           
  ,("pret_part def pl gen",         v19)
  ]                                                                  

verbParadigmList =
  [ ("mkV", [v1], "")
  , ("mkV", [v6, v3, v8], "")
  , ("mkV", [v6, v1, v5, v3, v8, v10], "")
  ]

n1 = "s Sg Indef Nom" 
n2 = "s Sg Indef Gen" 
n3 = "s Sg Def Nom"   
n4 = "s Sg Def Gen"   
n5 = "s Pl Indef Nom" 
n6 = "s Pl Indef Gen" 
n7 = "s Pl Def Nom"   
n8 = "s Pl Def Gen"   

nounParamMap =
  [ ("sg indef nom", n1)
  , ("sg indef gen", n2)
  , ("sg def nom",   n3)
  , ("sg def gen",   n4)
  , ("pl indef nom", n5)
  , ("pl indef gen", n6)
  , ("pl def nom",   n7)
  , ("pl def gen",   n8)
  ]

nounParadigmList =
  [ ("mkN", [n1], "")
  , ("mkN", [n1], "utrum")
  , ("mkN", [n1], "neutrum")
  , ("mkN", [n1, n5], "")
  , ("mkN", [n1, n3, n5, n7], "")
  ]



-------------------------------------------------------------------
-- Dump GF code
-------------------------------------------------------------------

printGF :: GrammarFile -> Convert ()
printGF entries = io $ do
  writeFile "generate/saldo.gf" $
     ("--# -path=.:abstract/\n" ++
      "abstract saldo = Cat ** {\n"++
      "\n"++
      "fun\n" ++ 
      concatMap showAbs entries ++
      "}")
  writeFile "generate/saldoCnc.gf" $
     ("--# -path=.:swedish:scandinavian:abstract:common\n" ++
      "concrete saldoCnc of saldo = CatSwe ** open ParadigmsSwe in {\n"++
      "\n"++
      "flags\n"++
      "  optimize=values ; coding=utf8 ;\n"++
      "\n"++
      "lin\n" ++
      concatMap showCnc entries ++
      "}")
  where
    showAbs (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id cat ++ " : " 
                                          ++ cat ++ " ;\n"
    showCnc (id,cat,[[]],a,paradigms) = "--  " ++ mkGFName id cat ++ " has no forms \n" 
    showCnc (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id cat ++ " = " 
                                          ++ "mk" ++ cat ++ " " 
                                          ++  unwords [case lemma_v of 
                                               {[]     ->"(variants {})";
                                                xs -> unwords (map fnutta xs)} 
                                               | lemma_v <- lemmas]
                                          ++ (if null a then "" else " "++a) ++ " ;\n"
    fnutta x = "\""++x++"\""

-------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------

io :: IO a -> Convert a
io = lift 

report :: String -> Convert ()
report x = tell ([x],[]) 

tellFailing :: String -> Convert ()
tellFailing x = tell ([],[x])

lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x
