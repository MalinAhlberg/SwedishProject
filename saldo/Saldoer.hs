{-# LANGUAGE TypeSynonymInstances #-}
module Main where
import Prelude hiding (lex)
import System.IO
import System.IO.Error
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
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Debug.Trace

import qualified PGF as PGF

import SaldoXML
import Prep
import UTF8

{-----------------------------------------------------------------------------
Translates saldom.xml into GF dictionary.
To change input format, just replace parseDict
Based on Krasimir's code.
-----------------------------------------------------------------------------}

--inputFile = "lillsaldo.xml"
inputFile = "saldo100k.xml"

type Convert = StateT CState IO
data CState   = CS {errs :: [String], msg :: [String], retries :: [GrammarInfo]
                   , pgf :: FilePath, ok :: [GrammarInfo]
                   , saldo :: Lex, changes :: Int, dead :: [String] 
                   , tmps :: [FilePath] }

data GrammarInfo = G {lemma :: String, pos :: String, forms :: [[String]]
                     ,extra :: String, functions :: (String,String)
                     , paradigms :: Paradigm}
     deriving Show
type Paradigm = [(String,[String],String)]

instance Eq GrammarInfo where
 g1 == g2 = (lemma g1) == (lemma g2) 
         -- checks equality on the name only, in order to
         -- simplify deletion from retries-list

main = do
  hSetBuffering stdout NoBuffering
  putStr $ "Reading "++inputFile++" ... "
  res   <- parseDict inputFile 
  saldo <- case res of
             Just s   -> return s 
             Nothing  -> fail $ "cannot read "++inputFile 
  putStrLn ""

  st <- execStateT (createGF saldo
                    >> compileGF
                    >> loop
                    >> printGFFinal
                    >> cleanUp)  initState 
  let ms   =  "\n Messages:\n "++ unlines (msg st)
  let ers  =  "\n Errors:\n "  ++ unlines (errs st)
  let fail =  "\n Failing:\n " ++ unlines (show (retries st):dead st)
  writeFile "Messages.txt" ms
  writeFile "Errors.txt" ers        
  writeFile "Fail.txt" fail

  where
    loop = do
     updateGF 
     compileGF
     changes <- gets changes
     unless (changes == 0) loop 

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF sal = do
  io $ putStr "Creating GF source for SALDO ... "
  io $ print [lemma | (lemma,E pos tab) <- Map.toList sal, lemma == "vilkatt_N"]
  mapM findGrammar (Map.toList sal) 
  modify $ \s -> s {saldo = sal}
  
  printGF 
  report "Lexicon created"
  io $ putStrLn ""

findGrammar :: (String,Entry) -> Convert ()
findGrammar (id,E pos table) =  do
  let xs = [(gf_cat,f,paradigms) | (fm_cat,gf_cat,_,f,paradigms) <- catMap, fm_cat == pos
                                                               ,okCat gf_cat id]
  if null xs then do tellFailing ("don't accept pos "++show pos
                                  ++", word '"++id++"' rejected.")
                     isDead id
             else do let cnc = [G id gf_cat [[snd $ head' "createGF" table]]
                                   "" (f,findA id++f') paradigms 
                                             | (gf_cat,(f,f'),paradigms) <- xs]
                     modify $ \s -> s {retries = cnc++retries s}

okCat "V2" = hasPrep  
okCat "VR" = isRefl
okCat _    = const True
findA w | part `elem` preps = ")\""++part++"\"" --first paranthesis to close mkV
        | otherwise         = ""
 where part = findsndWord w

hasPrep = (`elem` preps) . findsndWord
isRefl  = (=="sig") . findsndWord

-- extracts all words except for the first
findsndWord = drop 1 . fst . break (=='.') . snd . break (=='_')

preps = Prep.prepositions

--  ["över","efter","före","bakom","mellan","av","med","under","för"
--  ,"från","framför","i","på","genom","till","utan","utom","vid","in"] -- 'vid','in' is not a prep in GF


-------------------------------------------------------------------
-- Compare the current GF version with SALDO
-------------------------------------------------------------------
updateGF = do
  report "will update"
  tmp_saldo <- gets pgf
  io $ putStr ("Reading "++tmp_saldo++" ... ")
  pgf <- io $ PGF.readPGF tmp_saldo
  io $ putStrLn ""

  io $ putStr "Updating GF source for SALDO ... "
  cnc_file <- gets retries
  modify $ \s -> s {changes = 0, retries = []}
  report $ "updating "++ unlines (map show cnc_file)
  mapM (check pgf) cnc_file
  printGF 
  c <- gets changes
  io $ putStrLn ""
  let update = ("updated "++show c++" words")
  io $ putStrLn update
  report update

check gf entry@(G id cat lemmas _ _ _) = do
  saldo <- gets saldo
  case Map.lookup id saldo of
       Just (E p t) -> checkWord gf t entry 
       Nothing      -> do tellFailing ("unknown id in SALDO: " ++ id) 
                          isDead id

checkWord gf t entry@(G id cat lemmas _ _ _) = do 
  let gf_t     = concat $ PGF.tabularLinearizes gf (read "saldoCnc")
                                               (read (mkGFName id cat))
      paramMap = head' "check" [map | (_,gf_cat,map,_,_) <- catMap
                                      , gf_cat == cat]
  checkForms paramMap t gf_t entry


checkForms paramMap fm_t gf_t entry@(G id cat lemmas _ _  _)
  | null diffs = accept entry
  | otherwise  = do c <- gets changes 
                    modify $ \s -> s {changes = c+1}
                    report $ "redo word "++id
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
 
    getNextLemma x@(G id cat lemmas _ _ []) = do
        tellFailing ("No more paradigms to choose from: " ++id)
        isDead id
    getNextLemma entry@(G id cat lemmas b f ((_,xs,a):ps)) = do
        report $ "working on "++id
        report $ "next paradigm: " ++ show xs
        report $ "to choose from: "++show ps
        forms <- mapM getLemma xs
        if Nothing `elem` forms 
           then do report ("all forms for "++id++" not found" )
                   getNextLemma (G id cat lemmas b f ps)
           else replace (G id cat [catMaybes forms] a f ps)
      where
        getLemma  gf_p=
                     case lookup fm_p fm_t of
                          Just ""   -> do report ("form "++gf_p++" was empty for "++id)
                                          return Nothing
                          Just fm_v -> return $ Just fm_v
                          x         -> do report ("form "++gf_p++" do not exist "++id)
                                          return Nothing
          where
            fm_p = head' ("getLemma "++gf_p++"."++" Word: "++id) [fm_p | (fm_p,gf_p') <- paramMap, gf_p'==gf_p]
-------------------------------------------------------------------
-- Compile with GF
-------------------------------------------------------------------

compileGF :: Convert () 
compileGF = do
  io $ putStrLn "Compile generate/saldoCnc.gf ... "
  res <- io $ rawSystem "gf" ["--batch", "--make", "generate/saldoCnc.gf", "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return () 
    ExitFailure code -> report ("compiliation failed with error code "++show code)
  (fpath,h) <- io $ openTempFile "." "tmp.pgf"
  io $ hClose h
  io $ copyFile "saldo.pgf" fpath
  addTemp fpath
  report "compilation done"
  setPGF fpath  


-------------------------------------------------------------------
-- Generate GF identifier
-------------------------------------------------------------------

mkGFName :: String -> String -> String
mkGFName id' cat = name++"_"++cat
  where
       dash2us '-' = '_'
       dash2us x = x
       num x = if isDigit (head' "isDigit" x) then 'x':x else x
       name =  (++ [last id']) $ num {-$ dropWhile (== '_')-} $ transform_letters 
                   $ map dash2us $ takeWhile (/= '.') id'
                   -- $ dropWhile (== '_')-- $ undot (decodeUTF8 id')
       transform_letters w | any (`elem` translated) w = (++"_1") $ concat $ map trans w
                           | otherwise                 = w
                           
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

translated = ['\229', '\197', '\228', '\196', '\224', '\225', '\232', '\233', '\234', '\231', '\252', '\246', '\241', '\214', '\183']
-------------------------------------------------------------------
-- Mapping from SALDO categories/params to GF Resource Library
-------------------------------------------------------------------

catMap  = 
  [ (pack "ab", "Adv", map (first pack) advParamMap,  ("mkAdv",""), advParadigmList )
  , (pack "av",   "A", map (first pack) adjParamMap,  ("mkA",""), adjParadigmList )
  , (pack "vb",   "V", map (first pack) verbParamMap, ("mkV",""), verbParadigmList)
  , (pack "nn",   "N", map (first pack) nounParamMap, ("mkN",""), nounParadigmList)
  , (pack "vbm", "V2", map (first pack) verb2ParamMap, ("dirV2 (partV (mkV",")"), verb2ParadigmList)
  , (pack "vbm", "VR", map (first pack) verbRParamMap, ("dirV2 (reflV (mkV","))"), verbRParadigmList)
  ]

advParamMap =
  [("pos", "s")]

advParadigmList =
  [("mkAdv", ["s"], "") ]

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

-- could use normal verbParamMap if we are sure it is a preposition,
-- and will look the same in all paradims
verb2ParamMap = map (first (++" 1:1-2")) verbParamMap
              ++map (\(a,b) -> (a++" 1:2-2","part")) verbParamMap

verb2ParadigmList =
  [ ("", [v1], "" )                
  , ("", [v6, v3, v8], "")              
  , ("", [v6, v1, v5, v3, v8, v10], "") 
  ]                                          

verbRParamMap = map (first (++" 1:1-2")) verbParamMap

verbRParadigmList =
  [ ("", [v1],  "")                
  , ("", [v6, v3, v8], "")              
  , ("", [v6, v1, v5, v3, v8, v10], "") 
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
printGFFinal = do
  good <- gets ok
  printGF' good

printGF :: Convert ()
printGF = do
  new  <- gets retries
  good <- gets ok
  let entries = new++good
  printGF' entries

printGF' entries =
  io $ do
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
    showAbs (G id cat lemmas a _ paradigms) = "  " ++ mkGFName id cat ++ " : " 
                                            ++ find cat ++ " ;\n"
    showCnc (G id cat [[]] a _ paradigms)    = "--  " ++ mkGFName id cat ++ " has not enough forms \n" 
    showCnc (G id cat lemmas a (mk,end) paradigms) 
          = "  " ++ mkGFName id cat ++ " = " ++ mk++ " "
                             ++  unwords [case lemma_v of 
                                  {[]     ->"(variants {})";
                                   xs -> unwords (map fnutta xs)} 
                                  | lemma_v <- lemmas]
                             ++ (if null a then "" else " "++a++" ") 
                             ++end ++ " ;\n"
    -- avoid putting extra fnutts on variants"
    -- wrong! how to handle this.. maybe won't occur again?
    fnutta x@"variant {}" = "("++x++")"
    fnutta x = "\""++x++"\""
    find "VR" = "V"
    find x    = x


cleanUp :: Convert ()
cleanUp = do
 ts <- gets tmps
 io $ mapM_ (try . removeFile) ts

-------------------------------------------------------------------
-- State
-------------------------------------------------------------------
 
initState = CS {errs = [], msg = [], retries = []
               , pgf = "", ok = [] ,dead = []
               , saldo = Map.empty, changes = 0, tmps = [] }

setPGF f = modify $ \s -> s { pgf = f}
replace gr = modify $ \s -> s {retries = gr : delete gr (retries s)} -- hehe, make nice?
-- add to dead , remove from retries
isDead d = modify $ \s -> s {dead = d:dead s, retries = delete (emptyG d) (retries s)}  -- hehe, make nice?
  where emptyG d = G d [] [] [] ("","") []
--add to ok, remove from retries
accept :: GrammarInfo -> Convert ()
accept e = do report $ "accepting "++(lemma e)
              modify $ \s -> s {retries = delete e (retries s), ok = e:(ok s)}
              r <- gets retries
              report $ "deleted "++lemma e++" from retries. result: "++show (e `elem` r)

addTemp :: FilePath -> Convert ()
addTemp f = modify $ \s -> s {tmps = f:tmps s}
-------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------

io :: IO a -> Convert a
io = lift 

report :: String -> Convert ()
report x = modify $ \s  -> s {msg = x:msg s}

tellFailing :: String -> Convert ()
tellFailing x = modify $ \s -> s {errs = x:errs s}

lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x


f = do pgf <- PGF.readPGF "../gf/BigTest.pgf"
       let g  = PGF.tabularLinearizes  pgf (read "BigTestSwe") (read "switch8on_V2")
       return g
