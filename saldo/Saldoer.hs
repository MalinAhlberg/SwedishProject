{-# LANGUAGE TypeSynonymInstances #-}
module Saldoer where
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
import Control.Monad.Error
import Control.Arrow
import Debug.Trace

import qualified PGF as PGF

import SaldoXML
import Prep
import UTF8

{-----------------------------------------------------------------------------
Translates saldom.xml into GF dictionary.
To change input format, just replace parseDict
The main fuction is extract, which could operate alone or together with
SaldoMain
-----------------------------------------------------------------------------}

inputFile = "lillsaldo.xml"
--inputFile = "saldo100k.xml"
--inputFile = "saldom.xml"


type Convert = StateT CState (ErrorT String IO)
data CState   = CS {errs :: [String], msg :: [String], retries :: [GrammarInfo]
                   , pgf :: FilePath, ok :: [GrammarInfo]
                   , saldo :: Lex, changes :: Int, dead :: [String] 
                   , tmps :: [FilePath], partNo :: Int
                   , selection :: Maybe [String], name :: String}

data GrammarInfo = G {lemma :: String, pos :: String, forms :: [[String]]
                     ,extra :: String, functions :: (String,String)
                     , paradigms :: Paradigm}
     deriving Show
type Paradigm = [(String,[String],String)]

instance Eq GrammarInfo where
 g1 == g2 = lemma g1 == lemma g2 
         -- checks equality on the name only, in order to
         -- simplify deletion from retries-list

extract select name inputFile n  = do
  hSetBuffering stdout NoBuffering
  putStr $ "Reading "++inputFile++" ... "
  res   <- parseDict inputFile 
  saldo <- case res of
             Just s   -> return s 
             Nothing  -> fail $ "cannot read "++inputFile 
  putStrLn ""

  mst <- runErrorT $ execStateT (createGF saldo
                    >> compileGF
                    >> loop
                    >> printGFFinal
                    >> cleanUp)  (initState n select name)
  er <- case mst of 
             Left er  -> return er
             Right st -> do 
                 let ms   =  "\n Messages:\n "++ unlines (msg st)
                 let fails =  "\n Failing:\n " ++ unlines (show (retries st):dead st)
                 writeFile ("MessagesS"++show n++".txt") ms -- S to keep old files
                 writeFile ("FailS"++show n++".txt") fails
                 appendCode name $ ok st
                 return $ unlines (errs st)
  writeFile ("Errors"++show n++".txt")  $ "\n Errors:\n "  ++ er

  where
    loop = do
     updateGF 
     compileGF
     changes <- gets changes
     unless (changes == 0) loop 

appendCode name entries = do
  appendFile ("saldo"++name++".gf")    (concatMap showAbs entries)
  appendFile ("saldo"++name++"Cnc.gf") (concatMap showCnc entries)

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF :: Lex -> Convert ()
createGF sal = do
  io $ putStr "Creating GF source for SALDO ... "
  mapM_ findGrammar (Map.toList sal) 
  todo <- gets retries
  when (null todo) $ fail "No words from this section. Skipping"
  modify $ \s -> s {saldo = sal}
  
  printGF 
  report "Lexicon created"
  io $ putStrLn ""

findGrammar :: (String,Entry) -> Convert ()
findGrammar (id,E pos table) =  do
  rest <- gets selection
  let keep = maybe True (id `elem`) rest 
  when keep $ do
   let xs = [(gf_cat,f,paradigms) | (fm_cat,gf_cat,_,f,paradigms) <- catMap
                                    , fm_cat == pos ,okCat gf_cat id]
   if null xs then do tellFailing ("don't accept pos "++show pos
                                   ++", word '"++id++"' rejected.")
                      isDead id
              else do let cnc = [G id gf_cat [[snd $ head' "createGF" table]]
                                    "" (f,findA gf_cat id++f') paradigms 
                                              | (gf_cat,(f,f'),paradigms) <- xs]
                      modify $ \s -> s {retries = cnc++retries s}

--- particles can be prepositions (hitta på), adverbs (åka hem), nouns (åka hem)... 
--  we first check for reflexives, and accept the rest as particles.
okCat "VP" = hasPart
okCat "VR" = isRefl
okCat _    = const True
findA "VP" w =  ")\""++part++"\""
 where part = findsndWord w
findA _  _ = ""
--w | part `elem` preps = ")\""++part++"\"" --paranthesis to close mkV
        -- | otherwise         = ""

hasPart = (\x -> all isAlpha x &&  x/="sig") . findsndWord
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
  mapM_ (check pgf) cnc_file
  printGF 
  c <- gets changes
  io $ putStrLn ""
  let update = "updated "++show c++" words"
  io $ putStrLn update
  report update

check gf entry@(G id cat lemmas _ _ _) = do
  saldo <- gets saldo
  case Map.lookup id saldo of
       Just (E p t) -> checkWord gf t entry 
       Nothing      -> do tellFailing ("unknown id in SALDO: " ++ id) 
                          isDead id

checkWord gf t entry@(G id cat lemmas _ _ _) = do 
  langName <- getLangName
  let gf_t     = concat $ PGF.tabularLinearizes gf (read $ langName)
                                               (read (mkGFName id cat))
      paramMap = head' "check" [map | (_,gf_cat,map,_,_) <- catMap
                                      , gf_cat == cat]
  checkForms paramMap t gf_t entry

checkForms
  :: Eq a => [(a, [String])] -> [(a, [Char])] -> [([Char], [Char])] -> GrammarInfo -> Convert ()
checkForms paramMap fm_t gf_t entry@(G id cat lemmas _ _  _)
  | null diffs = accept entry
  | otherwise  = do c <- gets changes 
                    modify $ \s -> s {changes = c+1}
                    report $ "redo word "++id
                    report (show [[(lookup f fm_t,lookup g' gf_t) | g' <- g ] | (f,g) <- paramMap])
                    getNextLemma $!  entry 
  where
    diffs = [(fm_p,fm_v,gf_p,gf_v)  -- | gf_p' <- gf_p
                                   | (fm_p,gf_p) <- paramMap
                                   , fm_vs      <- [lookup' fm_p fm_t]
                                   , let gf_v  = catMaybes $ map (`lookup` gf_t) gf_p 
                                   , Just fm_v  <- [isDiff gf_v fm_vs]]
   -- if there is no information about the form in saldo, we chose to accept it
    isDiff  _ [] = Nothing
    isDiff ys xs | any (`elem` xs) ys = Nothing
                 | otherwise   = Just $ head xs
 
    getNextLemma x@(G id cat lemmas _ _ []) = do
        tellFailing ("No more paradigms to choose from: " ++id)
        isDead id
    getNextLemma entry@(G id cat lemmas b f ((pre,xs,a):ps)) = do
        report $ "working on "++id
        report $ "next paradigm: " ++ show xs
        report $ "to choose from: "++show ps
        forms <- mapM getLemma xs
        if Nothing `elem` forms 
        -- to do: if the comparative forms for an adjective doesn't exist, add compounda
           then do report ("all forms for "++id++" not found" )
                   getNextLemma (G id cat lemmas b f ps)
           else replace (G id cat [catMaybes forms] a {-(specialF pre-} f ps)
      where
        specialF "mk3A" _ = ("mk3A","") --- to be done more nicely -- can probably be removed now, but test first
        specialF x           f = f
        getLemma  gf_p=
                     case lookup fm_p fm_t of
                          Just ""   -> do report ("form "++gf_p++" was empty for "++id)
                                          return Nothing
                          Just fm_v -> return $ Just fm_v
                          x         -> do report ("form "++gf_p++" do not exist "++id)
                                          return Nothing
          where
            fm_p = head' ("getLemma "++gf_p++"."++" Word: "++id) [fm_p | (fm_p,gf_p') <- paramMap, gf_p `elem` gf_p']
-------------------------------------------------------------------
-- Compile with GF
-------------------------------------------------------------------

compileGF :: Convert () 
compileGF = do
  io $ putStrLn "Compile generate/saldoCnc.gf ... "
  concName <- getCncName
  pgfName  <- getPGFName
  res <- io $ rawSystem "gf" ["--batch", "--make", concName, "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return () 
    ExitFailure code -> do
                   n <- gets partNo 
                   io $ putStrLn "failed to create PGF.\nWill skip this section"
                   fail ("compiliation failed with error code "++show code
                         ++"\nPartition "++show n++" will be skipped")
  (fpath,h) <- io $ openTempFile "." "tmp.pgf"
  io $ hClose h
  io $ copyFile pgfName fpath
  addTemp fpath
  report "compilation done"
  setPGF fpath  


-------------------------------------------------------------------
-- Generate GF identifier
-------------------------------------------------------------------

mkGFName :: String -> String -> String
mkGFName id' cat = name++"_"++toGFcat cat
  where
       toGFcat "VR" = "V"
       toGFcat "VP" = "V"
       toGFcat  v   = v
       dash2us '-'  = '_'
       dash2us    x = x
       num x = if isDigit (head' "isDigit" x) then 'x':x else x
       name =  undot -- $ (++ [last id']) 
              $ num 
              $ transform_letters 
              $ map dash2us 
              $ takeWhile (/= '.')  -- in case there are unwanted (unintedned?) dots left
              $ undot (decodeUTF8 id')
       transform_letters w | any (`elem` translated) w = (++"_1") $ concatMap trans w
                           | otherwise                 = concatMap trans w -- to be sure..
                           
       trans '\229' = "aa"
       trans '\197' = "AA"
       trans '\228' = "ae"
       trans '\196' = "AE"
       trans '\224' = "a"
       trans '\225' = "a"
       trans '\232' = "e_"
       trans '\233' = "_e"
       trans '\234' = "ee"
       trans '\231' = "c"
       trans '\252' = "u"
       trans '\244' = "oo"
       trans '\246' = "oe"
       trans '\241' = "n"       
       trans '\214' = "OE"
       trans '\183' = "_"
       trans x   | isAscii x =  [x]
                 | otherwise = "x"
undot [] = []
undot ('.':'.':xs) = '_' : undot xs 
undot     ('.':xs) = '_' : undot xs
undot       (x:xs) = x:undot xs

translated = ['\229', '\197', '\228', '\196', '\224', '\225', '\232', '\233', '\234', '\231', '\252', '\246', '\241', '\214', '\183','\244']
-------------------------------------------------------------------
-- Mapping from SALDO categories/params to GF Resource Library
-------------------------------------------------------------------

-- all word classes that should be imported should be listed here. 
catMap  = 
  [ (pack "ab", "Adv", map (first pack) advParamMap,  ("mkAdv",""), advParadigmList )
  , (pack "av",   "A", map (first pack) adjParamMap,  ("mkA",""), adjParadigmList )
  , (pack "vb",   "V", map (first pack) verbParamMap, ("mkV",""), verbParadigmList)
  , (pack "nn",   "N", map (first pack) nounParamMap, ("mkN",""), nounParadigmList)
  -- particles were V2. Why? -"dirV2 (partV (mkV",")"
  -- VR should not be V2 either.
--  , (pack "vbm", "VR", map (first pack) verbRParamMap, ("reflV (mkV",")"), verbRParadigmList)
--  , (pack "vbm", "VP", map (first pack) verbPParamMap, ("partV (mkV",""), verbPParadigmList)
  ]
  
-- For prepositions, not run automatically
prepCatMap =  [(pack "pp", "Prep", [(pack "invar","s")],("mkPrep",""),[("mkPrep",["s"],"")])]

advParamMap :: [(String,[String])]
advParamMap =
  [("pos", ["s"]),("invar",["s"])] -- is invar needed?

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
  [("pos indef sg u nom",      [a1] )
  ,("pos indef sg u gen",      [a2] )
  ,("pos indef sg n nom",      [a3] )
  ,("pos indef sg n gen",      [a4] )
  ,("pos indef pl nom",        [a5] )
  ,("pos indef pl gen",        [a6] )
  ,("pos def sg no_masc nom",  [a7] )
  ,("pos def sg no_masc gen",  [a8] )
  ,("pos def pl nom",          [a9] )
  ,("pos def pl gen",          [a10])
  ,("komp nom",                [a11])
  ,("komp gen",                [a12])
  ,("super indef nom",         [a13])
  ,("super indef gen",         [a14])
  ,("super def no_masc nom",   [a15])
  ,("super def no_masc gen",   [a16])
  ]


adjParadigmList =
  [ ("mkA", [a1], "") , ("mkA", [a1, a3], "")
  , ("mkA", [a1, a11, a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a7, a5 , a11, a13, a15], "")
  , ("mk3A", [a1, a3, a5], "") 
  ]


v1  = "s (VF (VPres Act))"  
v2  = "s (VF (VPres Pass))" 
v3  = "s (VF (VPret Act))"  
v4  = "s (VF (VPret Pass))" 
v5  = "s (VF (VImper Act))" 
v5a  = "s (VF (VImper Pass))" 
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
  [("pres ind aktiv",               [v1] )  
  ,("pres ind s-form",              [v2] ) 
  ,("pret ind aktiv",               [v3] )  
  ,("pret ind s-form",              [v4] ) 
  ,("imper",                        [v5,v5a] ) 
  ,("inf aktiv",                    [v6] ) 
  ,("inf s-form",                   [v7] ) 
  ,("sup aktiv",                    [v8] ) 
  ,("sup s-form",                   [v9] ) 
  ,("pret_part indef sg u nom",     [v10])  
  ,("pret_part indef sg u gen",     [v11])  
  ,("pret_part indef sg n nom",     [v12])
  ,("pret_part indef sg n gen",     [v13]) 
  ,("pret_part indef pl nom",       [v14])        
  ,("pret_part indef pl gen",       [v15])        
  ,("pret_part def sg no_masc nom", [v16])           
  ,("pret_part def sg no_masc gen", [v17])           
  ,("pret_part def pl nom",         [v18])           
  ,("pret_part def pl gen",         [v19])
  ]                                                                  

verbParadigmList =
  [ ("mkV", [v1], "")
  , ("mkV", [v6, v3, v8], "")
  , ("mkV", [v6, v1, v5, v3, v8, v10], "")
  ]

-- could use normal verbParamMap if we are sure it is a preposition,
-- and will look the same in all paradims
verbPParamMap = map (first (++" 1:1-2")) verbParamMap
              ++map (\(a,b) -> (a++" 1:2-2",["part"])) verbParamMap

verbPParadigmList =
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
  [ ("sg indef nom", [n1])
  , ("sg indef gen", [n2])
  , ("sg def nom",   [n3])
  , ("sg def gen",   [n4])
  , ("pl indef nom", [n5])
  , ("pl indef gen", [n6])
  , ("pl def nom",   [n7])
  , ("pl def gen",   [n8])
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
  num <- gets partNo
  io $ printGF' good (show num) "Fin"

printGF :: Convert ()
printGF = do
  new  <- gets retries
  good <- gets ok
  let entries = new++good
  num <- gets partNo
  nam <- gets name
  io $ printGF' entries (show num) nam

printGF' [] _ _ = putStrLn "no lemmas to write"
printGF' entries num name = do
  let absName = "generate/saldo"++name++num++".gf"
      cncName = "generate/saldo"++name++num++"Cnc.gf"
  writeFile  absName $
      absHeader num name ++
      concatMap showAbs entries ++
      "}"
  writeFile cncName $
      concHeader num name ++ 
      concatMap showCnc entries ++
      "}"

showAbs (G id cat lemmas a _ paradigms) = "  " ++ mkGFName id cat ++ " : " 
                                        ++ find cat ++ " ;\n"
  where 
    find "VR" = "V"
    find "VP" = "V"
    find x    = x
showCnc (G id cat [[]] a _ paradigms)    = "--  " ++ mkGFName id cat ++ " has not enough forms \n" 
showCnc (G id cat lemmas a (mk,end) paradigms) 
      = "  " ++ mkGFName id cat ++ " = " ++ mk++ " "
                         ++  unwords [case lemma_v of 
                              {[]     ->"(variants {})";
                               xs -> unwords (map fnutta xs)} 
                              | lemma_v <- lemmas]
                         ++ (if null a then "" else " "++a++" ") 
                         ++end ++ " ;\n"
 where  
    -- avoid putting extra fnutts on variants"
    -- wrong! how to handle this.. maybe won't occur again?
    fnutta x@"variant {}" = "("++x++")"
    fnutta x = "\""++x++"\""

absHeader n nam =
  "--# -path=.:abstract:alltenses/\n" ++ "abstract saldo"++nam++n++" = Cat ** {\n"++
      "\n"++ "fun\n" 

concHeader n nam = 
      "--# -path=.:swedish:scandinavian:abstract:common:alltenses\n" ++
      "concrete saldo"++nam++n++"Cnc of saldo"++nam++n++" = CatSwe ** open ParadigmsSwe in {\n"++
      "\n"++
      "flags\n"++
      "  optimize=values ; coding=utf8 ;\n"++
      "\n"++
      "lin\n"
 
getCncName, getAbsName, getPGFName :: Convert String 
getCncName = do
 n   <- gets partNo
 nam <- gets name
 return $ "generate/saldo"++nam++show n++"Cnc.gf"
getAbsName = do
 n   <- gets partNo
 nam <- gets name
 return $ "generate/saldo"++nam++show n++".gf"
getLangName= do
 n   <- gets partNo
 nam <- gets name
 return $ "saldo"++nam++show n++"Cnc"
getPGFName = do
 n   <- gets partNo
 nam <- gets name
 return $ "saldo"++nam++show n++".pgf"


cleanUp :: Convert ()
cleanUp = do
 ts <- gets tmps
 io $ mapM_ (try . removeFile) ts

-------------------------------------------------------------------
-- State
-------------------------------------------------------------------
 
initState n s name 
  = CS {errs = [], msg = [], retries = []
        , pgf = "", ok = [] ,dead = []
        , saldo = Map.empty, changes = 0, tmps = []
        , partNo = n, selection = s, name = name }

setPGF f = modify $ \s -> s { pgf = f}
replace gr = modify $ \s -> s {retries = gr : delete gr (retries s)} -- hehe, make nice?
-- add to dead , remove from retries
isDead d = modify $ \s -> s {dead = d:dead s, retries = delete (emptyG d) (retries s)}  -- hehe, make nice?
  where emptyG d = G d [] [] [] ("","") []
--add to ok, remove from retries
accept :: GrammarInfo -> Convert ()
accept e = do report $ "accepting "++lemma e
              modify $ \s -> s {retries = delete e (retries s), ok = e:ok s}
              r <- gets retries
              report $ "deleted "++lemma e++" from retries. result: "++show (e `elem` r)

setPartNo :: Int -> Convert ()
setPartNo n = modify $ \s -> s {partNo = n}
addTemp :: FilePath -> Convert ()
addTemp f = modify $ \s -> s {tmps = f:tmps s}
-------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------

io :: IO a -> Convert a
io = lift . lift

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

