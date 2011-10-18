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
--import CommandsSw
--import Frontend
--import TestSw
--import Dictionary(Entry,set_lemma_id,get_lemma_id,unDict)
--import qualified Dict.Abs as Abs
--import EncodeSw
--import Dict.ErrM(Err(..))
--import DictToDictionary
--import General(Str(..))

{-
 ghci make-saldo.hs -ifm/sblex/src/lib/ -ifm/sblex/src/Dict/ -ifm/sblex/src/saldo/
-}

type Convert = WriterT [String] IO

inputFile = "lillsaldo.xml"

main = do
  hSetBuffering stdout NoBuffering
  putStr $ "Reading "++inputFile++" ... "
  res   <- parseDict inputFile 
  saldo <- case res of
             Just s   -> return s -- $ Map.fromList [(mkGFName (get_lemma_id entry)
                                    --              , entry) | entry <- unDict dict]
             Nothing  -> fail $ "cannot read "++inputFile 
  putStrLn ""

  (cnc_file,err) <- runWriterT $ createGF saldo
  putStrLn $ "errors: "++ unlines err
  
  (tmp_saldo,err') <- runWriterT compileGF
  err'' <- execWriterT (loop tmp_saldo saldo cnc_file)
  putStrLn $ "errors: "++ unlines (err'++err'')
  where
    loop tmp_saldo saldo cnc_file = do

      (cnc_file,changes) <- updateGF tmp_saldo saldo cnc_file
      tmp_saldo <- compileGF
      if changes == 0
        then return ()
        else loop tmp_saldo saldo cnc_file

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF saldo = do
  io $ putStr "Creating GF source for SALDO ... "
  io $ print [lemma | (lemma,E pos tab) <- Map.toList saldo, lemma == "vilkatt_N"]
  let cnc_file = [(id,gf_cat,[[snd $ head table]],"",paradigms) 
                  | (id,E pos table) <- Map.toList saldo
                     , (fm_cat,gf_cat,_,paradigms) <- catMap, fm_cat == pos]
                     -- ? , null [x | x@(_,(_,Str [])) <- inft]]
  printGF cnc_file
  io $ putStrLn ""
  return cnc_file
{-
createGF saldo = do
  putStr "Creating GF source for SALDO ... "
  print [lemma | (E lemma pos tab) <- saldo, lemma == "vilkatt_N"]
  let cnc_file = [(id,gf_cat,[[lemma]],"",paradigms) 
                  | (id,lemma,_,cat,_, inft,_) <- Map.elems saldo
                     , (fm_cat,gf_cat,_,paradigms) <- catMap, fm_cat == cat
                     , null [x | x@(_,(_,Str [])) <- inft]]
  printGF cnc_file
  putStrLn ""
  return cnc_file


-}

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
  cnc_file' <- mapM (check count saldo pgf) cnc_file
  printGF cnc_file'
  io $ putStrLn ""
  c <- io $ readIORef count
  io $ putStrLn ("updated "++show c++" words")
  return (cnc_file',c)

check count saldo gf entry@(id,cat,lemmas,_,paradigms) = do
  fm_t <- case Map.lookup id saldo of
               Just entry -> let (E p t) = entry in return t
                  -- handle better!
               Nothing    -> report ("unknown id in SALDO: " ++ id) >> fail "badbad"
  let gf_t = concat --(\a -> (fst $ head $ head a, map snd)) 
                         $  PGF.tabularLinearizes gf (read "saldoCnc")
                                                     (read (mkGFName id cat))
      paramMap = head [map | (_,gf_cat,map,_) <- catMap, gf_cat == cat]
  checkForms count paramMap fm_t gf_t entry
  --return undefined

checkForms count paramMap fm_t gf_t entry@(id,cat,lemmas,_,paradigms)
  | null diffs = report (show paramMap) 
                 >> report (show fm_t)
                 >> report (show gf_t)
                 >> report (show [(lookup f fm_t,lookup g gf_t) | (f,g) <- paramMap])
                 >> return entry
  | otherwise  = do c <- io $ readIORef count
                    io $ writeIORef count $! c+1
                    getNextLemma entry -- this may have to be done strict
  where
    diffs = [(fm_p,fm_v,gf_p,gf_v) | (fm_p,gf_p) <- paramMap
                                   , Just fm_v <- [lookup fm_p fm_t]
                                   , Just gf_v <- [lookup gf_p gf_t]
                                   , null (intersect fm_v gf_v)]
-- doesnt seem to find the word in saldo nor gf..
    getNextLemma (id,cat,lemmas,_,[]         ) = report ("No more paradigms to choose from: "++id++show fm_t)
                                                 >> fail "badLemma"
    getNextLemma (id,cat,lemmas,_,(_,xs,a):ps) = return (id,cat,[map getLemma xs],a, ps)
      where
        getLemma gf_p =  case lookup fm_p fm_t of
                          Just fm_v -> fm_v
                          _                 -> error "variant?"
          where
            fm_p = head [fm_p | (fm_p,gf_p') <- paramMap, gf_p'==gf_p]
-------------------------------------------------------------------
-- Compile with GF
-------------------------------------------------------------------

compileGF = do
  io $ putStrLn "Compile generate/saldoCnc.gf ... "
  res <- io $ rawSystem "gf" ["--batch", "--make", {-"-parser=off",-} "generate/saldoCnc.gf", "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return () --io $ putStrLn ""
    ExitFailure code -> report ("failed with error code "++show code)
  (fpath,h) <- io $ openTempFile "." "tmp.pgf"
  io $ hClose h
  io $ copyFile "saldo.pgf" fpath
  return fpath


-------------------------------------------------------------------
-- Generate GF identifier
-------------------------------------------------------------------

-- why all the '?'s ?
mkGFName id' cat = name++"_"++cat
  where
       dash2us '-' = '_'
       dash2us x = x
       num x = if isDigit (head x) then 'x':x else x
       name =  num $ dropWhile (== '_') $ transform_letters 
                   $ map dash2us $ dropWhile (== '_') $ undot (decodeUTF8 id')
       transform_letters = concat . map trans
       trans '\229' = "aa"
       trans '\197' = "AA"
       trans '\228' = "ae"
       trans '\196' = "AE"
       trans '\224' = "a"
       trans '\225' = "a"
       --trans '\231' = "s"
       trans '\232' = "e"
       trans '\233' = "e"
       trans '\234' = "e"
       trans '\231' = "c"
       trans '\252' = "u"
       trans '\246' = "oe"
       trans '\241' = "n"       
       trans '\214' = "OE"
       --trans '?'    = "ae"
       --trans '?'    = "AE"
       --trans '\246' = "oe"
       --trans '\214' = "OE"
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
  [("invar", "")]

advParadigmList =
  [("mkAdv", [""], "")
  ]

adjParamMap =
  [("pos indef sg u nom",    "s (AF (APosit (Strong (GSg Utr))) Nom)"  )
  ,("pos indef sg u gen",    "s (AF (APosit (Strong (GSg Utr))) Gen)"  )
  ,("pos indef sg n nom",    "s (AF (APosit (Strong (GSg Neutr))) Nom)")
  ,("pos indef sg n gen",    "s (AF (APosit (Strong (GSg Neutr))) Gen)")
  ,("pos indef pl nom",      "s (AF (APosit (Strong (GPl g))) Nom)"    )
  ,("pos indef pl gen",      "s (AF (APosit (Strong (GPl g))) Gen)"    )
  ,("pos def sg no_masc nom","s (AF (APosit (Weak Sg)) Nom)"       )
  ,("pos def sg no_masc gen","s (AF (APosit (Weak Sg)) Gen)"       )
  ,("pos def pl nom",        "s (AF (APosit (Weak Pl)) Nom)"       )
  ,("pos def pl gen",        "s (AF (APosit (Weak Pl)) Gen)"       )
  ,("komp nom",              "s (AF ACompar Nom)"                )
  ,("komp gen",              "s (AF ACompar Gen)"                )
  ,("super indef nom",       "s (AF (ASuperl SupStrong) Nom)"     )
  ,("super indef gen",       "s (AF (ASuperl SupStrong) Gen)"     )
  ,("super def no_masc nom", "s (AF (ASuperl SupWeak) Nom)"       )
  ,("super def no_masc gen", "s (AF (ASuperl SupWeak) Gen)"       )
  ]

-- add s here?
adjParadigmList =
  [ ("mkA", ["AF(APosit(StrongSgUtr))Nom)"], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom)", "AF(APosit(StrongSgNeutr))Nom)"], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom)", "AFAComparNom ", "AF(ASuperlSupStrong)Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom)", "AF(APosit(StrongSgNeutr))Nom ", "AF(APosit(StrongPlg))Nom)", "AFAComparNom)", "AF(ASuperlSupStrong)Nom)"], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom)", "AF(APosit(StrongSgNeutr))Nom)", "AF(APosit(StrongPlg))Nom)", "AFAComparNom)", "AF(ASuperlSupStrong)Nom)"], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom)", "AF(APosit(StrongSgNeutr))Nom)", "AF(APosit(WeakSg))Nom)", "AF(APosit(StrongPlg))Nom)", "AFAComparNom)", "AF(ASuperlSupStrong)Nom)", "AF(ASuperlSupWeak)Nom)"], "")
  ]

verbParamMap =
  [("pres ind aktiv",               "s (VF (VPres Act)")
  ,("pres ind s-form",              "s (VF (VPres Pass)")
  ,("pret ind aktiv",               "s (VF (VPret Act)")
  ,("pret ind s-form",              "s (VF (VPret Pass)")
  ,("imper",                        "s (VF (VImper Act)")
  ,("inf aktiv",                    "s (VI (VInfin Act)")
  ,("inf s-form",                   "s (VI (VInfin Pass)")
  ,("sup aktiv",                    "s (VI (VSupin Act)")
  ,("sup s-form",                   "s (VI (VSupin Pass)")
  ,("pret_part indef sg u nom",     "s (VI (VPtPret (Strong Sg Utr) Nom)")
  ,("pret_part indef sg u gen",     "s (VI (VPtPret (Strong Sg Utr) Gen)")
  ,("pret_part indef sg n nom",     "s (VI (VPtPret (Strong Sg Neutr) Nom)")
  ,("pret_part indef sg n gen",     "s (VI (VPtPret (Strong Sg Neutr) Gen)")
  ,("pret_part indef pl nom",       "s (VI (VPtPret (Strong Pl g) Nom)")
  ,("pret_part indef pl gen",       "s (VI (VPtPret (Strong Pl g) Gen)")
  ,("pret_part def sg no_masc nom", "s (VI (VPtPret (Weak Sg) Nom)")
  ,("pret_part def sg no_masc gen", "s (VI (VPtPret (Weak Sg) Gen)")
  ,("pret_part def pl nom",         "s (VI (VPtPret (Weak Pl) Nom)")
  ,("pret_part def pl gen",         "s (VI (VPtPret (Weak Pl) Gen)")
  ]

verbParadigmList =
  [ ("mkV", ["VF(VPresAct)"], "")
  , ("mkV", ["VI(VInfinAct)", "VF(VPretAct)", "VI(VSupinAct)"], "")
  , ("mkV", ["VI(VInfinAct)", "VF(VPresAct)", "VF(VImperAct)", "VF(VPretAct)", "VI(VSupinAct)", "VI(VPtPret(StrongSgUtr)Nom)"], "")
  ]

nounParamMap =
  [ ("sg indef nom", "s (Sg Indef Nom)")
  , ("sg indef gen", "s (Sg Indef Gen)")
  , ("sg def nom",   "s (Sg Def Nom)")
  , ("sg def gen",   "s (Sg Def Gen)")
  , ("pl indef nom", "s (Pl Indef Nom)")
  , ("pl indef gen", "s (Pl Indef Gen)")
  , ("pl def nom",   "s (Pl Def Nom)")
  , ("pl def gen",   "s (Pl Def Gen)")
  ]

nounParadigmList =
  [ ("mkN", ["Sg Indef Nom)"], "")
  , ("mkN", ["Sg Indef Nom)"], "utrum")
  , ("mkN", ["Sg Indef Nom)"], "neutrum")
  , ("mkN", ["Sg Indef Nom)", "Pl Indef Nom)"], "")
  , ("mkN", ["Sg Indef Nom)", "Sg Def Nom)", "Pl Indef Nom)", "Pl Def Nom)"], "")
  ]


-------------------------------------------------------------------
-- Read GF code
-------------------------------------------------------------------
{-
readGF = do
  ls <- fmap (init . drop 7 . lines) $ readFile "generate/saldoCnc.gf"
  return (map parseLine ls)
  where
    parseLine l = 
      case readP_to_S parseLine' l of
        [(x,s)] | all isSpace s -> x
        _                       -> error ("parse error: "++l)

    parseLine' = do
      Ident id <- lex
      Punc "=" <- lex
      Ident para <- lex
      lemmas <- many1 $ do
                  String lemma <- lex
                  return lemma
      Punc ";" <- lex
      return (id,drop 2 para,lemmas,[])
-}
-------------------------------------------------------------------
-- Dump GF code
-------------------------------------------------------------------

printGF :: [(String, [Char], [[[Char]]], [Char], t0)] -> Convert ()
printGF entries = io $ do
  writeFile "generate/saldo.gf" $
     ("--# -path=.:abstract/\n" ++
      "abstract saldo = Cat ** {\n"++
      "\n"++
      "fun\n" ++ 
      concatMap showAbs entries ++
      "}")
  writeFile "generate/saldoCnc.gf" $
     --("--# -path=.:swedish:scandinavian\n" ++
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
    showAbs (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id cat ++ " : " ++ cat ++ " ;\n"
    showCnc (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id cat ++ " = " ++ "mk" ++ cat ++ " " ++
                                               unwords [case lemma_v of {[]->"(variants {})"; (x:xs) 
                                                            -> "\"" ++ x ++ "\""} | lemma_v <- lemmas] ++
                                               (if null a then "" else " "++a) ++ " ;\n"


-------------------------------------------------------------------
-- FM related stuff
-------------------------------------------------------------------
{-
data SAL = SAL
 deriving Show

instance Language SAL where
 name         _ = "SALDO v1.0"
-- internDict   _ = swedishDict
 paradigms    _ = foldr insertCommand emptyC commands
-- composition  _ = decomposeSw
 testBench _ = tests
 dup_id_exceptions _ = sal_id_exceptions
 encoding _ = sw_encodings
 termParser _ ts e = add_id ts e

add_id :: [Abs.Term] -> Entry -> Entry
add_id [Abs.TermC _ [(Abs.TermA (Abs.NStr s))]]  e = set_lemma_id s e
-}


--- 
io :: IO a -> Convert a
io = lift 

report :: String -> Convert ()
report x = tell [x] 
