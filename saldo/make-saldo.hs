import Prelude hiding (lex)
import System.IO
import System.Cmd
import System.Exit
import System.Directory
import Data.Char
import Data.Maybe
import Data.List
import Data.IORef
import qualified Data.Map as Map
import Text.Read.Lex(Lexeme(..),lex)
import Text.ParserCombinators.ReadP
import Debug.Trace

import qualified PGF as PGF

import UTF8
import CommandsSw
import Frontend
import TestSw
import Dictionary(Entry,set_lemma_id,get_lemma_id,unDict)
import qualified Dict.Abs as Abs
import EncodeSw
import Dict.ErrM(Err(..))
import DictToDictionary
import General(Str(..))

{-
 ghci make-saldo.hs -ifm/sblex/src/lib/ -ifm/sblex/src/Dict/ -ifm/sblex/src/saldo/
-}

main = do
  hSetBuffering stdout NoBuffering
  putStr "Reading lillsaldo.xml ... "
  res   <- parseDict SAL "lillsaldo.xml" (False,False,False)
  saldo <- case res of
             Ok (dict,_) -> return $ Map.fromList [(mkGFName (get_lemma_id entry), entry) | entry <- unDict dict]
             Bad msg     -> fail msg
  putStrLn ""

  cnc_file <- createGF saldo
  tmp_saldo <- compileGF
  loop tmp_saldo saldo cnc_file
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
  putStr "Creating GF source for SALDO ... "
  print [(inft) | (id,lemma,_,cat,_, inft,_) <- Map.elems saldo, lemma == "betyg"]
  let cnc_file = [(id,gf_cat,[[lemma]],"",paradigms) | (id,lemma,_,cat,_, inft,_) <- Map.elems saldo
                                                     , (fm_cat,gf_cat,_,paradigms) <- catMap, fm_cat == cat
                                                     , null [x | x@(_,(_,Str [])) <- inft]]
  printGF cnc_file
  putStrLn ""
  return cnc_file


-------------------------------------------------------------------
-- Compare the current GF version with SALDO
-------------------------------------------------------------------

updateGF tmp_saldo saldo cnc_file = do
  putStr ("Reading "++tmp_saldo++" ... ")
  pgf <- PGF.readPGF tmp_saldo
  putStrLn ""

  putStr "Updating GF source for SALDO ... "
  count <- newIORef 0
  cnc_file <- mapM (check count saldo pgf) cnc_file
  printGF cnc_file
  putStrLn ""
  c <- readIORef count
  putStrLn ("updated "++show c++" words")
  return (cnc_file,c)

check count fm gf entry@(id,cat,lemmas,_,paradigms) = do
  let fm_t = case Map.lookup (mkGFName id) fm of
               Just entry -> let (_, _, _, _, _,inft,_) = entry in inft
               Nothing    -> error ("unknown id in SALDO: " ++ id)
      gf_t = undefined (\a -> (fst $ head $ head a, map snd)) $  PGF.tabularLinearizes gf (read "saldoCnc") (read (mkGFName id))
      paramMap = head [map | (_,gf_cat,map,_) <- catMap, gf_cat == cat]
  checkForms count paramMap fm_t gf_t entry

checkForms count paramMap fm_t gf_t entry@(id,cat,lemmas,_,paradigms)
  | null diffs = return entry
  | otherwise  = do c <- readIORef count
                    writeIORef count $! c+1
                    return $! getNextLemma entry
  where
    diffs = [(fm_p,fm_v,gf_p,gf_v) | (fm_p,gf_p) <- paramMap
                                   , Just (_,Str fm_v) <- [lookup fm_p fm_t], Just gf_v <- [lookup gf_p gf_t]
                                   , null (intersect fm_v gf_v)]

    getNextLemma (id,cat,lemmas,_,[]         ) = error ("No more paradigms to choose from: "++id++show fm_t)
    getNextLemma (id,cat,lemmas,_,(_,xs,a):ps) = (id,cat,map getLemma xs,a,ps)
      where
        getLemma gf_p = case lookup fm_p fm_t of
                          Just (_,Str fm_v) -> fm_v
                          _                 -> error "variant?"
          where
            fm_p = head [fm_p | (fm_p,gf_p') <- paramMap, gf_p'==gf_p]

-------------------------------------------------------------------
-- Compile with GF
-------------------------------------------------------------------

compileGF = do
  putStrLn "Compile saldoCnc.gf ... "
  res <- rawSystem "gf" ["--batch", "--make", "-parser=off", "saldoCnc.gf", "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> putStrLn ""
    ExitFailure code -> fail ("failed with error code "++show code)
  (fpath,h) <- openTempFile "." "tmp.pgf"
  hClose h
  copyFile "saldo.pgf" fpath
  return fpath


-------------------------------------------------------------------
-- Generate GF identifier
-------------------------------------------------------------------

-- why all the '?'s ?
mkGFName id' = name
  where
       dash2us '-' = '_'
       dash2us x = x
       num x = if isDigit (head x) then 'x':x else x
       name =  num $ dropWhile (== '_') $ transform_letters $ map dash2us $ dropWhile (== '_') $ undot (decodeUTF8 id')
       transform_letters = concat . map trans
       trans '\229' = "aa"
       trans '\197' = "AA"
       trans '\228' = "ae"
       trans '\196' = "AE"
       trans '\224' = "a"
       trans '\225' = "a"
       trans '\231' = "s"
       trans '\232' = "e"
       trans '\233' = "e"
       trans '\234' = "e"
       trans '?'    = "c"
       trans '\252' = "u"
       trans '?'    = "oe"
       trans '\241' = "n"       
       trans '?'    = "OE"
       trans '?'    = "ae"
       trans '?'    = "AE"
       trans '\246' = "oe"
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
  [ ("ab", "Adv", advParamMap,  advParadigmList )
  , ("av", "A",   adjParamMap,  adjParadigmList )
  , ("vb", "V",   verbParamMap, verbParadigmList)
  , ("nn", "N",   nounParamMap, nounParadigmList)
  ]

advParamMap =
  [("invar", "")]

advParadigmList =
  [("mkAdv", [""], "")
  ]

adjParamMap =
  [("pos indef sg u nom",    "AF(APosit(StrongSgUtr))Nom "  )
  ,("pos indef sg u gen",    "AF(APosit(StrongSgUtr))Gen "  )
  ,("pos indef sg n nom",    "AF(APosit(StrongSgNeutr))Nom ")
  ,("pos indef sg n gen",    "AF(APosit(StrongSgNeutr))Gen ")
  ,("pos indef pl nom",      "AF(APosit(StrongPlg))Nom "    )
  ,("pos indef pl gen",      "AF(APosit(StrongPlg))Gen "    )
  ,("pos def sg no_masc nom","AF(APosit(WeakSg))Nom "       )
  ,("pos def sg no_masc gen","AF(APosit(WeakSg))Gen "       )
  ,("pos def pl nom",        "AF(APosit(WeakPl))Nom "       )
  ,("pos def pl gen",        "AF(APosit(WeakPl))Gen "       )
  ,("komp nom",              "AFAComparNom "                )
  ,("komp gen",              "AFAComparGen "                )
  ,("super indef nom",       "AF(ASuperlSupStrong)Nom "     )
  ,("super indef gen",       "AF(ASuperlSupStrong)Gen "     )
  ,("super def no_masc nom", "AF(ASuperlSupWeak)Nom "       )
  ,("super def no_masc gen", "AF(ASuperlSupWeak)Gen "       )
  ]

adjParadigmList =
  [ ("mkA", ["AF(APosit(StrongSgUtr))Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom ", "AF(APosit(StrongSgNeutr))Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom ", "AFAComparNom ", "AF(ASuperlSupStrong)Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom ", "AF(APosit(StrongSgNeutr))Nom ", "AF(APosit(StrongPlg))Nom ", "AFAComparNom ", "AF(ASuperlSupStrong)Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom ", "AF(APosit(StrongSgNeutr))Nom ", "AF(APosit(StrongPlg))Nom ", "AFAComparNom ", "AF(ASuperlSupStrong)Nom "], "")
  , ("mkA", ["AF(APosit(StrongSgUtr))Nom ", "AF(APosit(StrongSgNeutr))Nom ", "AF(APosit(WeakSg))Nom ", "AF(APosit(StrongPlg))Nom ", "AFAComparNom ", "AF(ASuperlSupStrong)Nom ", "AF(ASuperlSupWeak)Nom "], "")
  ]

verbParamMap =
  [("pres ind aktiv",               "VF(VPresAct) ")
  ,("pres ind s-form",              "VF(VPresPass) ")
  ,("pret ind aktiv",               "VF(VPretAct) ")
  ,("pret ind s-form",              "VF(VPretPass) ")
  ,("imper",                        "VF(VImperAct) ")
  ,("inf aktiv",                    "VI(VInfinAct) ")
  ,("inf s-form",                   "VI(VInfinPass) ")
  ,("sup aktiv",                    "VI(VSupinAct) ")
  ,("sup s-form",                   "VI(VSupinPass) ")
  ,("pret_part indef sg u nom",     "VI(VPtPret(StrongSgUtr)Nom) ")
  ,("pret_part indef sg u gen",     "VI(VPtPret(StrongSgUtr)Gen) ")
  ,("pret_part indef sg n nom",     "VI(VPtPret(StrongSgNeutr)Nom) ")
  ,("pret_part indef sg n gen",     "VI(VPtPret(StrongSgNeutr)Gen) ")
  ,("pret_part indef pl nom",       "VI(VPtPret(StrongPlg)Nom) ")
  ,("pret_part indef pl gen",       "VI(VPtPret(StrongPlg)Gen) ")
  ,("pret_part def sg no_masc nom", "VI(VPtPret(WeakSg)Nom) ")
  ,("pret_part def sg no_masc gen", "VI(VPtPret(WeakSg)Gen) ")
  ,("pret_part def pl nom",         "VI(VPtPret(WeakPl)Nom) ")
  ,("pret_part def pl gen",         "VI(VPtPret(WeakPl)Gen) ")
  ]

verbParadigmList =
  [ ("mkV", ["VF(VPresAct) "], "")
  , ("mkV", ["VI(VInfinAct) ", "VF(VPretAct) ", "VI(VSupinAct) "], "")
  , ("mkV", ["VI(VInfinAct) ", "VF(VPresAct) ", "VF(VImperAct) ", "VF(VPretAct) ", "VI(VSupinAct) ", "VI(VPtPret(StrongSgUtr)Nom) "], "")
  ]

nounParamMap =
  [ ("sg indef nom", "Sg Indef Nom ")
  , ("sg indef gen", "Sg Indef Gen ")
  , ("sg def nom",   "Sg Def Nom ")
  , ("sg def gen",   "Sg Def Gen ")
  , ("pl indef nom", "Pl Indef Nom ")
  , ("pl indef gen", "Pl Indef Gen ")
  , ("pl def nom",   "Pl Def Nom ")
  , ("pl def gen",   "Pl Def Gen ")
  ]

nounParadigmList =
  [ ("mkN", ["Sg Indef Nom "], "")
  , ("mkN", ["Sg Indef Nom "], "utrum")
  , ("mkN", ["Sg Indef Nom "], "neutrum")
  , ("mkN", ["Sg Indef Nom ", "Pl Indef Nom "], "")
  , ("mkN", ["Sg Indef Nom ", "Sg Def Nom ", "Pl Indef Nom ", "Pl Def Nom "], "")
  ]


-------------------------------------------------------------------
-- Read GF code
-------------------------------------------------------------------

readGF = do
  ls <- fmap (init . drop 7 . lines) $ readFile "saldoCnc.gf"
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

-------------------------------------------------------------------
-- Dump GF code
-------------------------------------------------------------------

printGF entries = do
  writeFile "saldo.gf" $
     ("abstract saldo = Cat ** {\n"++
      "\n"++
      "fun\n" ++ 
      concatMap showAbs entries ++
      "}")
  writeFile "saldoCnc.gf" $
     ("--# -path=.:resource/swedish:resource/scandinavian\n" ++
      "concrete saldoCnc of saldo = CatSwe ** open ParadigmsSwe in {\n"++
      "\n"++
      "flags\n"++
      "  optimize=values ; coding=utf8 ;\n"++
      "\n"++
      "lin\n" ++
      concatMap showCnc entries ++
      "}")
  where
    showAbs (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id ++ " : " ++ cat ++ " ;\n"
    showCnc (id,cat,lemmas,a,paradigms) = "  " ++ mkGFName id ++ " = " ++ "mk" ++ cat ++ " " ++
                                               unwords [case lemma_v of {[]->"(variants {})"; (x:xs) -> "\"" ++ x ++ "\""} | lemma_v <- lemmas] ++
                                               (if null a then "" else " "++a) ++ " ;\n"


-------------------------------------------------------------------
-- FM related stuff
-------------------------------------------------------------------

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
