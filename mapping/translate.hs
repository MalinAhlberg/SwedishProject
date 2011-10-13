{-# LANGUAGE TupleSections, TypeOperators, TemplateHaskell #-}
module Translate where
import MonadSP
import Idents
import Test  
import qualified Format as Form

import PGF hiding (Tree,parse)
import Control.Arrow
import Control.Monad
import Control.Monad.RWS hiding (put,gets,get)
import System.IO
import System.Process
import System.FilePath
import Data.Maybe
import Data.List
import Data.IORef
import Data.Char
import Data.Tree
import Data.Label 
import Data.Label.PureM 
import GraphTree

-- Test by runnig mainTest. Use testGr, otherwise very slow

data SentenceType = Q | D | F
  deriving (Show,Eq)
data VPForm  = Cop | Sup | VV | VA 
             | V | V2 | V2A | V2Pass 
             | Fut | Fut' -- Fut : ska
             | VS         -- Fut': kommer att                  
                                            
  deriving (Eq,Show)
data S = S { _isReflGenVP  :: Bool
           , _isExist      :: Bool
           , _iquant       :: Bool
           , _passive      :: Bool
           , _sentenceType :: SentenceType
           , _complement   :: (VPForm,[Maybe Expr])
           , _object       :: Maybe Expr
           }

type PMonad = (RWS () [String] S)

$(mkLabels [''S])

test = False
usePGF = testGr
testGr = ("../gf/BigTest.pgf","BigTestSwe")
bigGr  = ("../gf/Big.pgf","BigSwe")
lang   = fromJust $ readLanguage "BigTestSwe"
paint  = True

startState :: S 
startState = S {_isReflGenVP = False, _isExist = False
               ,_passive = False
               ,_iquant = False, _complement = (V,[])
               , _sentenceType = D, _object = Nothing}


mapp f    = main' f >> return ()
main      = main' "test.xml" >> return ()
bigTest   = main' "../testSuites/testShortSimpleTwo.xml" 
            >>= writeFile "mappingShort6.txt" . unlines
evaluation = evaluations "EvalMappSuite2.xml" "Evalresult.txt" 
evaluations test to = 
    main' test 
    >>= writeFile to . unlines 
main2      = main' "test2.xml" >> return ()
mainTest   = main' "testSimple.xml" >>= putStrLn . compareRes
mainT2     = main' "testSimple.xml" >>= putStrLn . unlines
main' fil  = do
  pgf <- readPGF $ fst usePGF
  let Just language = readLanguage $ snd usePGF
      morpho        = buildMorpho pgf language
  s <- fmap concat $ Form.parse fil
  print $ prune $ snd $ head s
  ref <- newIORef (0,0,0)
  mapM (process pgf morpho ref) {-(if test then take 15 else id)-} s
  where
    process pgf morpho ref (id,t) = do
      (cn,co,l) <- readIORef ref
      let idN =  takeWhile (/='_') id 
      putStrLn idN
      let (e,trace) = evalRWS (parse penn pgf morpho (prune t)) () startState
          (cn',co') = count (cn,co) e
          l'        = l+1
      writeIORef ref (cn',co',l')
      when test $ putStrLn $ unlines trace
      hPutStrLn stdout (showExpr [] e)
      when paint $ do
        writeFile "tmp_tree.dot" --(graphvizAbstractTree pgf (True,False) e)
                                   (graphvizParseTree pgf lang e) 
        rawSystem "dot" ["-Tpdf", "tmp_tree.dot"
                        , "-otrees/tree"++showAlign l'++"GFparsX.pdf"]
                        --, "-o"++dropExtension fil++"GF.pdf"]
        return ()
      hPutStrLn stderr (show ((fromIntegral cn' / fromIntegral co') * 100))
      --return (showExpr [] e)
      return (idN++"\t"++showExpr [] e)

    count (cn,co) e = cn `seq` co `seq`
      case unApp e of
        Just (f,es) -> if f == meta
                         then foldl' count (cn,  co+1) es
                         else foldl' count (cn+1,co+1) es
        Nothing     -> (cn+1,co+1)


    
    prune (Node tag ts)
      |   tag == "ROOT" 
       && not (null ts)
       && last ts == Node "." [Node "." []] = Node tag (init ts)
      | otherwise                           = Node tag ts

showAlign n =
      replicate (5 - length s) '0' ++ s
      where
        s = show n


testa  str = do
  pgf <- readPGF "../gf/BigTest.pgf"
  let Just language = readLanguage "BigTestSwe"
      morpho        = buildMorpho pgf language
  return [(lemma,an,cat) | (lemma,an) <- lookupMorpho morpho str
                   ,let cat = maybe "" (showType []) (functionType pgf lemma)]

paintTree file = do
  s <- fmap concat $ Form.parse file 
  pgf <- readPGF $ fst usePGF
  mapM_ paintIt $ zip [0..] $ map snd s 
 where 
   paintIt (i,t) = do
          writeFile "tmp_treetest.dot" (dotTree t [])
          rawSystem "dot" ["-Tpdf", "tmp_treetest.dot", "-otrees/"++showAlign i++"testtree.pdf"]
          return ()



penn :: Grammar (RWS () [String] S) String Expr
penn =
  grammar (mkApp meta)
   ["ROOT" :-> do s <- inside "MS" $ cat "S"
                                     `mplus` cat "XP"
                  write "root found"
                  return s 
   
     ,"S" :-> do write "start" 
                 conj     <- maybeParse $ inside "++" pPConj
                 write ("conj: "++show conj)
                 (s,s2)   <- pS
                 m_voc <- maybeParse (do opt (word2 "IK") ""
                                         inside "TT" pNP)
                 opt (word2 "IP" `mplus`
                      word2 "I?" `mplus`
                      word2 "IG" `mplus`
                      word2 "IU") ""
                 let pconj = fromMaybe (mkExpr cidNoPConj) conj
                     voc   = maybe (mkExpr cidNoVoc) fst m_voc
                 return $ mkApp cidPhrUtt [pconj, s,voc]

     ,"AP" :-> do write "in AP"
                  ad <- inside "AA" pAdA
                  write ("found adA: "++show ad)
                  a  <- inside "HD" pAdj
                  write ("found adj: "++show a)
                  return $ mkApp cidAdAP [ad,a]
               `mplus`
               do as <- many $ inside "AA" pAdAdj
                  a2 <- inside "HD" pAdj 
                  return (foldr (\ada ap -> mkApp cidAdAP [ada,ap]) a2 as)
        {-
               `mplus`
                do pp <- cat "PP"
                   a  <- pAdj
                   return undefined
                `mplus`
                do avp <- cat "AVP"
                   a   <- pAdj
                   return undefined -- det kan anses vara så gott som slut 6088
-}
      ,"AVP" :-> --bland annat, just nu, t ex, i kontakt... 
                 do iadv <- inside "HD" pIAdv
                    adv  <- pAdv
                    return $ mkApp cidAdvIAdv [iadv, adv]
                 `mplus`
                 do consume
                    return (mkExpr meta)
     -- ,"CAVP" :-> coordinated AVP 
      ,"CAP" :-> conjunct cidConsAP cidBaseAP cidConjAP pAdj

      ,"NP" :-> pflatNP
                   
      ,"PP" :-> do pr     <- write "PP!" >> inside "PR" pPrep
                   write "prep found"
                   np <- pflatNP `mplus` inside "HD" (liftM fst pNP)
                   write "prep noun found"
                   returnApp cidPrepNP [pr,np]
      ,"VP" :-> do write "in cat VP"
                   word2 "IM"                
                   (tmp,s,pol,v) <- pVP "IV"
                   write $ "VP returns " ++ show v
                   return v 

   -- weirds
      ,"XX" :-> do n     <- maybeParse pNP    
                   let e = maybe (mkExpr meta) fst n  
                   write ("xx returns "++show e)
                   return $ mkApp meta [e]
      ,"XP" :-> do write "xp!" 
                   x <- cat "XX"
                   write "xp found noun"
                   a <- pAdv     
                   write "xp found adv "
                   opt (word2 "IP") ""
                   return $ mkApp meta [x,a]

-- not tested
      ,"CNP" :-> conjunct cidConsNP cidBaseNP cidConjNP pflatNP 
                 -- may have to add cat "NP"  
      ,"CPP" :-> conjunct cidConsAdv cidBaseAdv cidConjAdv (cat "PP")
      ,"CONJP" :-> conjunct meta meta meta (pflatNP `mplus` cat "PP" `mplus` pAdj)
                -- more cases? 
      ,"CVP" :-> conjunct cidConsVPS cidBaseVPS cidConjVPS (cat "VP")
      ,"CS" :-> conjunct cidConsS cidBaseS cidConjS (cat "S")
      ,"CXP" :-> conjunct meta meta meta (cat "XP")  -- coordinated XP
      --,"NAC" :-> (consume >> return (mkExpr meta))

      -- labels ----
      ,"++" :-> pPConj
      ,"+A" :-> pPredet
      ,"+F" :-> cat "S"
      ,"AA" :-> pAA
      ,"AG" :-> pSpecialPP cidBy8agent_Prep
      ,"AN" :-> pAppos
      ,"AT" :-> pAdj
      ,"CA" :-> pPredet
      --,"DB"  :(
      ,"DT" :-> pQuant `mplus` pIQuant `mplus` pPredet 
                       `mplus` liftM fst3 pN2
      ,"EF" :-> liftM fst3 parseRelS
      ,"EO" :-> cat "VP"
      ,"ES" :-> liftM fst pNP
   --   ,"ET" :-> cat "PP"
      ,"FO" :-> pItPron 
      ,"FS" :-> liftM fst pFS
      ,"FV" :-> liftM fst (msum $ map (`pSlashVP` "FV") vForms)
      -- punctuation: I?,"IC","ID","IG","IK","IM", "IO", "IP", "IQ", "IR", "IS", "IT", "IU",
      -- punctuation: , "JC", "JG", "JR", "JT",
      ,"IV" :-> liftM fst (msum $ map (`pSlashVP` "IV") vForms)
      ,"KA" :-> cat "S"
      ,"MA" :-> inAdv
      ,"MD" :-> cat "NP" `mplus` cat "PP"
--      ,"MS" :-> cat "S"
      ,"NA" :-> return (mkExpr cidPNeg)
      ,"OA" :-> cat "PP" `mplus` cat "VP"
      ,"OO" :-> cat "S"  `mplus` cat "VP" 
                         `mplus` pAdj
                         `mplus` liftM fst pNP
      ,"PL" :-> pPart "V"  -- could be all sorts of verbs
      ,"PR" :-> pPrep
--     --,"PT"  cant parse 'sjálv'
      ,"RA" :-> inAdv
      ,"SP" :-> do a <- pAdj         --undersok om fler sp kan ha denna
                   write ("adj return"++show a)
                   returnApp cidCompAP [a]
                `mplus`
                do (e,_) <- pNP
                   write ("coplua np "++show e)
                   returnApp cidCompNP [e]
                `mplus`
                do e <- cat "PP"
                   returnApp cidCompAdv [e]
                `mplus`
                do consume
                   return (mkExpr meta) --we know we are in SP, so ok to consume
      ,"SS" :-> liftM fst pNP
      --,"ST"  paragraph
      ,"TA" :-> inAdv
      ,"TT" :-> liftM fst pNP
      ,"UK" :-> pConj `mplus` pSubj
      ,"VA" :-> inAdv
      ,"VO" :-> cat "VP"
      ,"VS" :-> cat "VP"
      ,"XA" :-> cat "PP" -- sa att saga
      --,"XF" :-> XP
      --,"XT" -- sa kallad
      -- ,"XX" unclassifiable 
     --,"YY" :-> inside "YY" (lemma "ja,jo"  "") --fix!!
     ,"CJ" :-> cat "S" `mplus` cat "PP" `mplus` cat "VP"
                       `mplus` pAdj     `mplus` pflatNP
     ,"HD" :-> liftM fst3 pCN `mplus` pAdj `mplus` pIAdv `mplus` liftM fst pNP
   ] 


clType typ | typ==cidQuestVP = cidUseQCl
           | otherwise       = cidUseCl
utType typ | typ==cidQuestVP = cidUttQS
           | typ==cidUttQS   = cidUttQS
           | otherwise       = cidUttS

parseSCl = inside "S" pCl

pS = do
  cl <- do (cl,_,_,utt) <- pCl 
           write "found cl"
           write $ "utttype: " ++ show utt
           return $ mkApp (utType utt) [cl]
        `mplus`
        do write "to imperative"
           pImp 
        `mplus`
        do write "to npClause"
           pNPCl
        `mplus`
        do write "to SS"
           pSS
        `mplus`
     --   pRelS
     --   `mplus`
        do cl <- pUttAdv
           return $ mkApp cidUttAdv [cl]
  s2 <- maybeParse $ inside "+F" (optEat (cat "S") (mkExpr meta))
  let cl1   = maybe cl (\x -> mkApp meta [x]) s2 
  return (cl1,s2)


pNPCl = do 
 (np,typ) <- parseSubject
 guard $ typ /= cidImpersCl
 return $ mkApp cidUttNP [np]


pCl = 
  do write "looking for SS" 
     (np,typ) <- parseSubject
     write ("SS done "++show np)
     --advs <- many $ cat "OA"
     write "now to pVP" 
     (tmp,sim,pol,vp) <- (write "goto pVP" >> pVP "FV")
                               `mplus`
                               (write "no VP!" >> inside "FV" consume >> metaVP) -- obs! för passiv
    -- guard (vptyp/=cidExistNP || typ==cidExistNP)
     advs <- many pAdv
     let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
         e1 = constructCl typ np e0
         e2 = mkApp (clType typ) [fromMaybe (mkExpr meta) (isVTense tmp)
                             ,mkExpr pol,e1]
     return (e2,tmp,pol,utType typ)
  `mplus`
 -- AdvS; 'dessutom gjorde jag så'
 -- find out whether it should be AdvS or FocAdv here!
  do qadv <- maybeParse pIAdv
     advs <- many pAdv    
     (tmp,pol,np,nptyp,vp) <- pVSOs
     advs2 <- many pAdv
     let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs2
         cl = constructCl nptyp np e0
         t  = maybe cidUttS (const cidQuestVP) qadv
         c  = maybe cl (\q -> mkApp cidQuestIAdv [q,cl]) qadv  
         e1 = mkApp (clType t) [fromMaybe (mkExpr meta) (isVTense tmp)
                             ,mkExpr pol
                             ,c
                             ]
               -- if pAdv:s is found after pIAdv, there will be a problem..
         --(cl1,t) = maybe (e1,cidUttS) (\q -> (mkApp cidQuestIAdv [q,cl],cidQuestVP)) qadv  
         e2 = foldr (\ad e -> mkApp cidAdvS [ad, e]) e1 advs
     return (e2,tmp,pol,t)
  `mplus`
  --- question; 'vem är det'   
  pObjectFoc Q
  `mplus`
  --- Focused sentence
  pObjectFoc F
 
pObjectFoc sTyp = do
  sentenceType =: sTyp
  (tmp,pol,ip,qcl,qtyp) <- msum $ map (pOVS sTyp) vForms
  iq <- gets iquant
  guard $ sTyp/=Q || iq 
  write $ "focus found clause: "++show qcl
  write $ "foucs found ip: "++show ip
  let predVP = if sTyp==Q then cidQuestVP else cidPredVP 
      cl = mkApp qtyp [ip,qcl] -- how to deal with 'man'
      e1 = mkApp (clType predVP) [fromMaybe (mkExpr meta) (isVTense tmp)
                               ,mkExpr pol
                               ,cl ]
  return (e1,tmp,pol,predVP)


pSS =  
  do s1   <- cat "S"    -- jag går om hon kommer
     conj <- inside "UK" pConj
     s2   <- inside "S" pUttAdv 
     return $ mkApp cidSSubjS [s1,conj,s2] 


-- pComplVP isn't neeeded? we need tmp from pSlashVP, pol, ...
-- if   Cop then use QuestIComp : ip -> np -> qcl
-- else     then use SlashVP : sub_np -> slashvp -> clSlash
--                   QuestSlash : ip -> clSlash -> qcl
pOVS styp Cop = do
  write "try OVS copula"
  (pol,args,[]) <- pCompl Cop 
  obj <- case args of 
             [Nothing,Just sp] -> return sp
             xs                -> write ("pOVS cop fail"++show xs)
                                   >> mzero
  write $ "found compl in OVS Cop: "++show obj
  (v,t) <- pSlashVP Cop "FV"
  sentenceType =: D
  advs <- many pAdv    
  (np,nptyp) <- write "looking for SS in OVS" >> parseSubject
  advs1 <- many pAdv    
  let f   = if styp==Q then cidQuestIComp else meta
      np1 = foldr (\ad e -> mkApp cidAdvNP [ad, e]) np (advs++advs1)
  return (mkTmp t,pol,obj,np1,f)


pOVS styp typ = do
  write "try OVS"
  (pol,exps,b) <- pCompl typ
  complement =: (typ,exps)
  write $ "found compl in OVS "++show typ
  (tmp,s,pol,vp,np,advs) <- msum $ map (pSlashedVP pol b) vForms
  o <- gets object
  advs1 <- many pAdv    
  let qtyp = if styp==Q then cidQuestSlash else cidFocObj -- could be StrandQuest (vem tittar du på) -- or maybe ?QuestIAdv (på vilken katt sitter hon)
      obj  = fromMaybe (mkExpr meta) o   --cannot handle 'sig ser han'
      cl0  = mkApp cidSlashVP [np,vp]
      cl   = foldr (\ad e -> mkApp cidAdvSlash [ad, e]) cl0 (advs++advs1)
  return (tmp,pol,obj,cl,qtyp) 


pSlashedVP pol b typ = do
  (v,t) <- pSlashVP typ "FV"
  write "slashed found verb"
  sentenceType =: D
  advs <- many pAdv    
  (np,nptyp) <- write "looking for SS in pSlashedVP" >> parseSubject
  sentenceType =: Q
  (tmp,s,pol',vp) <- pComplVP typ Q v t (pol,[],b) 
  sentenceType =: D
  return (tmp,s,pol',vp,np,advs)
  
pVSOs = msum $ map pVSO vForms

pVSO typ = do
  (v,t) <- pSlashVP typ "FV"
           --`mplus`                                 -- we need to deal with unknowns somewhere
           --inside "FV" (consume >> metaVP' typ)    -- slash should find them if the right tag is there!
  (np,nptyp) <- write "looking for SS" >> parseSubject
  write ("AdvCl found np "++show np)
  exps <- pCompl typ
  (tmp,s,pol,vp) <- pComplVP typ D v t exps
  write ("AdvCl found compl "++show vp)
  return (tmp,pol,np,nptyp,vp)


pImp = do write "in imperative"
          (tmp,sim,pol,vp) <- pVP "FV"
          write "found vp in imp"
          guard (tmp==VImp)
          write "vp in imp is ok"
          advs <- many pAdv
          write ("advs found: "++show advs)
          let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
              imp = mkApp cidImpVP [e0] 
          return $ mkApp cidUttImpPol [mkExpr pol,imp]

-- "att det inte regnar"
pUttAdv = do 
 sub <- inside "UK" pSubj 
 (np,typ) <- parseSubject                    
 write ("SS done for UttAdv"++show np)
 write "now to pVP"
 pol <- pPol
 (tmp,sim,p,vp) <- (write "goto pVP" >> pVP "FV")
                     `mplus` (write "no VP!" >> inside "FV" consume >> metaVP) -- obs! för passiv
 guard (p==cidPPos)
 advs <- many pAdv
 let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
     e1 = constructCl typ np e0
     e2 = mkApp cidUseCl [fromMaybe (mkExpr meta) (isVTense tmp)
                         ,mkExpr pol,e1]
 return $ mkApp cidSubjS [sub,e2]

parseRelS = do 
  w <- inside "S" $ inside "SS" $ word "PO" 
  guard (w =="Som" || w == "som")
  pol <- pPol
  (tmp,sim,p,vp) <- write "goto pVP" >> pVP "FV"
  let t = fromMaybe (mkExpr meta) (isVTense tmp)
  return (vp,t,mkExpr pol) 
 
constructCl typ np vp = 
  if typ == cidGenericCl || typ == cidImpersCl
     then mkApp typ [vp]
     else mkApp typ [np,vp]

pSpecialPP cid = 
 do pr <- inside "PR" $ optEat (lemma "Prep" "s") meta
    write "prep found"
    guard (pr==cid)
    np     <- pflatNP
    return $ mkApp cidPrepNP [mkExpr pr,np]

-- som ...
--pRelS = undefined


data VForm a
  = VInf | VPart | VSupin | VImp | VTense a
   deriving (Show,Eq)

instance Functor VForm where
  fmap f VInf       = VInf
  fmap f VPart      = VPart
  fmap f VSupin     = VSupin
  fmap f VImp       = VImp
  fmap f (VTense t) = VTense (f t)

isVInf VInf = True
isVInf _    = False

isVPart VPart = True
isVPart _     = False

isVSupin VSupin = True
isVSupin _       = False

isVTense (VTense t) = Just t
isVTense _          = Nothing

isVTenseForm a (VTense t) = t == a 
isVTenseForm _ _          = False


vForms     = [Cop,Sup,Fut,Fut',VV,VA,V2A,V2,V2Pass,VS,V]
gfvForms  = ["VV","VA","V2A","V2","VS","V","V3"] -- need more?

pSlashVP V typ =
 do (t,v) <-inside typ $ pVerb "VV" "V"
                         `mplus`
                         liftM (,cidUseCopula) pCopula
             --            `mplus`
             --            inside "VV" (consume >> return metaVerb)
    return (mkExpr v,t)

pSlashVP VV typ =
 do (t,v) <- inside typ pVV
    return (v,t)

pSlashVP V2 typ =
 do (t,v) <- inside typ pV2Act
             `mplus`
             inside typ (inside "VV" pExist)
    return (v,t)

pSlashVP V2A typ =
 do (t,v) <- inside typ $ pVerb "VV" "V2A"
    return (mkExpr v,t)


pSlashVP V2Pass typ =
 do (t,v) <- inside typ pV2Pass
    return (v,t)

pSlashVP Cop typ =
 do t <- inside typ pCopula 
    return (mkExpr meta,t)

pSlashVP Sup typ =
 do t <- inside typ pHave
    return (mkExpr meta,t)


pSlashVP VA typ =
 do (t,v) <- inside typ pVA
    return (v,t)
 
pSlashVP Fut typ = 
 do t <- inside typ pFuturum
    write "found future form"
    return (mkExpr meta,t)

pSlashVP Fut' typ = 
 do t <- inside typ pFuturum'
    return (mkExpr meta,t)
pSlashVP VS typ = 
 do (t,v) <- inside typ $ pVerb "VV" "VS"
    return (mkExpr v,t)

mkTmp = mkTmp' cidASimul 
mkTmp' a | a ==cidASimul = fmap (\t -> mkApp cidTTAnt [mkExpr t,mkExpr cidASimul]) 
         | a ==cidAAnter = fmap (\t -> mkApp cidTTAnt [mkExpr t,mkExpr cidAAnter]) 


pVP typ = msum [pVO typ x | x <- vForms]

-- adv efter verb? före eller efter pol?
pVO typ vp = 
 do write $ "try verb "++show typ
    (v,t) <- pSlashVP vp typ
    write $ "found first verb: "++show v
    q <- gets sentenceType
    write $ "sentence type is "++show q
    comp <- if q==D then pCompl vp else return (cidPPos,[],[]) --- ?
    x <- maybeParse (inside "FO" $ word "PO")  -- dummy object. what to do?
    let verb = maybe v (\x -> mkApp meta [v,mkExpr cidDummy]) x 
    write $ "PVO returns "++ show verb ++show comp
    pComplVP vp q verb t comp

pInfVP = 
  do write "att v?"
     -- to go
     im <- opt (word2 "IM" >> return True) False
     write $ "infinite marker? "++ show im
     v  <- pVP "IV"
     return (im,v)

pComplVP V q vp tmp (pol,exps,_) = do 
  comp <- getComplement V q exps 
  (adv,part,adv1) <-  case comp of
                         (a:p:a1:_) -> return (a,p,a1)
                         _          -> argErr "V"
  let vp0  = fromMaybe vp part 
      vp1  = mkApp cidUseV [vp0]
      vp2  = maybe vp1 (\a -> mkApp cidAdvVP [vp1,a]) adv 
      vp3  = maybe vp2 (\a -> mkApp cidAdvVP [vp2,a]) adv1
  write ("particle "++show part++" verb "++show vp)
--   when (isJust p) $ guard (mkExpr (fromJust p) == vp)  -- how to do this right? need lists of verbs/particles to see which fit
  return (mkTmp tmp,cidASimul,pol,vp3) 

pComplVP VA q vp tmp (pol,exps,_) = do 
  comp <- getComplement VA q exps
  (adv,a) <-  case comp of
                  (a:Just aj:_) -> return (a,aj)
                  _             -> argErr "VA"
  let vp1  = maybe vp (\a -> mkApp cidAdvVPSlash [vp,a]) adv 
      vp2  = if q==D then mkApp cidComplVA   [vp1,a]
                     else vp1
  when (q/=D) $ object =: Just a
  return (mkTmp tmp,cidASimul,pol,vp2)

pComplVP VV q vp tmp (pol,exps,bs) = do
  comp <- getComplement VV q exps
  (adv,iv,p) <-  case comp of
                  (a:Just i:p':_) -> return (a,i,p')
                  _               -> argErr "VV"
  let vv0 = if bs==[True] then mkApp cidDropAttVV [vp] else vp
      vv1 = fromMaybe vv0 p 
      vv2  = maybe vv1 (\a -> mkApp cidAdvVP [vv1,a]) adv 
--   when (isJust p) $ guard (mkExpr (fromJust p) == vp)  -- how to do this right? need lists of verbs/particles to see which fit
      vv3 = if q==D then mkApp cidComplVV [vv2,iv] else vv2
  when (q/=D) $ object =: Just iv 
  return (mkTmp tmp,cidASimul,pol,vv3)

pComplVP V2 q vp tmp (pol,exps,_) = do
  comp <- getComplement V2 q exps
  (adv,obj,part) <-  case comp of
                  (a:o:p:[]) -> return (a,o,p)  -- particles should handled them self..
                  _        -> argErr "V2"
  isRefl <- gets isReflGenVP 
  write $ "refl? : "++show isRefl
  let compl = if isRefl then cidReflSlash else cidComplSlash
      combineVP =
        case (obj,isExistNP vp) of
             (Just o,False) -> 
               let vp0 = maybe vp 
                              (\a -> mkApp cidAdvVPSlash [vp,a]) adv 
               in  mkApp compl [vp0,o]
             (Just o,True)  -> 
                    maybe o (\a -> mkApp cidAdvVPSlash [o,a]) adv
             (Nothing,False) ->    -- for exist, maybe not worth the work..
                    let vp0 = mkApp cidReflVP [vp]
                    in  maybe vp (\a -> mkApp cidAdvVPSlash [vp0,a]) adv  
      vp0 = if q/=D then vp else combineVP
  when (q/=D) $ object =: obj
  return (mkTmp tmp,cidASimul,pol,vp0)

-- translate: hur målar du huset???
pComplVP V2A q vp tmp (pol,exps,_) = do
  comp <- getComplement V2A q exps
  (adv,obj,adj) <-  case comp of
                  (a:o:Just aj:p:_) -> return (a,o,aj) -- particles should handled them selves..
                  _               -> argErr "V2A"
  let slashVP = mkApp cidSlashV2A [vp,adj]
  when (q/=D) $ object =: obj
  case obj of
    Just o  -> do
               let vp0 = maybe slashVP (\a -> mkApp cidAdvVPSlash [slashVP,a]) adv  
                   vp1 = mkApp cidComplSlash [vp0,o]
               return (mkTmp tmp,cidASimul,pol,vp1)
    Nothing -> do
               let vp0 = mkApp cidReflVP [slashVP]
                   vp1 = maybe slashVP (\a -> mkApp cidAdvVPSlash [vp0,a]) adv  
               return (mkTmp tmp,cidASimul,pol,vp1)

-- vem åts? ok
pComplVP V2Pass q vp tmp (pol,exps,_) = do
  comp <- getComplement V2Pass q exps
  (adv1,agent,eo,adv2) <-  case comp of
                  (a:g:e:a2:p_) -> return (a,g,e,a2) -- particles should handled them selves..
 
                  _            -> argErr "V2Pass"
  let vp' = foldr (\a vp -> mkApp cidAdvVP [vp,a]) vp 
                               $ catMaybes [adv1,agent,adv2]
      vp3 = maybe vp' (\a -> mkApp meta [a]) eo --- wrong! cidExistNP if verb was 'finns' 
  return  (mkTmp tmp,cidASimul,pol,vp3)

-- vad hade du ätit? ok
pComplVP Sup q vp t (pol,exps,bs) = do
  comp <- getComplement Sup q exps
  (adv,sup) <-  case comp of
                    (a:Just s:_) -> return (a,s)
                    _            -> argErr "Sup"
  pass <- gets passive
  let tmp  = fmap (\t -> mkApp cidTTAnt [mkExpr t,mkExpr cidAAnter]) t
      vp1  = maybe sup (\a -> mkApp cidAdvVPSlash [sup,a]) adv 
      useV = if bs == [True] || not pass then cidUseV else cidPassV2
  passive =: False -- reset
  return (tmp,cidAAnter,pol,mkApp useV [vp1])

-- quest ok
pComplVP Cop q vp tmp (pol,exps,_) = do
  comp <- getComplement Cop q exps
  (adv,sp) <-  case comp of
                  (a:Just s:_) -> return (a,s)
                  _            -> argErr "Cop"
  write ("copula sp "++ show sp)
  let cop = mkApp cidUseComp [sp]
      vp1  = maybe cop (\a -> mkApp cidAdvVPSlash [cop,a]) adv 
  return (mkTmp tmp,cidASimul,pol,vp1)

-- vad ska du göra? ok
pComplVP Fut q vp t (pol,exps,_) = do
  comp <- getComplement Fut q exps
  (adv,v) <-  case comp of
                  (a:Just s:_) -> return (a,s)
                  _            -> argErr "Fut"
  let vp1  = maybe v (\a -> mkApp cidAdvVPSlash [v,a]) adv 
  write ("fut compl: "++show vp1)
  return (mkTmp t,cidASimul,pol,vp1)

-- vad kommer du att göra? ok
pComplVP Fut' q vp t (pol,exps,_) = do
  comp <- getComplement Fut' q exps
  (adv,vp0) <-  case comp of
                  (a:Just s:_) -> return (a,s)
                  _           -> argErr "Fut'"
  let vp1  = maybe vp0 (\a -> mkApp cidAdvVPSlash [vp0,a]) adv 
  return (mkTmp t,cidASimul,pol,mkApp meta [vp1])
 
pComplVP VS q vp t (pol,exps,_) = do
  comp <- getComplement VS q exps
  (adv,s) <-  case comp of
                  (a:Just s:_) -> return (a,s)
                  _            -> argErr "VS"
  let vp0 = if q==D then mkApp cidComplVS [vp,s] 
                    else vp
      vp1 = maybe vp0 (\a -> mkApp cidAdvVP [vp1,a]) adv
  when (q/=D) $ object =: Just s
  return (mkTmp t,cidASimul,pol,vp1)

getComplement v q exps | v `elem` [Fut,Fut',VV,Sup]
 = do write "looking for another complement"
      if q==Q then do (_,ex,_) <- pCompl v 
                      return ex
              else return exps
getComplement v q exps =
 if q==Q then do (t,c) <- gets complement
                 guard (t==v)
                 return c
         else return exps

argErr s = do
  write ("wrong number of arguments to pCompl "++ s)
  mzero

pPart v = do
  p <- do write "part right!!"
          inside "AB" (lemma v "part")
       `mplus`
       do write "part" 
          inside "PR" (lemma v "part") -- `mplus` optEat (lemma "Prep" "s") meta)
  return (mkExpr p)
       --   `mplus`
       --   (inside "AB" $ optEat (lemma "A" "s") meta)  -- vi vet inte hur en sån ska se ut
-- (send_V3,"c3 s","V3")
-- (mother_N2,"c2 s","N2")


pVV = do
  (t,v) <- tryVerb "FV" cidGet_VV "VV"  
           `mplus`
           tryVerb "WV" cidWant_VV "VV"
           `mplus`
           do write "looking for can"
              tryVerb "QV" cidCan_VV "VV"
           `mplus`
           tryVerb "MV" cidMust_VV "VV"
           `mplus`
           pVerb "VV" "VV"
  write ("VV returs tense "++show t)
  return (t,mkExpr v)

pVA = do
  (t,v) <- tryVerb "BV" cidBecome_VA "VA"
           `mplus`
           pVerb "FV" "VA" 
  write ("VA returs tense "++show t)
  return (t,mkExpr v)


pV2Act = do 
  (t,v) <- do t <- pHave
              return (t,mkExpr cidHave_V2)  -- need to look for passive form here too
           `mplus`
           -- man skulle kunna kolla mer på taggarna här
           do (t,v) <- do write "in pV2"
                          pVerb "VV" "V2"
                       `mplus`                   
                       do write "får är i farten"
                          tryVerb "FV" cidGet_V2 "V2"
                       `mplus`
                       tryVerb "GV" cidDo_V2 "V2"
                       `mplus`
                       tryVerb "GV" cidDo_VV "VV"
                       `mplus`
                       tryVerb "BV" cidBecome_V2 "V2"
              return (t,mkExpr v)
  return (t,mkApp cidSlashV2a [v]) 

pV2Pass = do
  (t,v) <- pPassVerb "VV" "V2"
           `mplus`
           tryVerb "GV" cidDo_V2 "V2"
           `mplus`
           tryVerb "FV" cidGet_V2 "V2"
  return (t,mkApp cidPassV2 [mkExpr v])

pExist =
-- do set isExist True
    do lemma "NP -> Cl" "s Pres Simul Pos Main"
       return (VTense cidTPres,mkExpr cidExistNP)
    `mplus`
    do lemma "NP -> Cl" "s Pret Simul Pos Main"
       return (VTense cidTPast,mkExpr cidExistNP)
    `mplus`
    do lemma "NP -> Cl" "s Pres Anter Pos Main"
        `mplus`
        lemma "NP -> Cl" "s Pret Anter Pos Main"
       return (VSupin,mkExpr cidExistNP)

tryVerb tag cid cat =
 do t <- tense tag
    return (t,cid) 
 `mplus`
  do write "no tense found"
     pVerb tag cat

pVerb = pVerb' "Act"
pPassVerb = pVerb' "Pass"

pVerb' act incat cat =
        do v <- inside incat $ lemma cat $ "s (VF (VPres "++act++"))"
           return (VTense cidTPres,v)
        `mplus`
        do v <- inside incat $ lemma cat $ "s (VF (VImper "++ act++"))"
           return (VImp,v)
        `mplus`
        do v <- inside incat $ lemma cat $ "s (VI (VInfin "++ act++"))"
           return (VInf,v)
        `mplus`
        do v <- inside incat $ lemma cat $ "s (VF (VPret "++ act++"))"
           return (VTense cidTPast,v)
        `mplus`
        do v <- inside incat $ lemma cat $ "s (VI (VSupin "++ act++"))"
           return (VSupin,v)
         {-      `mplus`     --careful here!
        (inside (incat++"PS") consume >> return (VTense cidTPres,meta))
        `mplus`
        (inside (incat++"PT") consume >> return (VTense cidTPast,meta))
        `mplus`
        (inside (incat++"SN") consume >> return (VSupin,meta))
        `mplus`
        do write "could not find verb"
           inside incat consume  
           return metaVerb -}


maybeVerbAdv  = maybeParse pAdv  

maybeParticle = maybeParse . inside "PL" . pPart 

metaVP = do
  let tmp = fmap (\t -> mkApp cidTTAnt [mkExpr t,mkExpr cidASimul]) $ VTense cidTPres  
  return (tmp,cidASimul,cidPPos,mkExpr meta)

--metaVP' :: VPForm -> P S String Expr (Expr,VForm CId)
metaVP' vf = return (mkExpr meta,VTense cidTPres)

metaVerb   = (VInf,meta)

pCompl :: VPForm -> P String Expr PMonad (CId,[Maybe Expr],[Bool])
pCompl Cop = do
  write "copula compl begins"
  pol <- pPol
  adv <- maybeParse $ pAdvMinus ["RA"]
  write $ "copula found adv"++show adv
  sp <- cat "SP"
        `mplus`
        do write "copula looking for adv2"
           a <- pAdv
           write "copula found adv2"
           returnApp cidCompAdv [a]
  return (pol,[adv,Just sp],[])

pCompl Sup = do
  write "supinum compl begins"
  p <- pPol
  adv <- maybeVerbAdv
  (t',sup,useV) <- inside "IV" $ 
                    do (t,s) <- msum [pVerb "TP" v | v <- gfvForms] 
                                `mplus`
                                inside "TP" (consume >> return (VSupin,meta))
                                `mplus`
                                msum [pVerb "VVSN" v | v <- gfvForms] 
                       return (t,s,True) 
              `mplus`         
               do (t,s) <- msum [pPassVerb "VVSN" v | v <- gfvForms] 
                  return (t,s,False) 
  guard (isVSupin t') 
  passive =: not useV
  return (p,[adv,Just $ mkExpr sup],[useV])

pCompl V2 = do
  write "v2 compl begins"
  (pol,adv,part) <- pV2Compl
  obj <- do inside "OO" $ word "POXPHH"  
            return Nothing -- sig
         `mplus`
         do write "look for np in oo"
            liftM (Just .fst) (inside "OO" pNP)
         `mplus`
         do write "look for np in sp"
            liftM (Just .fst) (inside "SP" pNP)
         `mplus`
         do o <- inside "OA" (cat "PP" `mplus` cat "VP")  
            return (Just $ mkApp meta [o]) -- hard. the preposition may be part of the verb
         `mplus`  
         do det <- inside "FO" pItPron     -- funnit det attraktivt att (VP)
            a   <- pAdj
            vp  <- inside "EO" $ cat "VP" 
            return (Just $ mkApp meta [det,mkApp meta [a,vp]])  --check this. AdjNP,VerbAP??
         `mplus`  
         do inside "IO" $ word "POXPHH"  
            return Nothing -- sig
         `mplus`
         liftM (Just . fst) (inside "ES" pNP)
                                                         
  write "oo ok"
  return (pol,[adv,obj,part],[])
pCompl V2A = do
  write "v2a compl begins"
  (pol,adv,part) <- pV2Compl
  obj <- do inside "OO" $ word "POXPHH"  
            return Nothing -- sig
         `mplus`
         liftM (Just .fst) (inside "OO" pNP)
         `mplus`
         liftM (Just .fst) (inside "SP" pNP)
         `mplus`
         do o <- inside "OA" (cat "PP" `mplus` cat "VP")  
            return (Just $ mkApp meta [o]) -- hard. the preposition may be part of the verb
         `mplus`  
         do inside "IO" $ word "POXPHH"  
            return Nothing -- sig
  adj <- inside "OO" pAdj
  write "oo ok"
  return (pol,[adv,obj,Just adj,part],[])


pCompl V2Pass = do
  write "v2pass compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol  <- pPol
  adv1 <- maybeVerbAdv
  eo   <- maybeParse $ liftM fst (inside "ES" pNP)
  part <- maybeParticle "V2"   
  write ("particle: "++show part)
  ag   <- maybeParse $ inside "AG" $ pSpecialPP cidBy8agent_Prep
  adv2 <- maybeVerbAdv
  write "agent ok"
  return (pol,[adv1,ag,eo,adv2,part],[])

-- dropAtt only needed for some verbs.. More checking?
pCompl VV = do
  write "vv compl begins"
  (pol,adv,part) <- pV2Compl --part not used
  (t',p,iv,b)  <- do write "look for infinite verb"
                     (im,(t,s,p,i)) <- inside "OO" (inside "VP" pInfVP)
                                       `mplus`
                                       (write "inf2" >> pInfVP)
                     return (t,p,i,im) 
                   `mplus`
                   do write "looking for weird verb phrase complement for vv"
                      (t,s,p,i) <- inside "OO" (inside "NAC" $ pVP "IV")
                      return (t,p,i,True)
                   `mplus`
                   do write "looking for complete verb phrase complement for vv"
                      v <- inside "OA" $ cat "VP"
                      return (VInf,cidPPos,v,True)
  write ("iv found "++show iv)
  guard (t'==VInf)  
  guard (p==cidPPos)  -- you cannot say 'jag vill inte (inte tänka)'
  write "iv ok"
  p <- maybeParticle "VV"
  write ("particle: "++show p)
  return (pol,[adv,Just iv,p],[b])

pCompl VA = do
  write "va compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol   <- pPol
  adv   <- maybeVerbAdv
  a     <- inside "SP" (pAdj `mplus` cat "CNP")
  return (pol,[adv,Just a],[])

pCompl V = do
  write "v-simple compl begins"
  pol <- pPol
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  adv <- maybeVerbAdv
  p   <- maybeParticle "V"
  write ("particle: "++show p)
  adv1  <- maybeParse $ inside "OA" $ cat "PP"
  return (pol,[adv,p,adv1],[])

pCompl Fut = do
  write "futurum compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  p   <- pPol
  adv <- maybeVerbAdv
  (t',s,p',iv) <- pVP "IV"
  write ("comlpfut "++show iv)
 -- guard $ p ==cidPPos
  return (p,[adv,Just iv],[])
pCompl Fut' = do
  write "futurum compl begins 'komma att'"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  p   <- pPol
  word2 "IM"
  adv <- maybeVerbAdv
  (t',iv) <- inside "IV" $ optEat (pVerb "VV" "V") (VInf,meta) -- inte bara V 
  return (p,[adv,Just $ mkExpr iv],[])

pCompl VS = do
  write "VS compl "
  (pol,adv,part) <- pV2Compl  -- part not used
  adv       <- maybeVerbAdv
  (s,t,p,_) <- inside "OO" $
                      inside "S" $ do conj <- inside "UK" pSubj
                                      pCl
  write "s in vs ok"
  return (pol,[adv,Just s],[])


pV2Compl = do
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol <- pPol
  write "oo pol ok"
  adv <- maybeVerbAdv
  part <- maybeParticle "V2"
  write ("particle: "++show part)
  return (pol,adv,part)

maybeParse = flip opt Nothing . liftM Just  

pflatNP =
  do write "in NP with Adj"
     -- good cars
     typ <- gets sentenceType
     m_predet     <- maybeParse $ inside "+A" pPredet
                                  `mplus`
                                  inside "CA" pPredet
                                  `mplus`
                                  inside "DT" pPredet
     m_det        <- if typ==Q then iquant =: True >> liftM Just (inside "DT" pIQuant)
                               else maybeParse $ inside "DT" pQuant 
     m_sitt       <- maybeParse $ inside "DT" pDetRefl   -- sig
     m_n2         <- maybeParse $ inside "DT" pN2 -- antal
     m_a          <- maybeParse $ inside "AT" pAdj
     (noun,n,def) <- inside "HD" pCN
     m_pt         <- maybeParse $ inside "PT" consume  -- no way of parsing 'själv' yet
     et           <- many $ inside "ET" $ cat "PP"
     m_app        <- maybeParse $ inside "AN" pAppos
     m_relCl      <- maybeParse $ do opt (word2 "IK") ""
                                     inside "EF" parseRelS
     write "start putting together np"
     opt (word2 "IP") ""
     t <- gets sentenceType
     let --cn  = mkExpr noun 
         cn0 = maybe noun (\x -> mkApp meta [noun,mkExpr meta]) m_pt  -- kvinnan själv'
         cn1 = case m_a of
                   Just a  -> mkApp cidAdjCN [a,mkApp cidUseN [cn0]]
                   Nothing -> mkApp cidUseN [cn0]
         num = mkExpr n
         d   = fromMaybe (mkApp (getCId t cidDetQuant) [mkExpr cidDefArt,num]) m_det
         cn2 = maybe cn1 (\app -> mkApp cidApposCN [cn1,app]) m_app
     np0 <- case (m_sitt,def,m_det) of
                (Just (),_,_)   -> returnApp cidReflCN [num,cn2]
                (_,     NDef,_) -> returnApp cidDetCN 
                                             [d --mkApp cidDetQuant [d,num]
                                             ,cn2]
                (_,NIndef,Nothing) -> if n == cidNumSg 
                            then returnApp cidMassNP [cn2]
                            else returnApp cidDetCN 
                                            [mkApp cidDetQuant 
                                            [mkExpr cidIndefArt,num],cn2]
                (_,NIndef,Just d)  -> returnApp cidDetCN [d,cn2]
                (_,NOther,_)       -> do guard (isNothing m_predet && isNothing m_det) --ok?
                                         return noun -- $ mkExpr noun
     t <- gets sentenceType
     let np' = maybe np0 (\(n2,num,def) -> mkApp (getCId t cidDetCN)
                                                          [mkApp cidDetQuant [def,num]
                                                          ,mkApp cidComplN2 [n2,np0]]) m_n2
         np1 = maybe np' (\p -> mkApp (getCId t cidPredetNP) [p,np']) m_predet
         np2 = maybe np1 (\(vp,t,p) -> mkApp cidRelNP' [np1,vp,t,p]) m_relCl
         res = foldr (\e n -> mkApp (getCId t cidAdvNP) [n,e]) np2 et 
     write $ "will return np" ++ show res
     return res
  `mplus`
  do (noun,n,def) <- inside "HD" pCN
     guard $ def == NIndef && n == cidNumSg  -- stämmer ej för 'våningarna 8 och 9'
     num <- pNumber
     returnApp cidCNNumNP [mkApp cidUseN [noun],num]
  `mplus`
  do w1 <- inside "AA" $ word "ABFA"
     w2 <- inside "HD" $ word "POZP"
     guard (map toLower w1 == "hur" && map toLower w2 == "mycket")
     iquant =: True
     returnApp cidhow8much_IAdv []
                 

-- returns (word :: CId, number :: CId, determined :: NounForm)
pCN = 
     inside "VN" pNoun
     `mplus`
     do n <- inside "NN" (optEat pNoun metaNoun)  --optEat eller ej?
        write ("pCN gives NN "++show n) >> return n
     `mplus`
     inside "AN" pNoun
     `mplus`
     do w <- inside "POCP" consume  -- varandra, reciprokt! ej i GF
        return (mkExpr meta,cidNumPl,NOther)
     `mplus`
     do write "test for particip"
        (part,num,def) <- inside "SP" findNParticip
        return (part,num,def)
     `mplus`
     do write "test for category X"
        w <- inside "PO" (lemma "X" "s (AF (APosit (Strong GPl)) Nom)")
        return (mkApp cidXDet [mkExpr w],cidNumPl,NIndef)
     `mplus`
     do word "NNDD"
        return (mkExpr meta,cidNumSg,NDef)  --kan vara Pl också..
     `mplus`
     do write "testing last pCN"
        word "NN"
        return (mkExpr meta,cidNumSg,NIndef)
 
 
-- may use tag, "xx    GG" = genitiv
pNoun    = pNoun' "Nom"
pNounGen = pNoun' "Gen"
pNoun' nom = 
  do      n <- lemma "N" ("s Pl Indef "++nom)
          return (mkExpr n,cidNumPl,NIndef)
  `mplus` do
          n <- lemma "N" ("s Sg Indef "++nom)
          return (mkExpr n,cidNumSg,NIndef)
  `mplus` do
          n <- lemma "N" ("s Sg Def "++nom)
          return (mkExpr n,cidNumSg,NDef)
  `mplus` do
          n <- lemma "N" ("s Pl Def "++ nom)
          return (mkExpr n,cidNumPl,NDef)



metaNoun = (mkExpr meta,cidNumSg,NIndef)
data NForm = NDef | NIndef | NOther -- NOther for reciprocs etc 
  deriving (Eq,Show)

--isDef :: NForm -> Bool
--isDef NIndef = True
--isDet _      = False
getDef NDef = cidDefArt
getDef NIndef = cidIndefArt
getDef NOther = meta


parseSubject = inside "SS" (optEat pNP (mkExpr meta,cidPredVP))
               `mplus` 
               inside "FS" pFS
              
pFS =
     do w <- inside "PO" $ lemma "VP -> Cl" "s Pres Simul Pos Main"
        write "imperson hittad!!"
        return (mkExpr w,cidImpersCl)
     `mplus`
     do w <- inside "PO" $ lemma "NP -> Cl" "s Pres Simul Pos Inv"
        return (mkExpr w,cidExistNP)

pItPron = 
 do p <- inside "POOP" $ lemma "Pron" "s NPNom"
    return $ mkExpr p
 
pPN = do n <- inside "PN" $ optEat (lemma "PN" "s Nom") cidName
         return $ mkExpr n
pNP = 
  (cat "NP" >>= \x -> write ("cat np "++show x) >> return (x,cidPredVP))  --här kanske vi behöver tänka mer ang PredVP
  `mplus` 
  (cat "AP" >>= \x -> return (x,cidPredVP))  --och här med
  `mplus` 
   do write "look for name"
      name <- pPN
      return (mkApp cidUsePN [name],cidPredVP)
  `mplus` 
   do w   <- inside "PO" $ lemma "IP" "s NPNom"
      iquant =: True
      return (mkExpr w,cidQuestVP)
            {- `mplus`  Ha med detta?
             inside "POFP" $ lemma "IQuant" "s" -}
   `mplus`
   do w <- inside "POTP" $ lemma "NP" "s NPNom"
      return (mkExpr w,cidPredVP)
   `mplus`
   do
      w   <- inside "PO" $ lemma "Pron" "s NPNom" 
                        {-   `mplus`                   -- for s1001, 'mycket blir enklare'
                           lemma "Det" "s True Neutr"  -- needs change in GF
                           `mplus`
                           lemma "Det" "s True Utr"  -}
      write "lemma ok"
      return (mkApp cidUsePron [mkExpr w],cidPredVP)
   `mplus`
   do w <- inside "PO" $ lemma "VP -> Cl" "s Pres Simul Pos Main"
      write "Man hittad!!"
      return (mkExpr w,cidGenericCl)
   `mplus`
   do det <- pQuant
      return (mkApp cidDetNP [det],cidPredVP)

   `mplus`
   do np <- pflatNP
      return (np,cidPredVP)
   `mplus`
   do write "in complicated np"
      (n,num,def) <- pCN 
      let cn   = mkApp cidUseN [n]
          nums = mkExpr num
      t <- gets sentenceType
      e0 <- case def of
                 NDef -> returnApp cidDetCN 
                                   [mkApp (getCId t cidDetQuant)
                                   [mkExpr cidDefArt, nums],cn]
                 NIndef -> if num==cidNumPl then return cn
                                            else return (mkApp cidMassNP [cn])
                 NOther -> return n  -- och guards!!
      return (e0,cidPredVP)
  
-- akta optEat här!! om fler läggs till måste den flyttas ut!
pAdj = 
  do ad <- inside "AJKP" $ optEat (lemma "A" "s (AF ACompar Nom)") meta
     return $ mkApp cidUseComparA [mkExpr ad] 
  `mplus`
  {-  not supported by gf 'den är gulast'
  do ad <- inside "AJSU" $ optEat (lemma "A" "(AF (ASuperl SupStrong) Nom") meta
     return $ mkApp cidUseOrdSuperl [mkExpr ad] 
  `mplus`
  -}
  do ad <- findAdj
     return $ mkApp cidPositA [ad]
  `mplus`
  do ad <- findA2
     return $ mkApp cidUseA2 [ad]
  `mplus`
  do write "will check AP"
     cat "AP"
  `mplus`
  cat "CAP" 
  `mplus`
  do a <- inside "PO" $ lemma "X" "s (AF (APosit (Strong (GSg Utr))) Nom)"
     return $ mkApp cidXAdj [mkExpr a]
  `mplus`
  do a <- inside "TP" $ optEat findAPerfParticip meta
     return (mkApp cidVPSlashAP  [mkExpr a])
  
findAdj = 
  do ad <- inside "AJ" (optEat findA meta)
           `mplus`
           do write "looking for particip adjective"
              inside "SP" findA
     return $ mkExpr ad
 where findA =         lemma "A" adjSN 
               `mplus` lemma "A" adjSU
               `mplus` lemma "A" adjWSg
               `mplus` lemma "A" adjWPl


findA2 = 
  do ad <- inside "AJ" (lemma "A2" "s (AF (APosit (Strong (GSg Neutr))) Nom)")
           `mplus`
           inside "AJ" (lemma "A2" "s (AF (APosit (Strong (GSg Utr))) Nom)")
           `mplus`
           inside "AJ" (lemma "A2" "s (AF (APosit (Strong GPl)) Nom)")
     return $ mkExpr ad


findNParticip = pNoun 


-- only V2 at the moment
findAPerfParticip = 
 lemma "V" "s (VI (VPtPret (Strong (GSg Utr)) Nom))"
 `mplus`
 lemma "V" "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
 `mplus`
 lemma "V" "s (VI (VPtPret (Strong GPl) Nom))"
 `mplus`
 lemma "V2" "s (VI (VPtPret (Strong (GSg Utr)) Nom))"
 `mplus`
 lemma "V2" "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
 `mplus`
 lemma "V2" "s (VI (VPtPret (Strong GPl) Nom))"
 `mplus`
 lemma "VV" "s (VI (VPtPret (Strong (GSg Utr)) Nom))"
 `mplus`
 lemma "VV" "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
 `mplus`
 lemma "VV" "s (VI (VPtPret (Strong GPl) Nom))"
 `mplus`
 lemma "VS" "s (VI (VPtPret (Strong (GSg Utr)) Nom))"
 `mplus`
 lemma "VS" "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
 `mplus`
 lemma "VS" "s (VI (VPtPret (Strong GPl) Nom))"

-- akta optEat här!! om fler läggs till måste den flyttas ut!
-- om inte adjektivet finns med blir det ett adA? kanske bättre tvärtom?
pAdA = inside "AB" $ do a <- lemma "A" "s (AF (APosit (Strong (GSg Neutr))) Nom)"
                        return (mkApp cidPositAdAAdj [mkApp a[]])
                    `mplus`
                     do ada <- optEat (lemma "AdA" "s") meta
                        return (mkExpr ada)

adv = ["RA","TA","MA","+A","CA","VA"]
pAdvMinus xs = pAdv' $ adv \\ xs
pAdv = pAdv' adv 
pAdv' xs = 
  msum [ inside x inAdv | x <- xs]
  `mplus`
  do write "looking for adv in AA1"
     inside "AA" pAA
     
inAdv = findAdverb `mplus` cat "PP" `mplus` cat "NP" `mplus` cat "AVP"

pAA =         cat "PP" 
      `mplus` pAdvAdj 
      `mplus` pAdv
      `mplus` findAdverb
      `mplus` inside "S" pUttAdv
      `mplus` cat "AVP"

pIAdv =
  msum [ inside x (cat "AVP") | x <- ["RA","TA"]]
  `mplus`
  do write "making a question"
     a <- inside "AB" $ lemma "IAdv" "s"
     return $ mkExpr a
  

findAdverb = do
  a <- inside "AB" $ optEat (lemma "Adv" "s") meta
  write $ "adverb found "++show a
  return (mkExpr a) 
 
pAdvAdj = do
  a <- findAdj
  return $ mkApp cidPositAdvAdj [a]
 
pAdAdj = liftM (\a -> mkApp cidPositAdAAdj [a]) findAdj         

pIQuant = inside "PO" piq
 where piq =    
            do dt <- lemma "IQuant" "s Sg Utr" 
                     `mplus`
                     lemma "IQuant" "s Sg Neutr"
               write ("det: "++show dt)
               returnApp cidDetQuant [mkExpr dt,mkExpr cidNumSg] 
            `mplus`
            do dt <- lemma "IQuant" "s Pl Utr"
                     `mplus`
                     lemma "IQuant" "s Pl Neutr"
               write ("det: "++show dt)
               returnApp cidDetQuant [mkExpr dt,mkExpr cidNumPl] 

pQuant =
  do w <- word "PODP"   -- to avoid this_Quant when it should be DefArt
     let den = map toLower w
     guard (den=="den" || den=="det")                               
     returnApp cidDetQuant [mkExpr cidDefArt,mkExpr cidNumSg]
  `mplus`                                                       
  do inside "PO" (   -- fler taggar än PO?                         
       do dt <-       lemma "Quant" "s Sg False False Utr" -- dessa två ej helt testade
              `mplus` lemma "Quant" "s Sg False False Neutr"
          write ("det: "++show dt)
          returnApp cidDetQuant [mkExpr dt,mkExpr cidNumSg] 
       `mplus`
       do dt <- lemma "Quant" "s Pl False False Utr"
                `mplus` 
                lemma "Quant" "s Pl False False Neutr"
          write ("det: "++show dt)
          returnApp cidDetQuant [mkExpr dt,mkExpr cidNumPl]) 
  `mplus`
  do w <- inside "PO" $ lemma "X" "s (AF (APosit (Strong GPl)) Nom)"
     return $ mkApp cidXDet [mkExpr w]
  `mplus`
  do dt <- inside "PO" $ lemma "Pron" "s (NPPoss GPl Nom)"
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkExpr dt],mkExpr cidNumPl]
  `mplus`
  do dt <- inside "PO" $ lemma "Det" "s False Utr"
     write ("det: "++show dt)
     return $ mkExpr dt 
  `mplus`
  do dt <- inside "PO" $ mplus (lemma "Pron" "s (NPPoss (GSg Neutr) Nom)")
                               (lemma "Pron" "s (NPPoss (GSg Utr) Nom)")
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkExpr dt],mkExpr cidNumSg]
  `mplus`
  do n <- pNumber 
     return $ mkApp cidDetQuant [mkExpr cidIndefArt,mkApp cidNumCard [n]]
 `mplus`
  do inside "EN" $ mplus (lemma "Quant" "s Sg False False Utr")
                         (lemma "Quant" "s Sg False False Neutr")
     return $ mkApp cidDetQuant [mkExpr cidIndefArt,mkExpr cidNumSg]
  `mplus`
  do n <- pNumber 
     return $ mkApp cidDetQuant [mkExpr cidIndefArt,mkApp cidNumCard [n]]
  `mplus`
  do p <- inside "POXPHHGG" $ lemma "Pron" "s (NPPoss (GSg Utr) Nom)"
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkExpr p]]
  `mplus`
  -- genitiv nouns
  do (n,num,def) <- insideSuff "GG" pNounGen
     let dt = mkApp cidDetQuant [mkExpr (getDef def),mkExpr num] 
         np = mkApp cidDetCN [dt,mkApp cidUseN [n]]
     return $ mkApp cidDetQuant [mkApp cidGenNP [np]] 
     



pDetRefl =         
  do w <- word "POXP" 
     write "setting it to true"
     isReflGenVP =: True
     t <- gets isReflGenVP
     write $ "it is " ++ show t
     return () -- $ mkExpr cidReflGenVP 


pN2 = 
  -- hur vill gf ha det här?
  do np <- cat "NP"
     return (np,mkExpr cidNumSg, mkExpr cidDefArt) -- obs this is obviously not always corret. use state?
  `mplus`
  inside "NNDD" (do n <- lemma "N2" "s Pl Def Nom"
                    return (mkExpr n,mkExpr cidNumPl,mkExpr cidDefArt)
                  `mplus`
                  do n <- lemma "N2" "s Sg Def Nom" 
                     return (mkExpr n,mkExpr cidNumSg,mkExpr cidDefArt) 
                  `mplus`
                  do n <- lemma "N2" "s Sg Indef Nom" 
                     return (mkExpr n,mkExpr cidNumSg,mkExpr cidIndefArt) 
                  `mplus`
                  do n <- lemma "N2" "s Pl Indef Nom"
                     return (mkExpr n,mkExpr cidNumPl,mkExpr cidIndefArt))

-- how to handle this? could be a lot of things..
pAppos = do inside "XP" consume 
            return (mkExpr meta)

pPConj = 
  do s <- inside "++" $ lemma "PConj" "s"
     return (mkExpr s)
  `mplus`
  do s <- inside "++" $ lemma "Conj" "s2"
     return (mkApp cidPConjConj [mkExpr s])

pConj = 
  do word "++OC"
     return $ mkExpr cidAndConj
  `mplus`
  do word "++EL"
     return $ mkExpr cidOrConj
  `mplus`
  do s <- inside "++" $ lemma "Conj" "s2"
     return (mkExpr s)
    
pSubj = do 
  s <- inside "UK" $ optEat (lemma "Subj" "s") meta
  return $ mkExpr s 

pCopula  = write "copula?" >> tense "AV"
pHave    = write "have" >> tense "HV"  
--pMust    = write "must?" >> tense "MV"
--pWant    = tense "WV"
--pCan     = tense "QV"
pFuturum = do write "futurum?"
              t <- tense "SV"
              write ("futurum: "++show t)
              if isVTenseForm cidTPres t then return (VTense cidTFut)
                                         else return (VTense cidTCond) -- ?
-- hmm.. weird.                                
pFuturum' = do write "futurum komma att?"
               t <- tense "KV"
               write ("futurum kom att: "++show t)
               if isVTenseForm cidTPres t then return (VTense cidTFut)
                                          else return t 
  
tense cat =
  do word $ cat++"IV"    
     return VInf
  `mplus`
  do word $ cat++"PK"   -- ??
     return VPart
  `mplus`
  do word $ cat++"PS"
     write "presens"
     return (VTense cidTPres)
  `mplus`
  do word $ cat++"PT"
     return (VTense cidTPast)
  `mplus`
  do word $ cat++"SN" 
     return VSupin
  `mplus`
  do word $ cat++"IP"
     return VPart      -- ?? imperativ
   
pPrep = do write "in pPrep"
           p <- inside "PR" $ optEat (lemma "Prep" "s") meta
           return $ mkExpr p

-- här behöver vi kanske kunna ha bla Adv, som 'även'. hur?
pPredet = 
  do w <- findPredet
     return $ mkExpr w 
--  `mplus`
--  do liftM fst pNP
 where findPredet = inside "AB" (lemma "Adv" "s")  --should look for Adv here
                                --  `mplus`                      --and make nice function for this
                                --  lemma "Predet" "s Neutr Sg"
                                --  `mplus`
                                --  lemma "Predet" "s Utr Pl"
                                --  `mplus`
                                --  lemma "Predet" "s Utr Sg") --) meta
                    `mplus`
                    do w <- word "PO"
                       let wd = map toLower w
                       guard (wd /="den" && wd /="det")
                       write "in pPredet with PO"
                       wordlookup w "Predet" "s Neutr Pl"
                        `mplus`
                        wordlookup w "Predet" "s Utr Pl"
                        `mplus`
                        wordlookup w "Predet" "s Utr Sg"
                        `mplus`
                        wordlookup w "Predet" "s Neutr Sg"
                      

                       


-- translate all numers to 1. could also be NumNumeral ( num (pot... n1))
pNumber = 
  inside "RO" $ do consume
                   return $ mkApp cidNumDigits 
                          [mkApp cidIDig 
                             [mkExpr cidD_1]] 

pPol =
  do w  <- cat "NA"
    -- guard (w == "inte" || w == "not") -- andra ord?
     return cidPNeg
  `mplus`
  return cidPPos

listOf f = 
  many $ do
   a <- inside "CJ" f 
   word2 "IK"
   return a

conjunct consf basef conjf f =
  do xs   <- listOf f
     write $ "found list of " ++show xs
     x1   <- inside "CJ" f
     write $ "found first conj " ++show x1
     conj <- inside "++" pConj 
     write $ "found  conj " ++show conj 
     x2   <- inside "CJ" f
     write $ "found snd conj " ++show x2
     let compXs x y = mkApp consf [x,y]
         conjs      = foldr  compXs (mkApp basef [x1,x2]) xs
     return $ mkApp conjf [conj, conjs]


-----
adjSN = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
adjSU = "s (AF (APosit (Strong (GSg Utr))) Nom)"
adjSPl = "s (AF (APosit (Strong GPl)) Nom)"
adjWPl = "s (AF (APosit (Weak Pl)) Nom)"
adjWSg = "s (AF (APosit (Weak Sg)) Nom)" 

isExistNP = (==mkExpr cidExistNP)
meta = mkCId "?"
mkExpr x = mkApp x []
returnApp cid exs = do
  t <- gets sentenceType
  return $ mkApp (getCId t cid) exs 

getCId Q  c | c == cidCompNP   = cidCompIP
            | c == cidCompAdv  = cidCompIAdv
            | c == cidDetCN    = cidIdetCN
            | c == cidDetQuant = cidIdetQuant
            | c == cidPrepNP   = cidPrepIP
getCId _ c = c
  
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

--set b = modify $ \s -> s {isReflGenVP = b}
-- Old code by Krasimir
{-
  advs   <- many (cat "ADVP")
  (t,a,p,e0) <- do (t,v) <- pV "V2"
                   pps   <- many (cat "PP")
                   np    <- cat "NP"
                   let e0 = mkApp cidComplSlash [mkApp cidSlashV2a [mkApp v []], np]
                       e1 = foldl (\e pp -> mkApp cidAdvVPSlash [e, pp]) e0 pps
                   return (t,cidASimul,cidPPos,e1)
                `mplus`
                do (t,v) <- pV "VS"
                   s     <- cat "S"
                            `mplus`
                            (inside "SBAR" $ do
                               (do cat "-NONE-"    -- missing preposition
                                   return ()
                                `mplus`
                                do w <- word "IN"
                                   guard (w == "that"))
                               cat "S")
                   return (t,cidASimul,cidPPos,mkApp cidComplVS [mkApp v [], s])
                `mplus`
                do (t,v) <- pV "V"
                   return (t,cidASimul,cidPPos,mkApp cidUseV [mkApp v []])
                `mplus`
                do t <- pCopula
                   p <- pPol
                   e <- inside "VP" $ pPassive
                   return (t,cidASimul,p,e)
                `mplus`
                do -- t <- pCopula
                   -- p <- pPol
                   e <- do e <- cat "AJ"               --malin
                           return (mkApp cidCompAP [e])
                        `mplus`
                        do e <- cat "NP"
                           return (mkApp cidCompNP [e])
                        `mplus`
                        do e <- cat "PP"
                           return (mkApp cidCompAdv [e])
                   return (VInf,cidASimul,cidPPos,mkApp cidUseComp [e])
                         --fel--
                `mplus`
                do t <- pCopula
                   (tmp,pol,e) <- pVP
                   guard (isVSupin tmp && pol == cidPPos)
                   p <- pPol
                   return (t,cidASimul,p,mkApp cidProgrVP [e])
                `mplus`
                do w <- word "MD"
                   t <- case w of
                          "will"  -> return cidTFut
                          "would" -> return cidTCond
                          _       -> mzero
                   p <- pPol
                   (tmp,pol,e) <- inside "VP" pVP
                   guard (isVInf tmp && pol == cidPPos)
                   return (VTense t,cidASimul,p,e)
                `mplus`
                do t <- pHave
                   p <- pPol
                   (tmp,pol,e) <- inside "VP" pVP
                   guard (isVPart tmp && pol == cidPPos)
                   return (t,cidAAnter,p,e)
                `mplus`
                do word "TO"                        -- infinitives
                   e <- cat "VP"
                   return (VInf,cidASimul,cidPPos,e)
  pps    <- many (cat "PP")
  let tmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp a []]) t
      e1  = foldr (\pp e -> mkApp cidAdVVP [pp, e]) e0 advs
      e2  = foldl (\e pp -> mkApp cidAdvVP [e, pp]) e1 pps
  return (tmp, p, e2)
-}
{-
pV cat =
  do v <- inside "VB"  (opt (lemma cat "s VInf") meta)
     return (VInf,v)
  `mplus`
  do v <- inside "VBP" (opt (lemma cat "s VInf") meta)
     return (VTense cidTPres,v)
  `mplus`
  do v <- inside "VBZ" (opt (lemma cat "s VPres") meta)
     return (VTense cidTPres,v)
  `mplus`
  do v <- inside "VBD" (opt (lemma cat "s VPast") meta)
     return (VTense cidTPast,v)
  `mplus`
  do v <- inside "VBN" (opt (lemma cat "s VPPart") meta)
     return (VPart,v)
  `mplus`
  do v <- inside "VBG" (opt (lemma cat "s VPresPart") meta)
     return (VGerund,v)
     -}
{-
pHave =
  do s <- word "VB"
     guard (s == "have")
     return VInf
  `mplus`
  do s <- word "VBP"
     guard (s == "have")
     return (VTense cidTPres)
  `mplus`
  do s <- word "VBZ"
     guard (s == "has")
     return (VTense cidTPres)
  `mplus`
  do s <- word "VBD"
     guard (s == "had")
     return (VTense cidTPast)
  `mplus`
  do s <- word "VBN"
     guard (s == "had")
     return VPart

pPassive = do
  advs <- many (cat "ADVP")
  v    <- inside "VBN" (opt (lemma "V2" "s VPPart") meta)
  inside "NP" (cat "-NONE-")
  pps  <- many (cat "PP")
  let e0 = mkApp cidPassV2 [mkApp v []]
      e1 = foldr (\ad e -> mkApp cidAdVVP [ad, e]) e0 advs
      e2 = foldl (\e pp -> mkApp cidAdvVP [e, pp]) e1 pps
  return e2

pBaseNP = 
  do np <- inside "NN" (lemma "NP" "s (NCase Nom)")
     return (mkApp np [])
  `mplus`
  do m_pdt <- maybeParse (cat "PDT")
     m_q   <- maybeParse pQuant
     m_num <- maybeParse pCD   
     adjs  <- many pModCN
     ns    <- many1 (mplus (cat "NN"  >>= \n -> return (n,cidNumSg)) 
                           (cat "NNS" >>= \n -> return (n,cidNumPl)))
     let (n,s) = last ns
         cn0   = foldr (\(n,s) e -> mkApp cidCompoundCN [mkApp s [], n, e])
                       (mkApp cidUseN [n])
                       (init ns)
         cn    = foldr (\adj e -> mkApp cidAdjCN [adj, e]) 
                       cn0
                       adjs
         num   = fromMaybe (mkApp s []) m_num
         
     e0 <- if s == cidNumSg
             then case m_q of
                    Just (q,True)  -> returnApp cidDetCN [mkApp cidDetQuant [q,num],cn]

                    Just (q,False) -> returnApp cidDetCN [q,cn]
                    Nothing        -> do guard (isNothing m_num)
                                         return (mkApp cidMassNP [cn])
             else case m_q of
                    Just (q,True)  -> returnApp cidDetCN [mkApp cidDetQuant [q,num],cn]
                    Just (q,False) -> returnApp cidDetCN [q,cn]
                    Nothing        -> returnApp cidDetCN [mkApp cidDetQuant [mkApp cidIndefArt [],num],cn]
     let e1 = case m_pdt of
                Just pdt -> mkApp cidPredetNP [pdt,e0]
                Nothing  -> e0
     return e1
  `mplus`
  do dt <- cat "QP"
     n  <- mplus (cat "NN") (cat "NNS")
     returnApp cidDetCN [dt,mkApp cidUseN [n]]
  `mplus`
  do m_q <- maybeParse pQuant
     ws2 <- many1 (word "NNP" `mplus` word "NNPS")
     let e0 = mkApp cidSymbPN
                    [mkApp cidMkSymb 
                           [mkStr (unwords ws2)]]
     case m_q of
       Just (q,b) -> do guard b
                        return (mkApp cidUseQuantPN [q,e0])
       Nothing    -> return (mkApp cidUsePN      [e0])
  `mplus`
  do p <- cat "PRP"
     return (mkApp cidUsePron [p])
  `mplus`
  do np   <- cat "NP"
     pps  <- many1 (cat "PP")
     prns <- many  (cat "PRN")
     let e0 = foldl (\e pp -> mkApp cidAdvNP [e, pp]) np pps
         e1 = foldl (\e pn -> mkApp meta     [e, pn]) e0 prns
     return e1
  `mplus`
  do np <- cat "NP"
     word ","
     vp <- inside "VP" pPassive
     word ","
     return (mkApp meta [np, vp])
  `mplus`
  do n <- pCD
     return (mkApp cidDetNP [mkApp cidDetQuant [mkApp cidIndefArt [],n]])

pBaseNPs = do
  np <- pBaseNP
  (do word ","
      (m_cc,np2) <- pBaseNPs
      return (m_cc   ,mkApp cidConsNP [np,np2])
   `mplus`
   do cc  <- cat "CC"
      np2 <- pBaseNP
      return (Just cc,mkApp cidBaseNP [np,np2])
   `mplus`
   do return (Nothing,np))

pNPs = do
  np <- cat "NP"
  (do word ","
      (m_cc,np2) <- pNPs
      return (m_cc   ,mkApp cidConsNP [np,np2])
   `mplus`
   do cc  <- cat "CC"
      np2 <- cat "NP"
      return (Just cc,mkApp cidBaseNP [np,np2])
   `mplus`
   do return (Nothing,np))

pModCN =
  do v <- cat "VBN"
     return (mkApp cidPastPartAP [v])
  `mplus`
  do a <- cat "JJ"
     return (mkApp cidPositA [a])
  `mplus`
  do a <- cat "ADJP"
     return a

pCD = 
  do w0 <- word "CD"
     let w = filter (/=',') w0
     guard (not (null w) && all isDigit w)
     let es = [mkApp (mkCId ("D_"++[d])) [] | d <- w]
         e0 = foldr (\e1 e2 -> mkApp cidIIDig [e1,e2]) (mkApp cidIDig [last es]) (init es)
         e1 = mkApp cidNumCard [mkApp cidNumDigits [e0]]
     return e1
  `mplus`
  do w <- word "CD"
     guard (w == "one")
     let e0 = mkApp cidnum [mkApp cidpot2as3 [mkApp cidpot1as2 [mkApp cidpot0as1 [mkApp cidpot01 []]]]]
         e1 = mkApp cidNumCard [mkApp cidNumNumeral [e0]]
     return e1
  `mplus`
  do w <- word "CD"
     guard (w == "seven")
     let e0 = mkApp cidnum [mkApp cidpot2as3 [mkApp cidpot1as2 [mkApp cidpot0as1 [mkApp cidpot0 [mkApp cidn7 []]]]]]
         e1 = mkApp cidNumCard [mkApp cidNumNumeral [e0]]
     return e1
  `mplus`
  do cat "CD"

pQuant =
  inside "DT" pDT
  `mplus`
  do dt <- cat "PRP$"
     return (dt,True)
  `mplus`
  do np <- inside "NP" $ do
             np <- pBaseNP
             word "POS"
             return np
     return (mkApp cidGenNP [np],True)

pDT =
  do dt <- mplus (lemma "Quant" "s False Sg")
                 (lemma "Quant" "s False Pl")
     return (mkApp dt [],True)
  `mplus`
  do dt <- lemma "Det" "s"
     return (mkApp dt [],False)

pAdA = do adv <- cat "RB"
          case unApp adv of
            Just (f,[a]) | f == cidPositAdvAdj 
                   -> return (mkApp cidPositAdAAdj [a])
            _      -> mzero
       `mplus`
       do ada <- inside "RB" (lemma "AdA" "s")
          return (mkApp ada [])

splitDashN (Node w []) =
  case break (=='-') w of
    (w1,'-':w2) -> Node w1 [] : Node "-" [Node "-" []] : splitDashN (Node w2 [])
    _           -> [Node w []]
splitDashN t = [t]
-}
