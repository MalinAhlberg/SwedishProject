{-# LANGUAGE TupleSections #-}
module Translate where
import Monad
import Idents
import Test  -- ta bort
import qualified Format as Form

import PGF hiding (Tree,parse)
import Control.Monad
import System.IO
import System.Process
import Data.Maybe
import Data.List
import Data.IORef
import Data.Char
import Data.Tree

import Debug.Trace
-- man måste ha två paranteser i början av indatan
-- Not parsable: 542 (particle), 452 (även noun), 694 (passive), 802 (passive)
--               898 (kallas), 1001 (mycket som pronomen), 1107 (fler som pronomen)
--               1129 (fler bilar,passiv),1150 (eller som PConj (hittepågrejs runt))
--               ...


-- Test by runnig mainTest. Use testGr, otherwise very slow

test = True
usePGF = testGr
testGr = ("../gf/BigTest.pgf","BigTestSwe")
bigGr  = ("../gf/BigNew.pgf","BigNewSwe")

trace' | test = trace
       | otherwise = flip const

main = main' "test.xml" >> return ()
main2 = main' "test2.xml" >> return ()
mainTest = main' "testSimple.xml" >>= putStrLn . compareRes
mainT2 = main' "testSimple.xml" >>= putStrLn . unlines
main' fil = do
  pgf <- readPGF $ fst usePGF
  let Just language = readLanguage $ snd usePGF
      morpho        = buildMorpho pgf language
  s <- fmap concat $ Form.parse fil
  ref <- trace' (show $ prune $ head s) $ newIORef (0,0,0)
  mapM (process pgf morpho ref) ({-(if test then take 15 else id)-} s)
  where
    process pgf morpho ref t = do
      (cn,co,l) <- readIORef ref
      let e         = parse penn pgf morpho (prune t)
          (cn',co') = count (cn,co) e
          l'        = l+1
      writeIORef ref (cn',co',l')
      hPutStrLn stdout (showExpr [] e)
      when test $ do
        writeFile "tmp_tree.dot" (graphvizAbstractTree pgf (True,False) e)
        rawSystem "dot" ["-Tpdf", "tmp_tree.dot", "-o trees/tree"++showAlign l'++".pdf"]
        return ()
      hPutStrLn stderr (show ((fromIntegral cn' / fromIntegral co') * 100))
      return (showExpr [] e)

    count (cn,co) e = cn `seq` co `seq`
      case unApp e of
        Just (f,es) -> if f == meta
                         then foldl' count (cn,  co+1) es
                         else foldl' count (cn+1,co+1) es
        Nothing     -> (cn+1,co+1)


    showAlign n =
      replicate (5 - length s) '0' ++ s
      where
        s = show n

    prune (Node tag ts)
      |   tag == "ROOT" 
       && not (null ts)
       && last ts == Node "." [Node "." []] = Node tag (init ts)
      | otherwise                           = Node tag ts


testa  str = do
  pgf <- readPGF "../gf/BigTest.pgf"
  let Just language = readLanguage "BigTestSwe"
      morpho        = buildMorpho pgf language
  return [(lemma,an,cat) | (lemma,an) <- lookupMorpho morpho str
                   ,let cat = maybe "" (showType []) (functionType pgf lemma)]


penn :: Grammar String Expr
penn =
  grammar (mkApp meta) 
   ["S" :-> do trace' "hej" $ return () 
               conj     <- opt (liftM Just $ inside "++" pPConj) Nothing
               trace' ("conj: "++show conj) $ return ()
               cl <- pCl 
                     `mplus`
                     pImp 
                     `mplus`
                     pNPCl
               opt (word2 "IP") ""
               opt (word2 "I?") ""
               let pconj = fromMaybe (mkApp cidNoPConj []) conj
               return $ mkApp cidPhrUtt [pconj, cl,mkApp cidNoVoc []]

     ,"AP" :-> do trace' "in AP" $ return ()
                  ad <- inside "AA" pAdA
                  trace' ("found adA: "++show ad) $ return ()
                  a  <- inside "HD" pAdj
                  trace' ("found adj: "++show a) $ return ()
                  return $ mkApp cidAdAP [ad,a]
               `mplus`
               do as <- many $ inside "AA" pAdAdj
                  a2 <- inside "HD" pAdj 
                  return (foldr (\ada ap -> mkApp cidAdAP [ada,ap]) (a2) as)
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
 --   ,"AVP" :-> bland annat, just nu, t ex, i kontakt... 
      ,"CAP" :-> do as   <- listOf pAdj 
                    a1   <- inside "CJ" pAdj
                    conj <- inside "++" pConj 
                    a2   <- inside "CJ" pAdj
                    let compas x y = mkApp cidConsAP [x,y]
                        conjs  = foldr  compas (mkApp cidBaseAP [a1,a2]) as
                    return $ mkApp cidConjAP [conj, conjs]
      ,"CNP" :-> do n1 <- inside "CJ" pAdj 
                    conj <- inside "++" pConj 
                    a2   <- inside "CJ" pAdj 
                    return $ mkApp cidConjNP [conj, mkApp cidBaseAP [n1,a2]]
      ,"NP" :-> pflatNP
                   
      ,"PP" :-> do pr     <- trace' "PP!" $ inside "PR" pPrep
                   trace' "prep found" $ return ()
                   np     <- pflatNP
                   trace' "prep noun found" $ return ()
                   return $ mkApp cidPrepNP [pr,np]
      ,"XX" :-> do n     <- opt (liftM Just pNP) Nothing  -- här får vi nog lägga till mer
                   let e = maybe (mkApp meta []) fst n
                   trace' ("xx returns "++show e) $ return ()
                   return $ mkApp meta [e]
      ,"XP" :-> do trace' "xp!" $ return ()
                   x <- cat "XX"
                   trace' "xp found noun" $ return ()
                   a <- pAdv     
                   trace' "xp found adv " $ return ()
                   opt (word2 "IP") ""
                   return $ mkApp meta [x,a]


     -- ,"CAVP" :-> do 
   --, "CC"  :-> do cc <- lemma "Conj" "s2"
    --              return (mkApp cc [])
   ]
 

pNPCl = do 
 (np,typ) <- inside "SS" $ optEat pNP (mkApp meta [],cidPredVP)
 return $ mkApp cidUttNP [np]


pCl = do 
 cl <- do trace' "looking for SS" $ return () 
          (np,typ) <- inside "SS" $ optEat pNP (mkApp meta [],cidPredVP)
          trace' ("SS done "++show np) return ()
          --advs <- many $ cat "OA"
          trace' "now to pVP" return ()
          (tmp,sim,pol,vp) <- (trace' "goto pVP" $ pVP "FV")
                              `mplus` (trace' "no VP!" $ inside "FV" consume >> metaVP) -- obs! för passiv
          advs <- many pAdv
          let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
              e1 = if typ == cidGenericCl then mkApp typ [e0]
                                          else mkApp typ [np,e0]
              e2 = mkApp cidUseCl [fromMaybe (mkApp meta []) (isVTense tmp)
                                  ,mkApp pol [],e1]
          return e2
       `mplus`
      -- find out whether it should be AdvS or FocAdv here!
       do advs <- many pAdv    -- use def to know if it is question or not
          (tmp,pol,np,nptyp,vp) <- pVSOs
          let e0 = mkApp cidUseCl [fromMaybe (mkApp meta []) (isVTense tmp)
                                  ,mkApp pol []
                                  ,mkApp nptyp [np,vp]
                                  ]
              e1 = foldr (\ad e -> mkApp cidAdvS [ad, e]) e0 advs
          return e1
 return $ mkApp cidUttS [cl]
 
pVSOs :: P String Expr (VForm Expr, CId, Expr, CId, Expr)
pVSOs = foldr1 (mplus) $ map pVSO vForms

pVSO :: VPForm -> P String Expr (VForm Expr, CId, Expr, CId, Expr)
pVSO typ = do
  (v,t) <- pSlashVP typ "FV"
                `mplus`
                inside "FV" (consume >> metaVP' typ)
  (np,nptyp) <- trace' "looking for SS" $ inside "SS" pNP
  trace' ("AdvCl found np "++show np) $ return ()
  (tmp,s,pol,vp) <- pComplVP typ v t
  trace' ("AdvCl found compl "++show vp) $ return ()
  return (tmp,pol,np,nptyp,vp)


pImp = do trace' "in imperative" $ return ()
          (tmp,sim,pol,vp) <- pVP "FV"
          trace' "found vp in imp" $ return ()
          guard (tmp==VInf)
          trace' "vp in imp is ok" $ return ()
          advs <- many pAdv
          trace' ("advs found: "++show advs) $ return ()
          let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
              imp = mkApp cidImpVP [e0] 
          return $ mkApp cidUttImpPol [mkApp pol [],imp]
          
data VForm a
  = VInf | VPart | VSupin | VTense a
   deriving (Show,Eq)

instance Functor VForm where
  fmap f VInf       = VInf
  fmap f VPart      = VPart
  fmap f VSupin     = VSupin
  fmap f (VTense t) = VTense (f t)

isVInf VInf = True
isVInf _    = False

isVPart VPart = True
isVPart _     = False

isVSupin VSupin = True
isVSupin _       = False

isVTense (VTense t) = Just t
isVTense _          = Nothing


data VPForm  = Cop | Sup | VV | VA | V | V2
  deriving (Eq,Show)

vForms  = [Cop,Sup,VV,VA,V2,V]

pSlashVP V typ =
 do (t,v) <-inside typ $ pVerb "VV" "V"
                         `mplus`
                         liftM (,cidUseCopula) pCopula
    return (mkApp v [],t)

pSlashVP VV typ =
 do (t,v) <- inside typ pVV
    return (v,t)

pSlashVP V2 typ =
 do (t,v) <- inside typ pV2 
    return (v,t)


pSlashVP Cop typ =
 do t <- inside typ pCopula 
    return (mkApp meta [],t)

pSlashVP Sup typ =
  do t <- inside typ pHave
     return (mkApp meta [],t)


pSlashVP VA typ =
  do (t,v) <- inside typ pVA
     return (v,t)



mkTmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) 



-- adv efter verb? före eller efter pol?
-- simplify by using pSlash and pCompVP
pVP typ = 
 do trace' "try pVP one" $ return ()    
    -- Copula: is red/a cat...
    pSlashVP Cop typ >>= uncurry (pComplVP Cop)

 `mplus`
  do trace' "VP supinum" $ return ()
     -- supinum
     pSlashVP Sup typ >>= uncurry (pComplVP Sup)
 `mplus`

 do trace' "try pVP two" $ return ()
    -- V2: have a cat
    pSlashVP V2 typ >>= uncurry (pComplVP V2)
 `mplus`

 do trace' "try VV" (return ())
    -- VV: must go, fortsätter att
    pSlashVP VV typ >>= uncurry (pComplVP VV)
  `mplus`

  do trace' "try VA" $ return ()
     -- VA 
     pSlashVP VA typ >>= uncurry (pComplVP VA)
  `mplus`

  do trace' "simple v tries" $ return ()
     -- V: think
     pSlashVP V typ >>= uncurry (pComplVP V)
  `mplus`

  do trace' "att v?" $ return ()
     -- to go
     inside typ (do word2 "IM"
                    pVP "IV")

pComplVP :: VPForm -> Expr -> VForm CId -> P String Expr (VForm Expr, CId, CId, Expr)
pComplVP V vp tmp = do 
  (pol,adv,p) <- pComplV
  let vp1  = maybe vp (\a -> mkApp meta [vp,a]) adv 
  case p of 
    Nothing -> return (mkTmp tmp,cidASimul,pol,mkApp cidUseV [vp1]) 
    Just x  -> return (mkTmp tmp,cidASimul,pol,mkApp meta    [vp1,mkApp x []]) 
pComplVP VA vp tmp = do 
  (pol,adv,a) <- pComplVA
  let vp1  = maybe vp (\a -> mkApp meta [vp,a]) adv 
  return (mkTmp tmp,cidASimul,pol,mkApp cidComplVA [vp1,a])
pComplVP VV vp tmp = do
  (pol,adv,iv,p,b) <- pComplVV
  let vv0 = if b then mkApp cidDropAttVV [vp] else vp
      vv1  = maybe vv0 (\a -> mkApp meta [vv0,a]) adv 
  let vv2 = case p of 
                Nothing -> mkApp cidComplVV [vv1,iv] 
                Just x  -> mkApp cidComplVV [vv1, mkApp meta [iv,mkApp x []]]
  return (mkTmp tmp,cidASimul,pol,vv2)
pComplVP V2 vp tmp = do
  (pol,adv,obj,adj) <- pComplV2
  let vp0  = maybe vp (\a -> mkApp meta [vp,a]) adv 
      np0 = maybe obj (\a -> mkApp meta [obj,a]) adj
      vp1 = mkApp cidComplSlash [vp0,np0]
  return (mkTmp tmp,cidASimul,pol,vp1)
pComplVP Sup vp t = do
  (pol,adv,sup) <- pComplSup
  let tmp  = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidAAnter []]) t
      vp0  = mkApp sup []
      vp1  = maybe vp0 (\a -> mkApp meta [vp0,a]) adv 
  return (tmp,cidAAnter,pol,mkApp cidUseV [vp1])
pComplVP Cop vp tmp = do
  (pol,adv,sp) <- pComplCopula 
  let cop = mkApp cidUseComp [sp]
      vp1  = maybe cop (\a -> mkApp meta [cop,a]) adv 
  return (mkTmp tmp,cidASimul,pol,vp1)




pPart = (trace' "part" $ inside "PR" $ lemma "V" "c s" `mplus` optEat (lemma "Prep" "s") meta)
        `mplus`
        (inside "AB" $ optEat (lemma "A" "s") meta)  -- vi vet inte hur en sån ska se ut
-- (send_V3,"c3 s","V3")
-- (mother_N2,"c2 s","N2")
     
pVV = do
  (t,v) <- tryVerb "FV" cidGet_VV "VV"  
           `mplus`
           tryVerb "WV" cidWant_VV "VV"
           `mplus`
           tryVerb "QV" cidCan_VV "VV"
           `mplus`
           tryVerb "MV" cidMust_VV "VV"
           `mplus`
           pVerb "VV" "VV"
  trace' ("VV returs tense "++show t) $ return ()
  return (t,mkApp v [])

pVA = do
  (t,v) <- tryVerb "BV" cidBecome_VA "VA"
           `mplus`
           pVerb "FV" "VA" 
  trace' ("VA returs tense "++show t) $ return ()
  return (t,mkApp v [])


pV2 = do 
  (t,v) <- do t <- pHave
              return (t,mkApp cidHave_V2 [])
           `mplus`
           -- man skulle kunna kolla mer på taggarna här
           do (t,v) <- trace' "in pV2" $ pVerb "VV" "V2"
                       `mplus`                   
                       trace' "får är i farten" (tryVerb "FV" cidGet_V2 "V2")
                       `mplus`
                       tryVerb "GV" cidDo_V2 "V2"
                       `mplus`
                       tryVerb "BV" cidBecome_V2 "V2"
              return (t,mkApp v [])
  return (t,mkApp cidSlashV2a [v])

tryVerb tag cid cat =
 do t <- tense tag
    return (t,cid) 
 `mplus`
  trace' "no tense found" (pVerb tag cat) 

pVerb incat cat =
        do v <- inside incat $ lemma cat "s (VF (VPres Act))"
           return (VTense cidTPres,v)
        `mplus`
        do v <- inside incat $ lemma cat "s (VI (VInfin Act))"
           return (VInf,v)
        `mplus`
        do v <- inside incat $ lemma cat "s (VF (VPret Act))"
           return (VTense cidTPast,v)
        `mplus`
        do v <- inside incat $ lemma cat "s (VI (VSupin Act))"
           return (VSupin,v)
        `mplus`
        (inside (incat++"PS") consume >> return (VTense cidTPres,meta))
        `mplus`
        (inside (incat++"PT") consume >> return (VTense cidTPast,meta))
        `mplus`
        (inside (incat++"SN") consume >> return (VSupin,meta))
        `mplus`
        do trace' "could not find verb" $ return ()
           inside incat consume  
           return metaVerb

maybeVerbAdv  = opt (liftM Just $ inside "+A" findAdverb) Nothing
maybeParticle = opt (liftM Just $ inside "PL" pPart) Nothing 

metaVP = do
  let tmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) $ VTense cidTPres  
  return (tmp,cidASimul,cidPPos,mkApp meta [])

metaVP' :: VPForm -> P String Expr (Expr,VForm CId)
metaVP' vf = return (mkApp meta [],VTense cidTPres)

metaVerb   = (VInf,meta)


pComplCopula = do
  trace' "copula compl begins" $ return ()
  pol <- pPol
  adv <- maybeVerbAdv
  sp <- inside "SP" (do a <- pAdj 
                        trace' ("adj return"++show a) $ return (mkApp cidCompAP [a])
                     `mplus`
                     do (e,_) <- pNP
                        return (mkApp cidCompNP [e])
                     `mplus`
                     do e <- cat "PP"
                        return (mkApp cidCompAdv [e]))
        `mplus`
        do a <- pAdv
           return (mkApp cidCompAdv [a])
  return (pol,adv,sp)

pComplSup = do
  trace' "supinum compl begins" $ return ()
  p <- pPol
  adv <- maybeVerbAdv
  (t',sup) <- inside "IV" $ optEat (pVerb "TP" "V") (VSupin,meta) -- inte bara V 
  guard (isVSupin t') -- && pol == cidPPos)
  return (p,adv,sup)

pComplV2 = do
  trace' "v2 compl begins" $ return ()
  pol <- pPol
  trace' "oo pol ok" $ return ()
  adv <- maybeVerbAdv
  (obj,f) <- inside "OO" pNP
             `mplus`
             inside "SP" pNP
             `mplus`
             do o <- inside "OA" (cat "PP" `mplus` cat "VP")
                return (mkApp meta [o],cidPredVP) --tveksamt fall?
  trace' "oo ok" $ return ()
  adj <- opt (liftM Just $ inside "OO" findAdj) Nothing  -- eller findAdvAdj?
  return (pol,adv,obj,adj)

-- dropAtt only needed for some verbs.. More checking?
pComplVV = do
  trace' "vv compl begins" $ return ()
  pol   <- pPol
  trace' "pol ok" $ return ()
  adv <- maybeVerbAdv
  (t',s,p,iv,b)  <- do (t,s,p,i) <- pVP "IV"
                       return (t,s,p,i,False)   
                   `mplus`
                   do (t,s,p,i) <- inside "OO" (pVP "VP")
                      return (t,s,p,i,False)
                   `mplus`
                   do (t,s,p,i) <- inside "OO" (inside "NAC" $ pVP "IV")
                      return (t,s,p,i,True)
  trace' ("iv found "++show iv) $ return ()
  guard (t'==VInf)  
  guard (p==cidPPos)  -- man får inte säga 'jag vill inte (inte tänka)'
  trace' "iv ok" $ return ()
  p <- maybeParticle
  trace' ("particle: "++show p) $ return ()
  return (pol,adv,iv,p,b)

pComplVA = do
  trace' "va compl begins" $ return ()
  pol   <- pPol
  adv   <- maybeVerbAdv
  a     <- inside "SP" pAdj
  return (pol,adv,a)

pComplV = do
  trace' "v-simple compl begins" $ return ()
  pol <- pPol
  adv <- maybeVerbAdv
  p   <- maybeParticle
  trace' ("particle: "++show p) $ return ()
  return (pol,adv,p)


pflatNP =
  do trace' "in NP with Adj" $ return ()
     -- good cars
     m_predet     <- opt (liftM Just $ inside "+A" pPredet) Nothing
     m_det        <- opt (liftM Just $ inside "DT" pDet)    Nothing 
     m_a          <- opt (liftM Just $ inside "AT" pAdj)    Nothing
     (noun,n,def) <- inside "HD" pCN
     et           <- many $ inside "ET" $ cat "PP"
     let cn = case m_a of
                   Just a  -> mkApp cidAdjCN [a,mkApp cidUseN [mkApp noun []]]
                   Nothing -> mkApp cidUseN [mkApp noun []]
         num = mkApp n []
         d   = fromMaybe (mkApp cidDefArt []) m_det
         -- mer när vi vet hur även ska hanteras. även den bilen, även bilar osv.
         -- kan antagligen först göra np av de andra och sen ta predet utanpå
     np0 <- case (def,m_det) of
                (NDef,_       ) -> return $ mkApp cidDetCN 
                                             [mkApp cidDetQuant [d,num]
                                             ,cn]
                (NIndef,Nothing) -> if n == cidNumSg 
                            then return (mkApp cidMassNP [cn])
                            else return $ mkApp cidDetCN 
                                            [mkApp cidDetQuant 
                                            [mkApp cidIndefArt [],num],cn]
                (NIndef,Just d)  -> return $ mkApp cidDetCN [d,cn]
                (NOther,_)       -> do guard (isNothing m_predet && isNothing m_det) --ok?
                                       return $ mkApp noun []
     let np1 = case m_predet of
                    Just p -> mkApp cidPredetNP [p,np0]
                    Nothing -> np0 
     return $ foldr (\e n -> mkApp cidAdvNP [n,e]) np1 et 
                 
-- returns (word :: CId, number :: CId, determined :: NounForm)
pCN = 
     inside "VN" pNoun
     `mplus`
     inside "NN" (optEat pNoun metaNoun)  --optEat eller ej?
     `mplus`
     inside "AN" pNoun
     `mplus`
     do word "NNDD"
        return (meta,cidNumSg,NDef)  --kan vara Pl också..
     `mplus`
     do w <- inside "POCP" consume  -- varandra, reciprokt! ej i GF
        return (meta,cidNumPl,NOther)
     `mplus`
     do trace' "testing last pCN" $ return ()
        word "NN"
        return (meta,cidNumSg,NIndef)
 
 
-- may use tag, "xx    GG" = genitiv
pNoun = 
  do      n <- (mplus (lemma "N" "s Pl Indef Nom")
                      (lemma "N" "s Pl Indef Gen"))
          return (n,cidNumPl,NIndef)
  `mplus` do
          n <- (mplus (lemma "N" "s Sg Indef Nom")
                      (lemma "N" "s Sg Indef Gen"))
          return (n,cidNumSg,NIndef)
  `mplus` do
          n <- (mplus (lemma "N" "s Sg Def Nom")
                      (lemma "N" "s Sg Def Gen"))
          return (n,cidNumSg,NDef)
  `mplus` do
          n <- (mplus (lemma "N" "s Pl Def Nom")
                      (lemma "N" "s Pl Def Gen"))
          return (n,cidNumPl,NDef)

metaNoun = (meta,cidNumSg,NIndef)
data NForm = NDef | NIndef | NOther -- NOther for reciprocs etc 

isDef :: NForm -> Bool
isDef NIndef = True
isDet _      = False
 
pNP = 
  (cat "NP" >>= \x -> trace' ("cat np "++show x) $ return (x,cidPredVP))  --här kanske vi behöver tänka mer ang PredVP
  `mplus` 
  (cat "AP" >>= \x -> return (x,cidPredVP))  --och här med
  `mplus` 
   do w   <- inside "POFP" $ lemma "IP" "s NPNom"
      return (mkApp w [],cidQuestVP)
            {- `mplus`  Ha med detta?
             inside "POFP" $ lemma "IQuant" "s -}
   `mplus`
   do w <- inside "POTP" $ lemma "NP" "s NPNom"
      return (mkApp w [],cidPredVP)
   `mplus`
   do
      w   <- inside "PO" $ lemma "Pron" "s NPNom" 
                        {-   `mplus`                   -- for s1001, 'mycket blir enklare'
                           lemma "Det" "s True Neutr"  -- needs change in GF
                           `mplus`
                           lemma "Det" "s True Utr"  -}
      trace' "lemma ok" return () 
      return (mkApp cidUsePron [mkApp w []],cidPredVP)
   `mplus`
   do w <- inside "PO" $ lemma "VP -> Cl" "s Pres Simul Pos Main"
      trace' "Man hittad!!" $ return ()
      return (mkApp w [],cidGenericCl)
   `mplus`
   do 
      trace' "in complicated np" $ return ()
      (n,num,def) <- pCN 
      let cn   = (mkApp cidUseN [mkApp n []])
          nums = mkApp num []
      e0 <- case def of
                 NDef -> return $ mkApp cidDetCN 
                                   [mkApp cidDetQuant 
                                   [mkApp cidDefArt [], nums],cn]
                 NIndef -> do guard (num == cidNumSg)
                              return (mkApp cidMassNP [cn])
                 NOther -> return $ mkApp n [] -- och guards!!
      return (e0,cidPredVP)
  
-- akta optEat här!! om fler läggs till måste den flyttas ut!
pAdj = 
  do ad <- inside "AJKP" $ optEat (lemma "A" "s (AF ACompar Nom)") meta
     return $ mkApp cidUseComparA [mkApp ad []] 
  `mplus`
  {-  not supported by gf 'den är gulast'
  do ad <- inside "AJSU" $ optEat (lemma "A" "(AF (ASuperl SupStrong) Nom") meta
     return $ mkApp cidUseOrdSuperl [mkApp ad []] 
  `mplus`
  -}
  do ad <- findAdj
     return $ mkApp cidPositA [ad]
  `mplus`
  trace' "will check AP" (cat "AP")
  `mplus`
  cat "CAP" 
  `mplus`
  do a <- inside "TP" $ optEat findAParticip meta
     return (mkApp meta [mkApp a []])
  
findAdj = 
  do ad <- inside "AJ" $ optEat (lemma "A" adjSN 
                                 `mplus` lemma "A" adjSU
                                 `mplus` lemma "A" adjWSg
                                 `mplus` lemma "A" adjWPl) meta
     return $ mkApp ad []
  `mplus`
  do part <- inside "SP" findNParticip
     return $ mkApp meta [mkApp part []]

-- not in gf :O!
-- make function and simplify
findNParticip = consume >> return meta
findAParticip = 
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
                        return (mkApp ada [])

pAdv = 
  trace' "looking for adv" (inside "RA" (findAdverb `mplus` cat "PP"))
  `mplus`
  trace' "looking for adv" (inside "TA" (findAdverb `mplus` cat "PP"))
  `mplus`
  trace' "looking for adv" (inside "MA" (findAdverb `mplus` cat "PP"))
  `mplus`
  trace' "looking for adv in AA1" (inside "AA" (cat "PP" `mplus` pAdvAdj `mplus` findAdverb))
--  `mplus`
--  trace' "looking for adv in AA2" (inside "AA" pAdvAdj)
  `mplus` inside "+A"  findAdverb 

findAdverb = do
  a <- inside "AB" $ optEat (lemma "Adv" "s") meta
  return (mkApp a []) 
 
pAdvAdj = do
  a <- findAdj
  return $ mkApp cidPositAdvAdj [a]
 
pAdAdj = liftM (\a -> mkApp cidPositAdAAdj [a]) findAdj


pDet = 
  do w <- word "PODP"            -- to avoid this_Quant when it should be DefArt
     guard (map toLower w=="den")
     return $ mkApp cidDefArt []
  `mplus`
  inside "PO" (   -- fler taggar än PO?
    do dt <-          lemma "Quant" "s Sg False False Utr"  -- dessa två ej helt testade
              `mplus` lemma "Quant" "s Sg False False Neutr"
              `mplus` lemma "Quant" "s Pl False False Utr"
              `mplus` lemma "Quant" "s Pl False False Neutr"
       trace' ("det: "++show dt) $ return ()
       return $ mkApp dt []) 
  `mplus`
  do dt <- inside "PO" $ lemma "Det" "s False Utr"
     trace' ("det: "++show dt) $ return ()
     return $ mkApp dt [] 
  `mplus`
  do dt <- inside "PO" $ lemma "Pron" "s (NPPoss GPl Nom)"
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkApp dt []],mkApp cidNumPl []]
  `mplus`
  do dt <- inside "PO" $ mplus (lemma "Pron" "s (NPPoss (GSg Neutr) Nom)")
                               (lemma "Pron" "s (NPPoss (GSg Utr) Nom)")
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkApp dt []],mkApp cidNumSg []]
  `mplus`
  do inside "EN" $ mplus (lemma "Quant" "s Sg False False Utr")
                         (lemma "Quant" "s Sg False False Neutr")
     return $ mkApp cidDetQuant [mkApp cidIndefArt [],mkApp cidNumSg []]
  `mplus`
  -- hur vill gf ha det här?
  do (dn,num) <- inside "NNDD" $ (do n <- lemma "N" "s Pl Def Nom"
                                     return (n,cidNumPl))
                                 `mplus`
                                 (do n <- optEat (lemma "N" "s Sg Def Nom") meta
                                     return (n,cidNumSg))         
     return $ mkApp cidDetQuant [mkApp meta [mkApp dn []],mkApp num []]


pPConj = 
  do s <- inside "++" $ lemma "PConj" "s"
     return (mkApp s [])
  `mplus`
  do s <- inside "++" $ lemma "Conj" "s2"
     return (mkApp cidPConjConj [mkApp s []])

pConj = 
  do word "++OC"
     return $ mkApp cidAndConj []
  `mplus`
  do word "++EL"
     return $ mkApp cidOrConj []
  `mplus`
  do s <- inside "++" $ lemma "Conj" "s2"
     return (mkApp s [])

pCopula = trace' "copula?" $ tense "AV"
pHave   = trace' "have" $ tense "HV"  
pMust   = trace' "must?" $ tense "MV"
pWant   = tense "WV"
pCan    = tense "QV"
  
tense cat =
  do word $ cat++"IV"    
     return VInf
  `mplus`
  do word $ cat++"PK"   -- ??
     return VPart
  `mplus`
  do word $ cat++"PS"
     trace' "presens" $ return ()
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
   
pPrep = do trace' "in pPrep" $ return ()
           p <- inside "PR" $ optEat (lemma "Prep" "s") meta
           return $ mkApp p []

-- här behöver vi kanske kunna ha bla Adv, som 'även'. hur?
pPredet = 
  do w <- optEat findPredet  meta
     return $ mkApp w [] 
 where findPredet = inside "AB" $ lemma "Predet" "s Neutr Pl"
                                  `mplus`
                                  lemma "Predet" "s Neutr Sg"
                                  `mplus`
                                  lemma "Predet" "s Utr Pl"
                                  `mplus`
                                  lemma "Predet" "s Utr Sg"


pPol =
  do w  <- cat "NA"
    -- guard (w == "inte" || w == "not") -- andra ord?
     return cidPNeg
  `mplus`
  return cidPPos

listOf f = 
  many $ do
   a <- inside "CJ" pAdj 
   word2 "IK"
   return a


-----
adjSN = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
adjSU = "s (AF (APosit (Strong (GSg Utr))) Nom)"
adjSPl = "s (AF (APosit (Strong GPl)) Nom)"
adjWPl = "s (AF (APosit (Weak Pl)) Nom)"
adjWSg = "s (AF (APosit (Weak Sg)) Nom)" 


meta = mkCId "?"
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
  do m_pdt <- opt (liftM Just (cat "PDT")) Nothing
     m_q   <- opt (liftM Just pQuant) Nothing
     m_num <- opt (liftM Just pCD   ) Nothing
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
                    Just (q,True)  -> return (mkApp cidDetCN [mkApp cidDetQuant [q,num],cn])

                    Just (q,False) -> return (mkApp cidDetCN [q,cn])
                    Nothing        -> do guard (isNothing m_num)
                                         return (mkApp cidMassNP [cn])
             else case m_q of
                    Just (q,True)  -> return (mkApp cidDetCN [mkApp cidDetQuant [q,num],cn])
                    Just (q,False) -> return (mkApp cidDetCN [q,cn])
                    Nothing        -> return (mkApp cidDetCN [mkApp cidDetQuant [mkApp cidIndefArt [],num],cn])
     let e1 = case m_pdt of
                Just pdt -> mkApp cidPredetNP [pdt,e0]
                Nothing  -> e0
     return e1
  `mplus`
  do dt <- cat "QP"
     n  <- mplus (cat "NN") (cat "NNS")
     return (mkApp cidDetCN [dt,mkApp cidUseN [n]])
  `mplus`
  do m_q <- opt (liftM Just pQuant) Nothing
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
