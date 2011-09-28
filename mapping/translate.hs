{-# LANGUAGE TupleSections, TypeOperators, TemplateHaskell #-}
module Translate where
import MonadSP
import Idents
import Test  -- ta bort
import qualified Format as Form

import PGF hiding (Tree,parse)
import Control.Monad
import Control.Monad.RWS hiding (put,gets,get)
import System.IO
import System.Process
import Data.Maybe
import Data.List
import Data.IORef
import Data.Char
import Data.Tree
import Data.Label 
import Data.Label.PureM 

--import Debug.Trace
-- man måste ha två paranteser i början av indatan
-- Not parsable: 542 (particle), 452 (även noun), 694 (passive), 802 (passive)
--               898 (kallas), 1001 (mycket som pronomen), 1107 (fler som pronomen)
--               1129 (fler bilar,passiv),1150 (eller som PConj (hittepågrejs runt))
--               ...


-- Test by runnig mainTest. Use testGr, otherwise very slow

data S = S { _isReflGenVP :: Bool
           , _isExist     :: Bool
           }

$(mkLabels [''S])

test = True
usePGF = testGr
testGr = ("../gf/BigTest.pgf","BigTestSwe")
bigGr  = ("../gf/BigNew.pgf","BigNewSwe")
paint  = False

startState :: S 
startState = S {_isReflGenVP = False, _isExist = False}

{-trace' | test = trace
       | otherwise = flip const
       -}

main = main' "test.xml" >> return ()
bigTest = main' "../testSuites/testShortSimpleTwo.xml" >>= writeFile "mappingShort3.txt" . unlines
main2 = main' "test2.xml" >> return ()
mainTest = main' "testSimple.xml" >>= putStrLn . compareRes
mainT2 = main' "testSimple.xml" >>= putStrLn . unlines
main' fil = do
  pgf <- readPGF $ fst usePGF
  let Just language = readLanguage $ snd usePGF
      morpho        = buildMorpho pgf language
  s <- fmap concat $ Form.parse fil
  putStrLn (show $ prune $ snd $ head s)
  ref <- newIORef (0,0,0)
  mapM (process pgf morpho ref) ({-(if test then take 15 else id)-} s)
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
        writeFile "tmp_tree.dot" (graphvizAbstractTree pgf (True,False) e)
        rawSystem "dot" ["-Tpdf", "tmp_tree.dot", "-otrees/tree"++showAlign l'++".pdf"]
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


penn :: Grammar (RWS () [String] S) String Expr
penn =
  grammar (mkApp meta)
   ["ROOT" :-> do -- fult, gör fint
                  s <- inside "MS" $ cat "S"
                                     `mplus` cat "XP"
                  write "HAHA"
                  return s -- $ mkApp cidPhrUtt [pconj, s,mkApp cidNoVoc []]
   
     ,"S" :-> do write "hej" 
                 conj     <- maybeParse $ inside "++" pPConj
             --    write ("conj: "++show conj)
                 (s,s2)   <- pS
                 m_voc <- maybeParse (do opt (word2 "IK") ""
                                         inside "TT" $ pNP)
                 opt (word2 "IP" `mplus`
                      word2 "I?" `mplus`
                      word2 "IG" `mplus`
                      word2 "IU") ""
                 let pconj = fromMaybe (mkApp cidNoPConj []) conj
                     voc   = maybe (mkApp cidNoVoc []) fst m_voc
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
     -- ,"CAVP" :-> coordinated AVP 
      ,"CAP" :-> conjunct cidConsAP cidBaseAP cidConjAP pAdj
                    {-as   <- listOf pAdj 
                    a1   <- inside "CJ" pAdj
                    conj <- inside "++" pConj 
                    a2   <- inside "CJ" pAdj
                    let compAs x y = mkApp cidConsAP [x,y]
                        conjs  = foldr  compAs (mkApp cidBaseAP [a1,a2]) as
                    return $ mkApp cidConjAP [conj, conjs] -}
      ,"NP" :-> pflatNP
                   
      ,"PP" :-> do pr     <- write "PP!" >> inside "PR" pPrep
                   write "prep found"
                   np     <- pflatNP
                   write "prep noun found"
                   return $ mkApp cidPrepNP [pr,np]
      ,"XX" :-> do n     <- maybeParse pNP  -- not really a cat 
                   let e = maybe (mkApp meta []) fst n    -- här får vi nog lägga till mer
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
      --,"NAC" :-> (consume >> return (mkApp meta []))
     --"ROOT" :-> släng bort
     --"S" :-> behövs ej
      ,"VP" :-> do word2 "IM"                
                   (tmp,s,pol,v) <- pVP "IV"
                   return v 

   ] 


clType typ | typ==cidQuestVP = cidUseQCl
           | otherwise       = cidUseCl
utType typ | typ==cidQuestVP = cidUttQS
           | otherwise       = cidUttS

parseSCl = inside "S" $ pCl

pS = do
  cl <- do (cl,_,_,utt) <- pCl 
           write "found cl"
           return $ mkApp utt [cl]
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
  s2 <- maybeParse $ inside "+F" (optEat (cat "S") (mkApp meta []))
  let cl1   = maybe cl (\x -> mkApp meta [x]) s2 
  return (cl1,s2)


pNPCl = do 
 (np,typ) <- parseSubject
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
         e1 = if typ == cidGenericCl then mkApp typ [e0]
                                     else mkApp typ [np,e0]
         e2 = mkApp (clType typ) [fromMaybe (mkApp meta []) (isVTense tmp)
                             ,mkApp pol [],e1]
     return (e2,tmp,pol,utType typ)
  `mplus`
 -- find out whether it should be AdvS or FocAdv here!
  do advs <- many pAdv    -- use def to know if it is question or not
     (tmp,pol,np,nptyp,vp) <- pVSOs
     advs2 <- many pAdv
     let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs2
         e1 = mkApp cidUseCl [fromMaybe (mkApp meta []) (isVTense tmp)
                             ,mkApp pol []
                             ,mkApp nptyp [np,e0]
                             ]
         e2 = foldr (\ad e -> mkApp cidAdvS [ad, e]) e1 advs
     return (e2,tmp,pol,cidUttS)
     

pSS =  
  do s1   <- cat "S"    -- jag går om hon kommer
     conj <- inside "UK" pConj
     s2   <- inside "S" pUttAdv 
     return $ mkApp cidSSubjS [s1,conj,s2] 


--pVSOs :: P S String Expr (VForm Expr, CId, Expr, CId, Expr)
pVSOs = foldr1 (mplus) $ map pVSO vForms

--pVSO :: VPForm -> P S String Expr (VForm Expr, CId, Expr, CId, Expr)
pVSO typ = do
  (v,t) <- pSlashVP typ "FV"
           --`mplus`                                 -- we need to deal with unknowns somewhere
           --inside "FV" (consume >> metaVP' typ)    -- slash should find them if the right tag is there!
  (np,nptyp) <- write "looking for SS" >> parseSubject
  write ("AdvCl found np "++show np)
  (tmp,s,pol,vp) <- pComplVP typ v t
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
          return $ mkApp cidUttImpPol [mkApp pol [],imp]

pUttAdv = do 
 sub <- inside "UK" $ pSubj 
 (np,typ) <- parseSubject
 write ("SS done for UttAdv"++show np)
 write "now to pVP"
 pol <- pPol
 (tmp,sim,p,vp) <- (write "goto pVP" >> pVP "FV")
                     `mplus` (write "no VP!" >> inside "FV" consume >> metaVP) -- obs! för passiv
 guard (p==cidPPos)
 advs <- many pAdv
 let e0 = foldr (\ad e -> mkApp cidAdvVP [e,ad]) vp advs
     e1 = if typ == cidGenericCl then mkApp typ [e0]
                                 else mkApp typ [np,e0]
     e2 = mkApp cidUseCl [fromMaybe (mkApp meta []) (isVTense tmp)
                         ,mkApp pol [],e1]
 return $ mkApp cidSubjS [sub,e2]

parseRelS = do 
  w <- inside "S" $ inside "SS" $ word "PO" 
  guard (w =="Som" || w == "som")
  pol <- pPol
  (tmp,sim,p,vp) <- (write "goto pVP" >> pVP "FV")
  let t = fromMaybe (mkApp meta []) (isVTense tmp)
  return (vp,t,mkApp pol []) 
 

pSpecialPP cid = 
 do pr <- inside "PR" $ optEat (lemma "Prep" "s") meta
    write "prep found"
    guard (pr==cid)
    np     <- pflatNP
    return $ mkApp cidPrepNP [mkApp pr [],np]

-- som ...
pRelS = do undefined



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

data VPForm  = Cop | Sup | VV | VA | V | V2 | V2A | V2Pass | Fut | Fut' 
             | VS                           -- Fut : ska
                                            -- Fut': kommer att
  deriving (Eq,Show)

vForms  = [Cop,Sup,Fut,Fut',VV,VA,V2A,V2,V2Pass,VS,V]

pSlashVP V typ =
 do (t,v) <-inside typ $ pVerb "VV" "V"
                         `mplus`
                         liftM (,cidUseCopula) pCopula
             --            `mplus`
             --            inside "VV" (consume >> return metaVerb)
    return (mkApp v [],t)

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
    return (mkApp v [],t)


pSlashVP V2Pass typ =
 do (t,v) <- inside typ pV2Pass
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
 
pSlashVP Fut typ = 
 do t <- inside typ pFuturum
    return (mkApp meta [],t)

pSlashVP Fut' typ = 
 do t <- inside typ pFuturum'
    return (mkApp meta [],t)
pSlashVP VS typ = 
 do (t,v) <- inside typ $ pVerb "VV" "VS"
    return (mkApp v [],t)

mkTmp = mkTmp' cidASimul 
mkTmp' a | a ==cidASimul = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) 
         | a ==cidAAnter = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidAAnter []]) 



-- adv efter verb? före eller efter pol?
-- simplify by using pSlash and pCompVP
pVP typ = 
 do write "try pVP one"    
    -- Copula: is red/a cat...
    pSlashVP Cop typ >>= uncurry (pComplVP Cop) -- >>= write "pVP Cop". return

 `mplus`
  do write "VP supinum"
     -- supinum
     pSlashVP Sup typ >>= uncurry (pComplVP Sup) -- >>= write "pVP Sup". return

 `mplus`
 do write "VP Futurum"
    pSlashVP Fut typ >>= uncurry (pComplVP Fut) -- >>= write "pVP Fut". return
 
 `mplus`
 do write "VP kommer att"            --make nice
    pSlashVP Fut' typ >>= uncurry (pComplVP Fut') -- >>= write "pVP kommer att". return

 `mplus`
 do write "try pVP2A two"
    -- V2: have a cat
    pSlashVP V2A typ >>= uncurry (pComplVP V2A) -- >>= write "pVP V2A". return

 `mplus`
 do write "try pVP two"
    -- V2: have a cat
    pSlashVP V2 typ >>= uncurry (pComplVP V2) -- >>= write "pVP V2". return

 `mplus`
 do write "try VV"
    -- VV: must go, fortsätter att
    pSlashVP VV typ >>= uncurry (pComplVP VV) -- >>= write "pVP VV". return
  `mplus`

  do write "try VA"
     -- VA 
     pSlashVP VA typ >>= uncurry (pComplVP VA) -- >>= write "pVP VA". return
 `mplus`

 do write "try pVP two pass"
    -- V2: have a cat
    pSlashVP V2Pass typ >>= uncurry (pComplVP V2Pass)
 `mplus`

 do write "try pVS"
    -- VS: know that ... 
    pSlashVP VS typ >>= uncurry (pComplVP VS)
 `mplus`

 do write "simple v tries"
     -- V: think
    pSlashVP V typ >>= uncurry (pComplVP V) -- >>= write "pVP Simple". return


pInfVP = 
  do write "att v?"
     -- to go
     do im <- opt (word2 "IM" >> return True) False
        v  <- pVP "IV"
        return (im,v)

--pComplVP :: VPForm -> Expr -> VForm CId -> P S String Expr (VForm Expr, CId, CId, Expr)
pComplVP V vp tmp = do 
  (pol,adv,part,adv1) <- pComplV
  let vp0  = maybe vp (\p -> mkApp p []) part 
      vp1  = mkApp cidUseV [vp0]
      vp2  = maybe vp1 (\a -> mkApp cidAdvVP [vp1,a]) adv 
      vp3  = maybe vp2 (\a -> mkApp cidAdvVP [vp2,a]) adv1
  write ("particle "++show part++" verb "++show vp)
--   when (isJust p) $ guard (mkApp (fromJust p) [] == vp)  -- how to do this right? need lists of verbs/particles to see which fit
  return (mkTmp tmp,cidASimul,pol,vp3) 
pComplVP VA vp tmp = do 
  (pol,adv,a) <- pComplVA
  let vp1  = maybe vp (\a -> mkApp cidAdvVPSlash [vp,a]) adv 
  return (mkTmp tmp,cidASimul,pol,mkApp cidComplVA [vp1,a])
pComplVP VV vp tmp = do
  (pol,adv,iv,p,b) <- pComplVV
  let vv0 = if b then mkApp cidDropAttVV [vp] else vp
      vv1 = maybe vv0 (\p -> mkApp p []) p 
      vv2  = maybe vv1 (\a -> mkApp cidAdvVP [vv1,a]) adv 
--   when (isJust p) $ guard (mkApp (fromJust p) [] == vp)  -- how to do this right? need lists of verbs/particles to see which fit
      vv3 = mkApp cidComplVV [vv2,iv] 
  return (mkTmp tmp,cidASimul,pol,vv3)
pComplVP V2 vp tmp = do
  (pol,adv,obj) <- pComplV2
  isRefl <- gets isReflGenVP 
  write $ "refl? : "++show isRefl
  let compl = if isRefl then cidReflSlash else cidComplSlash
  case (obj,isExistNP vp) of
    (Just o,False) -> do
                      let vp0 = maybe vp (\a -> mkApp cidAdvVPSlash [vp,a]) adv  -- meta :O
                          vp1 = mkApp compl [vp0,o]
                      return (mkTmp tmp,cidASimul,pol,vp1)
    (Just o,True)  -> do
               let vp1 = maybe o (\a -> mkApp cidAdvVPSlash [o,a]) adv
               return (mkTmp tmp,cidASimul,pol,vp1)
    (Nothing,False) -> do                                  -- for exist, maybe not worth the work..
               let vp0 = mkApp cidReflVP [vp]
                   vp1 = maybe vp (\a -> mkApp cidAdvVPSlash [vp0,a]) adv  -- meta :O
               return (mkTmp tmp,cidASimul,pol,vp1)
pComplVP V2A vp tmp = do
  (pol,adv,obj,adj) <- pComplV2A
  let slashVP = mkApp cidSlashV2A [vp,adj]
  case obj of
    Just o  -> do
               let vp0 = maybe slashVP (\a -> mkApp cidAdvVPSlash [slashVP,a]) adv  -- meta :O
                   vp1 = mkApp cidComplSlash [vp0,o]
               return (mkTmp tmp,cidASimul,pol,vp1)
    Nothing -> do
               let vp0 = mkApp cidReflVP [slashVP]
                   vp1 = maybe slashVP (\a -> mkApp cidAdvVPSlash [vp0,a]) adv  -- meta :O
               return (mkTmp tmp,cidASimul,pol,vp1)

pComplVP V2Pass vp tmp = do
  (pol,adv1,agent,eo,adv2) <- pComplV2Pass
  let vp' = foldr (\a vp -> mkApp cidAdvVP [vp,a]) vp $ catMaybes [adv1,agent,adv2]
--      vp0 = maybe vp  (\a -> mkApp cidAdvVP [vp,a]) adv1
--      vp1 = maybe vp0 (\a -> mkApp cidAdvVP [vp0,a]) agent
--      vp2 = maybe vp1 (\a -> mkApp cidAdvVP [vp1,a]) adv2
      vp3 = maybe vp' (\a -> mkApp meta [a]) eo --- wrong! cidExistNP if verb was 'finns' 
  return  (mkTmp tmp,cidASimul,pol,vp3)
pComplVP Sup vp t = do
  (pol,adv,sup,useV) <- pComplSup
  let tmp  = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidAAnter []]) t
      vp0  = mkApp sup []
      vp1  = maybe vp0 (\a -> mkApp cidAdvVPSlash [vp0,a]) adv 
  return (tmp,cidAAnter,pol,mkApp useV [vp1])
pComplVP Cop vp tmp = do
  (pol,adv,sp) <- pComplCopula 
  write ("copula sp "++ show sp)
  let cop = mkApp cidUseComp [sp]
      vp1  = maybe cop (\a -> mkApp cidAdvVPSlash [cop,a]) adv 
  return (mkTmp tmp,cidASimul,pol,vp1)
pComplVP Fut vp t = do
  (pol,adv,v) <- pComplFut
  let vp1  = maybe v (\a -> mkApp cidAdvVPSlash [v,a]) adv 
  write ("fut compl: "++show vp1)
  return (mkTmp t,cidASimul,pol,vp1)
pComplVP Fut' vp t = do
  (pol,adv,v) <- pComplFut'
  let vp0  = mkApp v []
      vp1  = maybe vp0 (\a -> mkApp cidAdvVPSlash [vp0,a]) adv 
  return (mkTmp t,cidASimul,pol,mkApp meta [vp1])
pComplVP VS vp t = do
   (pol,adv,s) <- pComplVS
   let vp0 = mkApp cidComplVS [vp,s]
       vp1 = maybe vp0 (\a -> mkApp cidAdvVP [vp1,a]) adv
   return (mkTmp t,cidASimul,pol,vp1)




pPart v = do write "part right!!"
             inside "AB" (lemma v "part")
          `mplus`
          do write "part" 
             inside "PR" (lemma v "part") -- `mplus` optEat (lemma "Prep" "s") meta)
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
  return (t,mkApp v [])

pVA = do
  (t,v) <- tryVerb "BV" cidBecome_VA "VA"
           `mplus`
           pVerb "FV" "VA" 
  write ("VA returs tense "++show t)
  return (t,mkApp v [])


pV2Act = do 
  (t,v) <- do t <- pHave
              return (t,mkApp cidHave_V2 [])  -- need to look for passive form here too
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
                       tryVerb "BV" cidBecome_V2 "V2"
              return (t,mkApp v [])
  return (t,mkApp cidSlashV2a [v]) 

pV2Pass = do
  (t,v) <- pPassVerb "VV" "V2"
           `mplus`
           tryVerb "GV" cidDo_V2 "V2"
           `mplus`
           tryVerb "FV" cidGet_V2 "V2"
  return (t,mkApp cidPassV2 [mkApp v []])

pExist =
-- do set isExist True
    do lemma "NP -> Cl" "s Pres Simul Pos Main"
       return (VTense cidTPres,mkApp cidExistNP [])
    `mplus`
    do lemma "NP -> Cl" "s Pret Simul Pos Main"
       return (VTense cidTPast,mkApp cidExistNP [])
    `mplus`
    do (lemma "NP -> Cl" "s Pres Anter Pos Main"
        `mplus`
        lemma "NP -> Cl" "s Pret Anter Pos Main")
       return (VSupin,mkApp cidExistNP [])

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


maybeVerbAdv  = maybeParse $ inside "+A" findAdverb
                             `mplus`
                             inside "AA" findAdverb 
maybeParticle = maybeParse . inside "PL" . pPart 

metaVP = do
  let tmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) $ VTense cidTPres  
  return (tmp,cidASimul,cidPPos,mkApp meta [])

--metaVP' :: VPForm -> P S String Expr (Expr,VForm CId)
metaVP' vf = return (mkApp meta [],VTense cidTPres)

metaVerb   = (VInf,meta)


pComplCopula = do
  write "copula compl begins"
  pol <- pPol
  adv <- maybeVerbAdv
  sp <- inside "SP" (do a <- pAdj 
                        write ("adj return"++show a) >> return (mkApp cidCompAP [a])
                     `mplus`
                     do (e,_) <- pNP
                        write ("coplua np "++show e)
                        return (mkApp cidCompNP [e])
                     `mplus`
                     do e <- cat "PP"
                        return (mkApp cidCompAdv [e])
                     `mplus`
                     do consume
                        return (mkApp meta [])) --we know we are in SP, so ok to consume
        `mplus`
        do a <- pAdv
           return (mkApp cidCompAdv [a])
  return (pol,adv,sp)

pComplSup = do
  write "supinum compl begins"
  p <- pPol
  adv <- maybeVerbAdv
  (t',sup,useV) <- inside "IV" $ 
                    do (t,s) <- foldr1 mplus [pVerb "TP" v | v <- ["V","V2"]]  -- inte bara V och V2? 
                                `mplus`
                                inside "TP" (consume >> return (VSupin,meta))
                                `mplus`
                                foldr1 mplus [pVerb "VVSN" v | v <- ["V","V2"]]  -- inte bara V och V2? 
                       return (t,s,cidUseV)
              `mplus`         
               do (t,s) <- foldr1 mplus [pPassVerb "VVSN" v | v <- ["V","V2"]]  -- inte bara V och V2? 
                  return (t,s,cidPassV2)
  guard (isVSupin t') -- && pol == cidPPos)
  return (p,adv,sup,useV)

pComplV2 = do
  write "v2 compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol <- pPol
  write "oo pol ok"
  adv <- maybeVerbAdv
  obj <- do inside "OO" $ word "POXPHH"  
            return (Nothing) -- sig
         `mplus`
         liftM (Just .fst) (inside "OO" pNP)
         `mplus`
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
            return (Nothing) -- sig
         `mplus`
         liftM (Just . fst) (inside "ES" pNP)
                                                         -- then a. how to combine them?
  write "oo ok"
  return (pol,adv,obj)
pComplV2A = do
  write "v2a compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol <- pPol
  write "oo pol ok"
  adv <- maybeVerbAdv
  obj <- do inside "OO" $ word "POXPHH"  
            return (Nothing) -- sig
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
  return (pol,adv,obj,adj)


pComplV2Pass = do
  write "v2pass compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol  <- pPol
  adv1 <- maybeVerbAdv
  eo   <- maybeParse $ liftM fst (inside "ES" pNP)
  ag   <- maybeParse $ inside "AG" $ pSpecialPP cidBy8agent_Prep
  adv2 <- maybeVerbAdv
  write "agent ok"
  return (pol,adv1,ag,eo,adv2)

-- dropAtt only needed for some verbs.. More checking?
pComplVV = do
  write "vv compl begins"
  pol   <- pPol
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  write "pol ok"
  adv <- maybeVerbAdv
  (t',s,p,iv,b)  <- do (t,s,p,i) <- pVP "IV"
                       return (t,s,p,i,False)   
                   `mplus`
                   do (im,(t,s,p,i)) <- inside "OO" $ inside "VP" pInfVP 
                      return (t,s,p,i,im) 
                   `mplus`
                   do (t,s,p,i) <- inside "OO" (inside "NAC" $ pVP "IV")
                      return (t,s,p,i,True)
  write ("iv found "++show iv)
  guard (t'==VInf)  
  guard (p==cidPPos)  -- man får inte säga 'jag vill inte (inte tänka)'
  write "iv ok"
  p <- maybeParticle "VV"
  write ("particle: "++show p)
  return (pol,adv,iv,p,b)

pComplVA = do
  write "va compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  pol   <- pPol
  adv   <- maybeVerbAdv
  a     <- inside "SP" pAdj
  return (pol,adv,a)

pComplV = do
  write "v-simple compl begins"
  pol <- pPol
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  adv <- maybeVerbAdv
  p   <- maybeParticle "V"
  write ("particle: "++show p)
  adv1  <- maybeParse $ inside "OA" $ cat "PP"
  return (pol,adv,p,adv1)

pComplFut = do
  write "futurum compl begins"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  p   <- pPol
  adv <- maybeVerbAdv
  (t',s,p',iv) <- pVP "IV" -- optEat (pVerb "VV" "V") (VInf,meta) -- inte bara V 
  write ("comlpfut "++show iv)
 -- guard $ p ==cidPPos
  return (p,adv,iv)
pComplFut' = do
  write "futurum compl begins 'komma att'"
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  p   <- pPol
  word2 "IM"
  adv <- maybeVerbAdv
  (t',iv) <- inside "IV" $ optEat (pVerb "VV" "V") (VInf,meta) -- inte bara V 
  return (p,adv,iv)

pComplVS = do
  write "VS compl "
  pol   <- pPol
  opt (word2 "FO") ""          -- dummy object, alla/själva. ?
  write "pol ok"
  adv       <- maybeVerbAdv
  (s,t,p,_) <- inside "OO" $
                      inside "S" $ do conj <- inside "UK" pSubj
                                      pCl
  write "s in vs ok"
  return (pol,adv,s)



maybeParse = flip opt Nothing . liftM Just  

pflatNP =
  do write "in NP with Adj"
     -- good cars
     m_predet     <- maybeParse $ inside "+A" pPredet
                                  `mplus`
                                  inside "CA" pPredet
                                  `mplus`
                                  inside "DT" pPredet
     m_det        <- maybeParse $ inside "DT" pDet
     m_sitt       <- maybeParse $ inside "DT" pDetRefl  
     m_n2         <- maybeParse $ inside "DT" pN2 -- antal
     m_a          <- maybeParse $ inside "AT" pAdj
     (noun,n,def) <- inside "HD" pCN
     m_pt         <- maybeParse $ inside "PT" consume  -- no way of parsing 'själv' yet
     et           <- many $ inside "ET" $ cat "PP"
     m_app        <- maybeParse $ inside "AN" pAppos
     m_relCl      <- maybeParse $ do opt (word2 "IK") ""
                                     inside "EF" $ parseRelS
     opt (word2 "IP") ""
     let cn  = mkApp noun [] 
         cn0 = maybe cn (\x -> mkApp meta [cn,mkApp meta []]) m_pt  -- kvinnan själv'
         cn1 = case m_a of
                   Just a  -> mkApp cidAdjCN [a,mkApp cidUseN [cn0]]
                   Nothing -> mkApp cidUseN [cn0]
         num = mkApp n []
         d   = fromMaybe (mkApp cidDefArt []) m_det
         cn2 = maybe cn1 (\app -> mkApp cidApposCN [cn1,app]) m_app
     np0 <- case (m_sitt,def,m_det) of
                (Just (),_,_)   -> return $ mkApp cidReflCN [num,cn2]
                (_,     NDef,_) -> return $ mkApp cidDetCN 
                                             [mkApp cidDetQuant [d,num]
                                             ,cn2]
                (_,NIndef,Nothing) -> if n == cidNumSg 
                            then return (mkApp cidMassNP [cn2])
                            else return $ mkApp cidDetCN 
                                            [mkApp cidDetQuant 
                                            [mkApp cidIndefArt [],num],cn2]
                (_,NIndef,Just d)  -> return $ mkApp cidDetCN [d,cn2]
                (_,NOther,_)       -> do guard (isNothing m_predet && isNothing m_det) --ok?
                                         return $ mkApp noun []
     let np' = maybe np0 (\(n2,num,def) -> mkApp cidDetCN [mkApp cidDetQuant [def,num]
                                                          ,mkApp cidComplN2 [n2,np0]]) m_n2
         np1 = maybe np' (\p -> mkApp cidPredetNP [p,np']) m_predet
         np2 = maybe np1 (\(vp,t,p) -> mkApp cidRelNP' [np1,vp,t,p]) m_relCl

     return $ foldr (\e n -> mkApp cidAdvNP [n,e]) np2 et 
  `mplus`
  do (noun,n,def) <- inside "HD" pCN
     guard $ def == NIndef && n == cidNumSg  -- stämmer ej för 'våningarna 8 och 9'
     num <- pNumber
     return $ mkApp cidCNNumNP [mkApp cidUseN [mkApp noun []],num]
                 
-- returns (word :: CId, number :: CId, determined :: NounForm)
pCN = 
     inside "VN" pNoun
     `mplus`
     do n <- inside "NN" (optEat pNoun metaNoun)  --optEat eller ej?
        write ("pCN gives NN "++show n) >> return n
     `mplus`
     inside "AN" pNoun
     `mplus`
     do word "NNDD"
        return (meta,cidNumSg,NDef)  --kan vara Pl också..
     `mplus`
     do w <- inside "POCP" consume  -- varandra, reciprokt! ej i GF
        return (meta,cidNumPl,NOther)
     `mplus`
     do write "test for particip"
        (part,num,def) <- inside "SP" findNParticip
        return $ (part,num,def)
     `mplus`
     do write "testing last pCN"
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
  deriving (Eq,Show)

isDef :: NForm -> Bool
isDef NIndef = True
isDet _      = False


parseSubject = inside "SS" (optEat pNP (mkApp meta [],cidPredVP))
               `mplus` 
               inside "FS"  
                  (do w <- inside "PO" $ lemma "VP -> Cl" "s Pres Simul Pos Main"
                      write "imperson hittad!!"
                      return (mkApp w [],cidImpersCl))
                  `mplus`
                  (do w <- inside "PO" $ lemma "NP -> Cl" "s Pres Simul Pos Inv"
                      return (mkApp w [],cidExistNP))

pItPron = 
 do p <- inside "POOP" $ lemma "Pron" "s NPNom"
    return $ mkApp p []
 
pPN = do n <- inside "PN" $ optEat (lemma "PN" "s Nom") cidName
         return $ mkApp n []
pNP = 
  (cat "NP" >>= \x -> write ("cat np "++show x) >> return (x,cidPredVP))  --här kanske vi behöver tänka mer ang PredVP
  `mplus` 
  (cat "AP" >>= \x -> return (x,cidPredVP))  --och här med
  `mplus` 
   do name <- pPN
      return (mkApp cidUsePN [name],cidPredVP)
  `mplus` 
   do w   <- inside "PO" $ lemma "IP" "s NPNom"
      return (mkApp w [],cidQuestVP)
            {- `mplus`  Ha med detta?
             inside "POFP" $ lemma "IQuant" "s" -}
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
      write "lemma ok"
      return (mkApp cidUsePron [mkApp w []],cidPredVP)
   `mplus`
   do w <- inside "PO" $ lemma "VP -> Cl" "s Pres Simul Pos Main"
      write "Man hittad!!"
      return (mkApp w [],cidGenericCl)
   `mplus`
   do det <- pQuant
      return (mkApp cidDetNP [det],cidPredVP)

   `mplus`
   do 
      write "in complicated np"
      (n,num,def) <- pCN 
      let cn   = (mkApp cidUseN [mkApp n []])
          nums = mkApp num []
      e0 <- case def of
                 NDef -> return $ mkApp cidDetCN 
                                   [mkApp cidDetQuant 
                                   [mkApp cidDefArt [], nums],cn]
                 NIndef -> if num==cidNumPl then return cn
                                            else return (mkApp cidMassNP [cn])
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
  do ad <- findA2
     return $ mkApp cidUseA2 [ad]
  `mplus`
  do write "will check AP"
     cat "AP"
  `mplus`
  cat "CAP" 
  `mplus`
  do a <- inside "TP" $ optEat findAPerfParticip meta
     return (mkApp meta [mkApp a []])
  
findAdj = 
  do ad <- inside "AJ" (optEat findA meta)
           `mplus`
           do write "found particip adjective"
              inside "SP" findA
     return $ mkApp ad []
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
     return $ mkApp ad []

-- not in gf :O!
-- make function and simplify
findNParticip = pNoun -- consume >> return meta
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
                        return (mkApp ada [])

pAdv = 
  do write "looking for adv"
     (inside "RA" $ (findAdverb `mplus` cat "PP" `mplus` cat "NP"))
  `mplus`
  do write "looking for adv" 
     (inside "TA" $ (findAdverb `mplus` cat "PP" `mplus` cat "NP"))
  `mplus`
  do write "looking for adv" 
     (inside "MA" $ (findAdverb `mplus` cat "PP" `mplus` cat "NP"))
  `mplus`
  do write "looking for adv" 
     (inside "CA" $ (findAdverb )) -- denna med? `mplus` cat "PP"))
  `mplus`
  do write "looking for adv in AA1"
     (inside "AA" (cat "PP" `mplus` pAdvAdj 
                            `mplus` findAdverb
                            `mplus` inside "S" pUttAdv))
--  `mplus`
--  write "looking for adv in AA2" >> (inside "AA" pAdvAdj)
  `mplus` inside "+A"  findAdverb 
  `mplus` inside "OO" (cat "NP")

findAdverb = do
  a <- inside "AB" $ optEat (lemma "Adv" "s") meta
  return (mkApp a []) 
 
pAdvAdj = do
  a <- findAdj
  return $ mkApp cidPositAdvAdj [a]
 
pAdAdj = liftM (\a -> mkApp cidPositAdAAdj [a]) findAdj         

pQuant =
  inside "PO" (   -- fler taggar än PO?                         
    do dt <-          lemma "Quant" "s Sg False False Utr"  -- dessa två ej helt testade
              `mplus` lemma "Quant" "s Sg False False Neutr"
       write ("det: "++show dt)
       return $ (mkApp cidDetQuant [mkApp dt [],mkApp cidNumSg []]) 
    `mplus`
     do dt <-         lemma "Quant" "s Pl False False Utr"
              `mplus` lemma "Quant" "s Pl False False Neutr"
        write ("det: "++show dt)
        return $ (mkApp cidDetQuant [mkApp dt [],mkApp cidNumPl []]))
  `mplus`
  do dt <- inside "PO" $ lemma "Pron" "s (NPPoss GPl Nom)"
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkApp dt []],mkApp cidNumPl []]
  `mplus`
  do dt <- inside "PO" $ mplus (lemma "Pron" "s (NPPoss (GSg Neutr) Nom)")
                               (lemma "Pron" "s (NPPoss (GSg Utr) Nom)")
     return $ mkApp cidDetQuant [mkApp cidPossPron [mkApp dt []],mkApp cidNumSg []]
  `mplus`
  do n <- pNumber 
     return $ mkApp cidDetQuant [mkApp cidIndefArt [],mkApp cidNumCard [n]]


pDetRefl =         
  do w <- word "POXP" 
     write "setting it to true"
     isReflGenVP =: True
     t <- gets isReflGenVP
     write $ "it is " ++ show t
     return () -- $ mkApp cidReflGenVP [] 
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
       write ("det: "++show dt)
       return $ mkApp dt [])
  `mplus`
  do dt <- inside "PO" $ lemma "Det" "s False Utr"
     write ("det: "++show dt)
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
  do n <- pNumber 
     return $ mkApp cidDetQuant [mkApp cidIndefArt [],mkApp cidNumCard [n]]

pN2 = 
  -- hur vill gf ha det här?
 -- cat "NP"
 -- `mplus`
  inside "NNDD" (do n <- lemma "N2" "s Pl Def Nom"
                    return (mkApp n [],mkExpr cidNumPl,mkExpr cidDefArt)--(n,cidNumPl))
                  `mplus`
                  do n <- (lemma "N2" "s Sg Def Nom") 
                     return (mkApp n [],mkExpr cidNumSg,mkExpr cidDefArt) --mkApp n [])) --(n,cidNumSg)))
                  `mplus`
                  do n <- (lemma "N2" "s Sg Indef Nom") 
                     return (mkApp n [],mkExpr cidNumSg,mkExpr cidIndefArt) --mkApp n [])) --(n,cidNumSg)))
                  `mplus`
                  do n <- (lemma "N2" "s Pl Indef Nom") 
                     return (mkApp n [],mkExpr cidNumPl,mkExpr cidIndefArt)) --mkApp n [])) --(n,cidNumSg)))
     --return $ mkApp cidDetQuant [mkApp cidComplN2 [mkApp dn []],mkApp num []]

-- how to handle this? could be a lot of things..
pAppos = do inside "XP" $ consume 
            return (mkApp meta [])

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
    
pSubj = do 
  s <- inside "UK" $ optEat (lemma "Subj" "s") meta
  return $ mkApp s [] 

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
           return $ mkApp p []

-- här behöver vi kanske kunna ha bla Adv, som 'även'. hur?
pPredet = 
  do w <- findPredet
     return $ mkApp w [] 
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
                       p <- (wordlookup w "Predet" "s Neutr Pl"
                             `mplus`
                             wordlookup w "Predet" "s Utr Pl"
                             `mplus`
                             wordlookup w "Predet" "s Utr Sg"
                             `mplus`
                             wordlookup w "Predet" "s Neutr Sg")
                       return p
                       


-- translate all numers to 1. could also be NumNumeral ( num (pot... n1))
pNumber = 
  inside "RO" $ do consume
                   return $ mkApp cidNumDigits 
                          [mkApp cidIDig 
                             [mkApp cidD_1 []]] 

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
     x1   <- inside "CJ" f
     conj <- inside "++" pConj 
     x2   <- inside "CJ" f
     let compXs x y = mkApp consf [x,y]
         conjs      = foldr  compXs (mkApp basef [x1,x2]) xs
     return $ mkApp conjf [conj, conjs]


-----
adjSN = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
adjSU = "s (AF (APosit (Strong (GSg Utr))) Nom)"
adjSPl = "s (AF (APosit (Strong GPl)) Nom)"
adjWPl = "s (AF (APosit (Weak Pl)) Nom)"
adjWSg = "s (AF (APosit (Weak Sg)) Nom)" 

isExistNP = (==mkApp cidExistNP [])
meta = mkCId "?"
mkExpr x = mkApp x []


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
