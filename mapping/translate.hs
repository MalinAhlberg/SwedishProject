import Monad
import Idents
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
-- man måste ha två paranteser i början av indatan..:/
test = True

main = do
  pgf <- readPGF "../gf/BigTest.pgf"
  let Just language = readLanguage "BigTestSwe"
      morpho        = buildMorpho pgf language
  s <- fmap concat $ Form.parse "test.xml"
  ref <- trace (show $ prune $ head s) $ newIORef (0,0,0)
  mapM_ (process pgf morpho ref) ((if test then take 15 else id) s)
  where
    process pgf morpho ref t = do
      (cn,co,l) <- readIORef ref
      let e         = parse penn pgf morpho (prune t)
          (cn',co') = count (cn,co) e
          l'        = l+1
      writeIORef ref (cn',co',l')
      hPutStrLn stdout (showExpr [] e)
      when test $ do
        writeFile ("tmp_tree.dot") (graphvizAbstractTree pgf (True,False) e)
        rawSystem "dot" ["-Tpdf", "tmp_tree.dot", "-otrees/tree"++showAlign l'++".pdf"]
        return ()
      hPutStrLn stderr (show ((fromIntegral cn' / fromIntegral co') * 100))

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
  return $ [(lemma,an,cat) | (lemma,an) <- lookupMorpho morpho str
                   ,let cat = maybe "" (showType []) (functionType pgf lemma)]


penn :: Grammar String Expr
penn =
  grammar (mkApp meta) 
   ["S" :-> do trace "hej" $ return () 
               np   <- trace "looking for SS" $ inside "SS" pSS
               trace ("SS done "++show np) return ()
               advs <- many $ cat "OA"
               trace "now to pVP" return ()
               (tmp,sim,pol,vp) <- trace "goto pVP" pVP  
               opt (word2 "IP") ""
               let e0 = mkApp cidUseCl [fromMaybe (mkApp meta []) (isVTense tmp)
                                       ,mkApp pol []
                                       ,mkApp cidPredVP [np,vp]
                                       ]
                   e1 = foldr (\ad e -> mkApp cidAdvS [ad, e]) e0 advs
               return e1

     -- will probably not work since the categoris (PP tex) are inside others (AA tex)          
     ,"AP" :-> do ad <- pAdA
                  a  <- pAdj
                  return $ mkApp cidAdAP [ad,mkApp cidPositA [a]]
               `mplus`
               do as <- many pAdj
                  a2 <- pAdj 
                  return (foldr (\ada ap -> mkApp cidAdAP [ada,ap]) (mkApp cidPositA [a2]) as)
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
      ,"CAP" :-> do as   <- listOf pAdj -- many $ do a <- inside "CJ" $ pAdj 
                                    --  word2 "IK"
                                     -- return a
                    a1   <- inside "CJ" $ pAdj
                    conj <- inside "++" pConj 
                    a2   <- inside "CJ" $ pAdj
                    let compas x y = mkApp cidConsAP [x,y]
                        conjs  = foldr  compas (mkApp cidBaseAP [a1,a2]) as
                    return $ mkApp cidConjAP [conj, conjs]
      ,"CNP" :-> do n1 <- inside "CJ" $ pAdj -- pN
                    conj <- inside "++" pConj 
                    a2   <- inside "CJ" $ pAdj -- pN
                    return $ mkApp cidConjNP [conj, mkApp cidBaseAP [n1,a2]]
      --,"NP" :-> 
     -- ,"CAVP" :-> do 
   --, "CC"  :-> do cc <- lemma "Conj" "s2"
    --              return (mkApp cc [])
{-    , "SS" :-> do dep <- getDep 
                  w   <- lemma "Pron" $ "s "++order dep
                  trace "lemma ok" return ()  --          s Pres Simul Pos Inv
                  return (mkApp cidUsePron [mkApp w []])
   , "AV"  :-> do -- dep <- trace "in AV" getDep
                  ar <- cat "ROOT" --eller VInf inte root alltid + att man borde se tempus mm här
                  (tmp,pol,vp) <- trace "catAv ok" $ inside "SP" pVP --kanske en speciell med bara AJ osv? nej..
                  return (mkApp cidUseComp [vp])
   , "ADVP":-> do adv <- cat "RB"
                  case unApp adv of
                    Just (f,[a]) | f == cidPositAdvAdj -> return (mkApp cidPositAdVAdj [a])
                    _                                  -> mzero
   , "ADJP":-> do adas <- many pAdA
                  adj  <- cat "JJ"
                  return (foldr (\ada ap -> mkApp cidAdAP [ada,ap]) (mkApp cidPositA [adj]) adas)
   , "S"   :-> do advs <- many $ do
                            pp <- cat "PP"
                            word ","
                            return pp 
                  np <- cat "NP"
                  (tmp,pol,vp) <- do (tmp,pol,vp) <- inside "VP" pVP
                                     return (isVTense tmp,pol,vp)
                                  `mplus`
                                  do vp <- cat "VP"
                                     return (Nothing,meta,vp)
                  opt (word ".") ""
                  let e0 = mkApp cidUseCl [fromMaybe (mkApp meta []) tmp
                                          ,mkApp pol []
                                          ,mkApp cidPredVP [np,vp]
                                          ]
                      e1 = foldr (\ad e -> mkApp cidAdvS [ad, e]) e0 advs
                  return e1
   , "NP"  :-> do (m_cc,list_np) <- pBaseNPs
                  case m_cc of
                    Just cc -> return (mkApp cidConjNP [cc, list_np])
                    Nothing -> return list_np
               `mplus`
               do (m_cc,list_np) <- pNPs
                  case m_cc of
                    Just cc -> return (mkApp cidConjNP [cc, list_np])
                    Nothing -> return list_np
   , "VP"  :-> do (_,_,e) <- pVP
                  return e
   , "PP"  :-> do prep <- do cat "IN"
                          `mplus`
                          do word "TO"
                             return (mkApp cidto_Prep [])
                          `mplus`
                          do w1 <- word "JJ"
                             w2 <- word "IN"
                             guard (w1 == "such" && w2 == "as")
                             return (mkApp cidsuch_as_Prep [])
                  np   <- cat "NP"
                  return (mkApp cidPrepNP [prep,np])
   , "CC"  :-> do cc <- lemma "Conj" "s2"
                  return (mkApp cc [])
   , "DT"  :-> do (dt,b) <- pDT
                  return dt
   , "IN"  :-> do prep <- lemma "Prep" "s"
                  return (mkApp prep [])
   , "NN"  :-> do transform (concatMap splitDashN)
                  (do n <- lemma "N" "s Sg Nom"
                      (do word "-"
                          n2 <- lemma "N" "s Sg Nom"
                          return (mkApp cidDashCN [mkApp n [], mkApp n2 []])
                       `mplus`
                       do return (mkApp n [])))
               `mplus`
               do v <- lemma "V" "s VPresPart"
                  return (mkApp cidGerundN [mkApp v []])
   , "NNS" :-> do transform (concatMap splitDashN)
                  (do n <- lemma "N" "s Pl Nom"
                      return (mkApp n [])
                   `mplus`
                   do n1 <- lemma "N" "s Sg Nom"
                      word "-"
                      n2 <- lemma "N" "s Pl Nom"
                      return (mkApp cidDashCN [mkApp n1 [], mkApp n2 []]))
   , "PRP" :-> do p <- lemma "Pron" "s (NCase Nom)"
                  return (mkApp p [])
   , "PRP$":-> do p <- lemma "Pron" "s (NCase Gen)"
                  return (mkApp cidPossPron [mkApp p []])
   , "RB"  :-> do a <- lemma "A" "s AAdv"
                  return (mkApp cidPositAdvAdj [mkApp a []])
               `mplus`
               do adv <- lemma "Adv" "s"
                  return (mkApp adv [])
   , "-NONE-"
           :-> return (mkApp meta [])
   , "JJ"  :-> do a <- lemma "A" "s (AAdj Posit Nom)"
                  return (mkApp a [])
   , "JJR" :-> do a <- lemma "A" "s (AAdj Compar Nom)"
                  return (mkApp a [])
   , "JJS" :-> do a <- lemma "A" "s (AAdj Superl Nom)"
                  return (mkApp a [])
   , "VB"  :-> do v <- mplus (lemma "V" "s VInf")  (lemma "V2" "s VInf")
                  return (mkApp v [])
   , "VBD" :-> do v <- mplus (lemma "V" "s VPast") (lemma "V2" "s VPast")
                  return (mkApp v [])
   , "VBG" :-> do v <- mplus (lemma "V" "s VPresPart") (lemma "V2" "s VPresPart")
                  return (mkApp v [])
   , "VBN" :-> do v <- mplus (lemma "V" "s VPPart") (lemma "V2" "s VPPart")
                  return (mkApp v [])
   , "VBP" :-> do v <- mplus (lemma "V" "s VInf") (lemma "V2" "s VInf")
                  return (mkApp v [])
   , "VBZ" :-> do v <- mplus (lemma "V" "s VPres") (lemma "V2" "s VPres")
                  return (mkApp v [])
   , "PDT" :-> do pdt <- lemma "Predet" "s"
                  return (mkApp pdt [])
                  -}
   ]


order :: String -> String
order "SS" = "NPNom"
order "ROOT" = "SP" -- annars kanske "OP"
order _    = "Main"   --OBS!!!


data VForm a
  = VInf | VPart | VSupin | VTense a

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

--getVTemp "IP" = cidTImp
getVTemp "IV" = ("VInf",VInf)
getVTemp "PK" = ("VPres",VTense cidTCond) --fel fst
getVTemp "PS" = ("VPres",VTense cidTPres)
getVTemp "PT" = ("VPast",VTense cidTPast)
getVTemp "SN" = ("VPast",VTense cidTPast) -- fel = supinum
getVTemp _    = ("VInf",VInf) --Nothing

pSS =  
  do
     w   <- inside "PO" $ lemma "Pron" $ "s NPNom" -- ++order dep
     trace "lemma ok" return () 
     return (mkApp cidUsePron [mkApp w []])
 {-`mplus`
     cat "NP"
     -}

pVP = 
 do trace "try pV2 one" $ return ()
    t <- inside "FV" $ pCopula 
    pol <- trace "FV done" pPol
    sp <- inside "SP" $ do a <- pAdj 
                           trace "adj return" $ return (mkApp cidCompAP [a])
                        `mplus`
                        do e <- cat "NP"
                           return (mkApp cidCompNP [e])
                        `mplus`
                        do e <- cat "PP"
                           return (mkApp cidCompAdv [e])
    let tmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) t
    return (tmp,cidASimul,pol,mkApp cidUseComp [sp])
 `mplus`
 do trace "try pV2 two" $ return ()
    (t,v) <- inside "FV" $ pV2 
    trace "have ok" $ return ()
    pol <- pPol
    trace "pol ok" $ return ()
    obj <- inside "OO" $ pNP
    trace "oo ok" $ return ()
    let tmp = fmap (\t -> mkApp cidTTAnt [mkApp t [],mkApp cidASimul []]) t
    return (tmp,cidASimul,pol,mkApp cidComplSlash [v,obj])

pV2 = do 
  (t,v) <- do t <- pHave
              return (t,mkApp cidHave_V2 [])
           `mplus`
           do (t,v) <- inside "VV"  (pVerb "V2")  -- man skulle kunna kolla mer på taggarna här
                       `mplus`                   
                       tryVerb "FV" cidGet_V2 "V2"
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
  inside tag (pVerb cat)


pVerb cat =
  do v <- optEat (lemma cat "s (VF (VPres Act))") meta
     return (VTense cidTPres,v)
  `mplus`
  do v <- optEat (lemma cat "s (VI (VInfin Act))") meta
     return (VInf,v)
  `mplus`
  do v <- optEat (lemma cat "s (VF (VPret Act))") meta
     return (VTense cidTPast,v)
  `mplus`
  do v <- optEat (lemma cat "s (VI (VSupin Act))") meta
     return (VSupin,v)

{-
pNP = 
 cat "NP"
 `mplus` 
  do n <- do trace "VN" $ inside "VN" pNoun
             `mplus`
             inside "NN" pNoun
     return $ mkApp cidUseN [mkApp n []]
     -}
 
-- may use tag, "xx    GG" = genitiv

pCN = 
  do      n <- (mplus (optEat (lemma "N" "s Pl Indef Nom") meta)
                      (optEat (lemma "N" "s Pl Indef Gen") meta))
          return (n,cidNumPl,False)
  `mplus` do
     n <- (mplus (optEat (lemma "N" "s Sg Indef Nom") meta)
                      (optEat (lemma "N" "s Sg Indef Gen") meta))
     return (n,cidNumSg,False)
  `mplus` do
          n <- (mplus (optEat (lemma "N" "s Sg Def Nom") meta)
                      (optEat (lemma "N" "s Sg Def Gen") meta))
          return (n,cidNumSg,True)
  `mplus` do
          n <- (mplus (optEat (lemma "N" "s Pl Def Nom") meta)
               (optEat (lemma "N" "s Pl Def Gen") meta))
          return (n,cidNumPl,False)

pNP = 
  cat "NP"
  `mplus` 
  do m_q   <- return Nothing  --opt (liftM Just pQuant) Nothing   -- lägg till svenska här!
     m_num <- return Nothing --- opt (liftM Just pCD   ) Nothing   -- och här!!
     (n,num,def) <- pCN
     let cn   = (mkApp cidUseN [mkApp n []])
         {-cn    = foldr (\adj e -> mkApp cidAdjCN [adj, e]) 
                       cn0
                       adjs-}
         nums  = fromMaybe (mkApp num []) m_num
         defs  = (m_num,def)
          
     e0 <- case defs of
                (Just q,_   )   -> return $ mkApp cidDetCN
                                                  [mkApp cidDetQuant [q,nums],cn]
                (_     ,True)   -> return $ mkApp cidDetCN 
                                                  [mkApp cidDetQuant 
                                                  [mkApp cidDefArt [], nums],cn]
                (Nothing,False) -> if num == cidNumSg 
                                      then do guard (isNothing m_num)
                                              return (mkApp cidMassNP [cn])
                                       else return $ mkApp cidDetCN 
                                                       [mkApp cidDetQuant 
                                                       [mkApp cidIndefArt [],nums],cn]
    {- let e1 = case m_pdt of
                Just pdt -> mkApp cidPredetNP [pdt,e0]
                Nothing  -> e0-}
     return  e0
    

pAdj = do 
  ad <- (inside "AJ" $ lemma "A" adjAn)
        `mplus`
        (inside "HD" $ lemma "A" adjAn)
  return $ mkApp cidPositA [mkApp ad []]
  `mplus`
  cat "AP"
  `mplus`
  cat "CAP" 

pAdA = do ada <- inside "ABJA" $ lemma "AdA" "s"
          return (mkApp ada [])

pConj = 
  do s <- word "++OC"
     return $ mkApp cidAndConj []
  `mplus`
  do s <- word "++EL"
     return $ mkApp cidOrConj []
  `mplus`
  do s <- inside "++" $ lemma "Conj" "s2"
     return (mkApp s [])

pCopula = tense "AV"
pHave   = trace "have" $ tense "HV"  
  
tense cat =
  do word $ cat++"IV"    
     return VInf
  `mplus`
  do word $ cat++"PK"   -- ??
     return VPart
  `mplus`
  do word $ cat++"PS"
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
   




pPol =
  do w  <- cat "NA"
    -- guard (w == "inte" || w == "not") -- andra ord?
     return cidPNeg
  `mplus`
  do return cidPPos

listOf f = 
  many $ do
   a <- inside "CJ" $ pAdj 
   word2 "IK"
   return a


-----
adjAn = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
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
meta = mkCId "?"
