----------------------------------------------------------------------
-- |
-- Module      : NounRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
--
-- Swedish noun functions
-------------------------------------------------------------------------
module NounRulesSw where

import GenRulesSw
import General
import TypesSw
import Char(isDigit)
import List

type NSg    = String
type NSgD   = String 
type NPl    = String
type NPlD   = String
type NComp  = String
type NDeriv = String 

-- General Noun functions

mk_substantive :: NSg -> NSgD -> NPl -> NPlD -> NComp -> NDeriv -> Substantive
mk_substantive apa apan apor aporna ap_comp ap_deriv = 
 variants (mk_substantive_v (lift apa) (lift apan) (lift apor) 
                     (lift aporna) (lift ap_comp) (lift ap_comp) (lift ap_deriv)) [(MedComposite,strings (compound_s ap_comp))]

mk_substantive_comp :: NSg -> NSgD -> NPl -> NPlD -> NComp -> NComp -> NDeriv -> Substantive
mk_substantive_comp apa apan apor aporna ap_comp ap_comp2 ap_deriv a = 
    mk_substantive_v (lift apa) (lift apan) (lift apor) 
                     (lift aporna) (lift ap_comp) (lift ap_comp2) (lift ap_deriv) a

mk_substantive_v :: [NSg] -> [NSgD] -> [NPl] -> [NPlD] -> [NComp] -> [NComp] -> [NDeriv]-> Substantive
mk_substantive_v apa apan apor aporna ap_comp ap_comp2 ap_deriv a = 
        case a of
          SF n s c -> strings $ mk_case_v c $ 
                       case (n,s) of
                         (Sg,Indef) -> apa
                         (Sg,Def)   -> apan
                         (Pl,Indef) -> apor
                         (Pl,Def)   -> aporna
          InitComposite    -> strings ap_comp
          MedComposite     -> strings ap_comp2
          SMS              -> nonExist
          Deriv            -> strings ap_deriv

mk_nna :: [String] -> [String] -> [String] -> [String] ->
          [String] -> [String] -> [String] -> [String] -> Substantive
mk_nna apa apas apan apans apor apors aporna apornas a = strings $
        case a of
          SF n s c -> 
                       case (n,s,c) of
                         (Sg,Indef,Nom) -> apa
                         (Sg,Indef,Gen) -> apas
                         (Sg,Def,Nom)   -> apan
                         (Sg,Def,Gen)   -> apans
                         (Pl,Indef,Nom) -> apor
                         (Pl,Indef,Gen) -> apors
                         (Pl,Def,Nom)   -> aporna
                         (Pl,Def,Gen)   -> apornas
          InitComposite   -> map (++ "-") apa
          MedComposite    -> map (++ "-") apa
          SMS             -> map (++ "-") apa
          Deriv           -> map (++ "-") apa

mk_subst :: NSg -> NSgD -> NPl -> NPlD -> Substantive
mk_subst bil bilen bilar bilarna = 
    mk_substantive bil bilen bilar bilarna bil bil

mk_subst_v :: [NSg] -> [NSgD] -> [NPl] -> [NPlD] -> [NComp] -> Substantive
mk_subst_v bil bilen bilar bilarna bilc = 
    variants (mk_substantive_v bil bilen bilar bilarna bilc bilc bilc) [(MedComposite,strings (concat (map compound_s bilc)))]

mk_case :: Casus -> String -> String
mk_case c [] = []
mk_case c s = case c of
  Nom                          -> s
  Gen | elem (last s) "sxzSXZ" -> s
  Gen | elem (last s) "?!" || isDigit (last s) -> s ++ ":s"
  Gen                          ->  s ++ "s"

mk_case_v :: Casus -> [String] -> [String]
mk_case_v c xs = concat $ map f (filter (not.null) xs)
  where f s = case c of
               Nom                                          -> [s]
               Gen | not (null s) && elem (last s) "sxzSXZ" -> [s]
               Gen | not (null s) && (elem (last s) "!?" || isDigit (last s)) -> [s ++ ":s",s++"-s"]
               Gen                                          -> [s ++ "s"]

hyphenate_compounds :: Substantive -> Substantive
hyphenate_compounds f = compvariants [w +? "-" | w <- unStr (f InitComposite)]
                                     [w +? "-" | w <- unStr (f MedComposite)] f
                                     
geminator :: Substantive -> Substantive
geminator f 
 | and [isSuffixOf "mm" w | w <- unStr (f (SF Sg Indef Nom))] = f
 | otherwise = compexcept' [ungeminate w  | w <- unStr (f InitComposite)] [ungeminate w | w <- unStr (f MedComposite)] f

-- additional compound form
compvariants :: [NComp] -> [NComp] -> Substantive -> Substantive
compvariants compi compm f = variants f [
                              (InitComposite,strings compi),
                              (MedComposite,strings compm),
                              (SMS,strings [ x | x@(_:_) <- compi,last x=='-'])
                             ] 

compvariant :: [NComp] -> Substantive -> Substantive
compvariant compi f = variants f [(InitComposite,strings compi), (MedComposite,strings (concat [compound_s w | w <- compi]))]

-- replace compound form
compexcept ::  [NComp] -> Substantive -> Substantive
compexcept compi f = excepts f [(InitComposite,strings compi), (MedComposite,strings (concat [compound_s w | w <- compi]))]

compexcept' ::  [NComp] -> [NComp] -> Substantive -> Substantive
compexcept' compi compm f = excepts f [(InitComposite,strings compi), (MedComposite,strings compm)]

compderivexcept :: [NComp] -> [NDeriv] -> Substantive -> Substantive 
compderivexcept comp deriv f = excepts (compexcept comp f) [(Deriv,strings deriv)]

--- variants
sgdefvariants :: [NSgD] -> Substantive -> Substantive
sgdefvariants nom_s finite_f = 
    variants finite_f [(SF Sg Def Nom, strings nom_s), (SF Sg Def Gen, strings (map (mk_case Gen) nom_s))]

sgindefvariants :: [NSg] -> Substantive -> Substantive
sgindefvariants nom_s finite_f = 
    variants finite_f [(SF Sg Indef Nom, strings nom_s), (SF Sg Indef Gen, strings (map (mk_case Gen) nom_s))]

pldefvariants :: [NPlD] -> Substantive -> Substantive
pldefvariants nom_s finite_f = 
    variants finite_f [(SF Pl Def Nom, strings nom_s), (SF Pl Def Gen, strings (map (mk_case Gen) nom_s))]

plindefvariants :: [NPl] -> Substantive -> Substantive
plindefvariants nom_s finite_f = 
    variants finite_f [(SF Pl Indef Nom, strings nom_s), (SF Pl Indef Gen, strings (map (mk_case Gen) nom_s))]

-- excepts
sgdefexcepts :: [NSgD] -> Substantive -> Substantive
sgdefexcepts nom_s finite_f  = 
    excepts finite_f [(SF Sg Def Nom, strings nom_s), (SF Sg Def Gen, strings (map (mk_case Gen) nom_s))]

sgindefexcepts :: [NSg] -> Substantive -> Substantive
sgindefexcepts nom_s finite_f = 
    excepts finite_f [(SF Sg Indef Nom, strings nom_s), (SF Sg Indef Gen, strings (map (mk_case Gen) nom_s))]

pldefexcepts :: [NPlD] -> Substantive -> Substantive
pldefexcepts nom_s finite_f = 
    excepts finite_f [(SF Pl Def Nom, strings nom_s), (SF Pl Def Gen, strings (map (mk_case Gen) nom_s))]

plindefexcepts :: [NPl] -> Substantive -> Substantive
plindefexcepts nom_s finite_f = 
    excepts finite_f [(SF Pl Indef Nom, strings nom_s), (SF Pl Indef Gen, strings (map (mk_case Gen) nom_s))]

sgvariants :: [NSg] -> [NSgD] -> Substantive -> Substantive
sgvariants sg sget f = sgdefvariants sget (sgindefvariants sg f) 

plvariants :: [NPl] -> [NPlD] -> Substantive -> Substantive
plvariants pl plet f = pldefvariants plet (plindefvariants pl f) 

plexcepts :: [NPl] -> [NPlD] -> Substantive -> Substantive
plexcepts pl plet f = pldefexcepts plet (plindefexcepts pl f) 
    
no_plural :: Substantive -> Substantive
no_plural v = v `only` [SF Sg s c | s <- values, c <- values]

no_singular :: Substantive -> Substantive
no_singular v = v `only` [SF Pl s c | s <- values, c <- values]

onlyPlD :: Substantive -> Substantive
onlyPlD v = v `only` [SF Pl Def c | c <- values]

-- Declension 1
nn1 :: String -> Substantive
nn1 apa = mk_substantive apa (ifEndThen (=='a') apa (apa ++ "n") (apa ++ "en")) apor (apor ++ "na") [] []
 where ap   = dropEndIf (=='a') apa
       apor = ap ++ "or"

nn1flicka :: String -> Substantive
nn1flicka flicka = -- compvariants [tk 1 flicka ++ "e"]  $ 
                   nn1 flicka 
                  
nn1kyrka :: String -> Substantive
nn1kyrka kyrka = compvariant [tk 1 kyrka ++ "o"] (nn1 kyrka) 

nn1gata :: String -> Substantive
nn1gata gata = compvariant [tk 1 gata ++ "u"] (nn1 gata) 

nn1olja :: String -> Substantive
nn1olja olja = compexcept [tk 1 olja ++ "e"] (nn1 olja) 

nn1mamma :: String -> Substantive
nn1mamma mamma = compexcept [mamma] (nn1 mamma) 

nn1siffra :: String -> Substantive
nn1siffra siffra = compexcept [insert_second_last (tk 1 siffra) 'e'] 
                               (nn1 siffra) 

nn1dimma :: String -> Substantive
nn1dimma dimma   = compexcept [tk 2 dimma] (nn1 dimma) 

nn1sopor :: String -> Substantive
nn1sopor sopor = no_singular $ nn1 $ tk 2 sopor

nn1faggorna :: String -> Substantive
nn1faggorna faggorna = onlyPlD $ nn1 $ tk 4 faggorna

-- Declension 2

nn2 :: String -> Substantive
nn2 pojke = mk_substantive pojke pojken (pojk ++ "ar") (pojk ++ "arna") pojk pojk
 where pojk   = drop_final_e pojke
       pojken = pojke ++ if_vowel (last pojke) "n" "en" 
 
-- ungeminate 
nn2nyckel :: String -> Substantive
nn2nyckel nyckel = mk_substantive nyckel (nyckel ++"n") (nyckl ++"ar") (nyckl ++"arna") nyckel nyckl
 where nyckl = drop_last_vowel nyckel

nn2öken :: String -> Substantive
nn2öken öken = mk_subst öken (ökn ++"en") (ökn ++"ar") (ökn ++"arna") 
 where ökn = drop_last_vowel öken

nn2hummer :: String -> Substantive
nn2hummer hummer = mk_subst hummer (hummer ++"n") (humr ++"ar") (humr ++"arna")
 where humr = drop_second_last $ drop_last_vowel $ hummer

nn2kam :: String -> Substantive
nn2kam kam = mk_subst kam (kamm ++"en") (kamm ++"ar") (kamm ++"arna")
    where kamm = geminate kam

nn2pengar :: String -> Substantive
nn2pengar pengar = mk_subst [] [] (pengar) (pengar ++"na")

nn2mor :: String -> Substantive
nn2mor mor_or_moder = compvariants [moder++"s",mor,mor++"s"] [mor,mor++"s"] $ sgindefvariants [moder] $
 mk_substantive mor (moder ++"n") (moedr ++"ar") (moedr ++"arna") moder moder
    where (mor,moder)   = 
              if (dp 3 mor_or_moder) == "der" then (tk 3 mor_or_moder ++ "r",
                                                    mor_or_moder)
               else
                   (mor_or_moder, tk 1 mor_or_moder ++ "der")
          moedr = umlaut $ drop_last_vowel moder

nn2dotter :: String -> Substantive
nn2dotter dotter = mk_substantive dotter (dotter ++"n") (döttr ++"ar") (döttr ++"arna") dotter []
 where döttr = umlaut $ drop_last_vowel dotter

nn2fordran :: String -> Substantive
nn2fordran fordran = mk_substantive fordran fordran fordringar (fordringar ++"na") fordran []
 where fordringar = tk 2 fordran ++ "ingar"

nn2verkan :: String -> Substantive
nn2verkan verkan = mk_substantive verkan verkan verkningar (verkningar ++"na") verkan []
 where verkningar = tk 2 verkan ++ "ningar"

nn2toker :: String -> Substantive
nn2toker tok = sgindefvariants  [tok++"er"] (nn2 tok) 

nn2herre :: String -> Substantive
nn2herre herre = sgdefvariants [herre++"n"] (nn2 herre) 

nn2vers :: String -> Substantive
nn2vers vers =  combine (NounRulesSw.nn2 vers) (NounRulesSw.nn3 vers)

nn2manöver :: String -> Substantive
nn2manöver manöver = combine (nn2nyckel manöver) 
                             (no_singular (nn3 (drop_last_vowel manöver)))

nn2själ :: String -> Substantive
nn2själ själ = compvariant [själ++"a"] (nn2 själ) 

nn2brud :: String -> Substantive
nn2brud brud = compvariant [brud++"e"] (nn2 brud) 

nn2jord :: String -> Substantive
nn2jord jord = compvariant [jord++"e",jord++"a"] (nn2 jord) 

nn2hjälte :: String -> Substantive
nn2hjälte hjälte = compexcept  [hjälte] (nn2 hjälte)

nn2herde :: String -> Substantive
nn2herde herde = compexcept [herd++"e", herd++"a"] (nn2 herde) 
 where herd = tk 1 herde

nn2by :: String -> Substantive
nn2by by = compvariant [by++"a"] (nn2 by) 

nn2fågel :: String -> Substantive
nn2fågel fågel = compvariant [(drop_last_vowel fågel)++"a"] (nn2nyckel fågel) 

nn2lem :: String -> Substantive
nn2lem lem = compvariant [(geminate lem)++"a"] (nn2kam lem) 

nn2vägnar :: String -> Substantive
nn2vägnar vägnar = no_singular $ nn2 (tk 2 vägnar)

nn2stadgar :: String -> Substantive
nn2stadgar stadgar = compvariant [(tk 2 stadgar)++"e"] (nn2vägnar stadgar) 

-- Declension 3 
nn3 :: String -> Substantive
nn3 sak = mk_subst sak (sak ++ "en") (sak ++ "er") (sak++"erna") 

nn3vin :: String -> Substantive
nn3vin vin = mk_subst vin (vin ++ "et") (vin ++ "er") (vin ++ "erna") 

nn3gäst :: String -> Substantive
nn3gäst gäst = compvariant [gäst++"a"] (nn3 gäst) 

nn3bygd :: String -> Substantive
nn3bygd bygd = compvariant [bygd++"e"] (nn3 bygd) 

nn3hävd :: String -> Substantive
nn3hävd hävd = compvariant [hävd++"e",hävd++"a"] (nn3 hävd) 

nn3motor :: String -> Substantive
nn3motor motor =  mk_subst motor           (motor ++ "n") 
                           (motor ++ "er") (motor ++ "erna") 

nn3parti :: String -> Substantive
nn3parti parti = sgdefvariants [parti++"t"] (nn3vin parti) 

nn3poesi :: String -> Substantive
nn3poesi poesi = sgdefvariants [poesi++"en"] (nn3motor poesi) 

nn3musa :: String -> Substantive
nn3musa musa = mk_subst musa (musa ++ "n") (mus ++ "er") (mus ++ "erna")
 where mus = tk 1 musa

nn3museum :: String -> Substantive
nn3museum museum = mk_subst museum         (muse ++ "et") 
                            (muse ++ "er") (muse ++ "erna") 
 where muse = tk 2 museum

nn3gladiolus :: String -> Substantive
nn3gladiolus gladiolus = mk_subst gladiolus    (gladiolus ++ "en")  
                             (gladiol ++ "er") (gladiol ++ "erna") 
 where gladiol = tk 2 gladiolus

nn3fiber :: String -> Substantive
nn3fiber fiber = mk_substantive fiber (fiber++"n") (fibr ++ "er") (fibr ++ "erna") fiber fibr
 where fibr = drop_last_vowel fiber

nn3tand :: String -> Substantive
nn3tand tand = mk_subst tand (tand++"en") (tänd ++ "er") (tänd ++ "erna") 
 where tänd = umlaut tand

nn3land :: String -> Substantive
nn3land land = mk_subst land (land++"et") (länd ++ "er") (länd ++ "erna") 
 where länd = umlaut land

nn3paraply :: String -> Substantive
nn3paraply paraply = sgdefvariants [paraply++"t",paraply++"n"] (nn3vin paraply) 

nn3hobby :: String -> Substantive
nn3hobby hobby = plindefvariants  [(tk 1 hobby)++"ies"] (nn3 hobby ) 

nn3kastanj :: String -> Substantive
nn3kastanj kastanj = sgindefvariants  [kastanj++"e"] (nn3 kastanj)

nn3akademi :: String -> Substantive
nn3akademi akademi = sgdefvariants  [akademi] (nn3 akademi)

nn3paket :: String -> Substantive
nn3paket paket = combine (nn3 paket) (nn5 paket)

nn3element :: String -> Substantive
nn3element element = combine (nn3vin element) (nn5 element)

nn3kläder :: String -> Substantive
nn3kläder = no_singular . nn3 . tk 2

nn3kliche :: String -> Substantive
nn3kliche kliche = combine (nn3motor kliche) (no_singular (nn3 (tk 1 kliche ++ "e")))

nn3bok :: String -> Substantive
nn3bok bok = mk_subst bok (bok ++"en") (böcker) (böcker++"na") 
 where böcker = umlaut (tk 1 bok) ++ "cker"

nn3fot :: String -> Substantive
nn3fot fot = mk_subst fot (fot ++"en") (fötter) (fötter++"na") 
 where fötter = umlaut (geminate fot) ++ "er"

nn3bockfot :: String -> Substantive
nn3bockfot fot = compexcept' [fot,fot+?"s"] [fot,fot+?"s"] (mk_subst fot (fot ++"en") (fötter) (fötter++"na")) 
 where fötter = umlaut (geminate fot) ++ "er"

nn3vän :: String -> Substantive
nn3vän vän = mk_subst vän (vänn ++"en") (vänn ++"er") (vänn ++"erna") 
    where vänn = vän ++ [last vän]

nn3flanell :: String -> Substantive
nn3flanell flanell = sgdefvariants  [flanell++"et"] (nn3 flanell)

-- Declension 4
nn4 :: String -> Substantive
nn4 linje = mk_subst linje (linje ++ "n") (linje ++ "r") (linje++"rna") 

nn4studio :: String -> Substantive
nn4studio studio = plindefvariants [studio++"s"] (nn4 studio) 

nn4ampere :: String -> Substantive         
nn4ampere ampere = plvariants [ampere] [ampere++"n"] (nn4 ampere) 

nn4bonde :: String -> Substantive
nn4bonde bonde = compvariant [bond] $ mk_substantive bonde (bonde ++ "n") (bönd ++ "er") (bönd++"erna") bonde bond
   where bond = tk 1 bonde
         bönd = umlaut bond

-- Declension 5
nn5 :: String -> Substantive
nn5 rike = mk_subst rike (rike++et) (rike++en) (rike++ena) 
  where et  = if_vowel (last rike) "t" "et"
        en  = if_vowel (last rike) "n" "en"
        ena = if_vowel (last rike) "na" "ena"

nn5knä :: String -> Substantive
nn5knä knä = sgdefvariants [knä++"et"] (nn5 knä) 

nn5äpple :: String -> Substantive
nn5äpple äpple = compexcept [insert_second_last (tk 1 äpple) 'e'] (nn5 äpple) 

nn5samhälle :: String -> Substantive
nn5samhälle samhälle = compvariant [tk 1 samhälle] (nn5 samhälle) 

nn5arbete :: String -> Substantive
nn5arbete arbete =  compderivexcept [arbet] [] (nn5 arbete) 
 where arbet = tk 1 arbete

nn5bi :: String -> Substantive
nn5bi bi = pldefvariants [bi++"en"] 
           (except (nn5 bi) [(SF Sg Def c, mk_case c (bi++"et")) | c <- values])  

nn5frö :: String -> Substantive
nn5frö frö = pldefvariants [frö++"en"] $ sgdefvariants [frö++"et"] (nn5 frö) 

nn5party :: String -> Substantive 
nn5party party = plindefvariants [tk 1 party ++ "ies"] (nn5 party) 

nn5abc :: String -> Substantive
nn5abc abc nf = strings $ case nf of
    (SF Sg  Indef Nom) -> [abc]
    (SF Sg Indef Gen) -> [abc ++ ":s",abc++"-s"]
    (SF Sg Def   Nom) -> [abc ++ "-et", abc++"-t",abc ++ ":et", abc++":t"]
    (SF Sg Def   Gen) -> [abc ++ "-ets", abc++"-ts",abc ++ ":ets", abc++":ts"]
    (SF Pl Indef Nom) -> [abc ++ ":n",abc++"-n"]
    (SF Pl Indef Gen) -> [abc ++ ":ns",abc++"-ns"]
    (SF Pl Def   Nom) -> [abc++"-na",abc++":na"]
    (SF Pl Def   Gen) -> [abc++"-nas",abc++":nas"]
    InitComposite         -> [abc++"-"]
    MedComposite         -> [abc++"-"]
    SMS               -> [abc++"-"]
    Deriv             -> [abc]

nnkol14 :: String -> Substantive 
nnkol14 kol14 n = strings $ case n of
                             (SF Sg Indef Nom) -> [kol14]
                             (SF Sg Indef Gen) -> [kol14++"-s",kol14++":s"]
                             InitComposite         -> [kol14++"-"]
                             MedComposite         -> [kol14++"-"]
                             _                 -> []

nn5anmodan :: String -> Substantive
nn5anmodan anmodan = mk_subst anmodan anmodan anmodanden (anmodanden++"a")
 where anmodanden = anmodan++"den"
 
-- Declension 6
nn6 :: String -> Substantive
nn6 lik = mk_subst lik (lik ++ "et") lik (lik ++ "en")

nn6barn :: String -> Substantive
nn6barn barn = compvariant [barn++"a"] (nn6 barn) 

nn6arv :: String -> Substantive
nn6arv arv = compvariant  [arv++"e"] (nn6 arv)

nn6mil :: String -> Substantive
nn6mil mil = mk_subst mil (mil ++ "en") mil (mil ++ "en")

nn6broder :: String -> Substantive
nn6broder broder = compvariants [broder++"s"] [] $ mk_subst broder (broder ++ "n") (bröd++"er") (bröd ++ "erna")
 where bröd = umlaut brod
       brod = tk 2 broder

nn6akademiker :: String -> Substantive
nn6akademiker akademiker = 
 mk_subst akademiker (akademiker ++ "n") akademiker (akademiker ++ "na") 

nn6lager :: String -> Substantive
nn6lager lager = pldefvariants  [lager++"na"] 
                  (mk_substantive lager (lagr ++"et") (lager) (lagr ++"en") lager lagr)
  where lagr = ungeminate (tk 2 lager) ++ [last lager]

nn6nummer :: String -> Substantive
nn6nummer nummer = pldefvariants [nummer++"na"] 
                    (mk_substantive nummer (numr ++"et") (nummer) (numr ++"en") nummer numr)
  where numr = drop_second_last $ drop_last_vowel $ nummer

nn6garage :: String -> Substantive
nn6garage garage = mk_subst garage (garage ++ "t") garage (garage ++ "n") 

nn6manus :: String -> Substantive
nn6manus manus = sgdefvariants [manus]  (nn6 manus) 

nn6gås :: String -> Substantive            
nn6gås gås = mk_subst gås (gås++"en") gäss (gäss++"en") 
 where gäss = (vc "ä" gås) ++"s"

nn6ordförande :: String -> Substantive
nn6ordförande ordförande = plindefvariants  [ordförande++"n"] (nn6akademiker ordförande)

nn6far :: String -> Substantive
nn6far far_or_fader = nn2mor far `except` [(SF Pl d c, mkPl d c) | d <- values, c <- values]
  where far = if (dp 3 far_or_fader == "der") then tk 3 far_or_fader ++"r" else
                  far_or_fader
        mkPl d c = mk_case c $
              case d of
                Indef -> (umlaut (tk 1 far)) ++ "der"
                _     -> (umlaut (tk 1 far)) ++ "derna"

nn6papper :: String -> Substantive
nn6papper papper = mk_substantive papper (pappr ++ "et") papper (pappr ++ "en") papper pappr
 where pappr = drop_last_vowel papper 

nn6kikare :: String -> Substantive
nn6kikare kikare = mk_substantive kikare (kikare ++ "n") kikare (kikar ++ "na") kikar kikar
 where kikar = tk 1 kikare

nn6program :: String -> Substantive
nn6program program = mk_subst program (programm ++"et") program (programm ++"en") 
    where programm = program ++ [last program]

nn6mus :: String -> Substantive
nn6mus mus = mk_subst mus (mus ++"en") möss (möss ++"en") 
    where möss = vc "ö" $ geminate mus

nn6vaktman :: String -> Substantive
nn6vaktman man = mk_subst man (mann ++"en") män (männ ++"en") 
    where män   = umlaut man
          männ  = geminate män
          mann  = geminate man

nn6borst :: String -> Substantive
nn6borst borst = sgdefvariants  [borst++"en"] (nn6 borst)

nn6klientel :: String -> Substantive
nn6klientel klientel = combine (nn6 klientel) (nn6mil klientel)

--nn6frx :: String -> Substantive
--nn6frx f = mk_substantive_v [f] [f ++"-et",f ++":et",f++"-t",f++":t"] [f++"-en",f++":en",f++"-n",f++":n"] 
--                                [f++"-ena",f++":ena",f++"-na",f++":na"] [f++"-"] [f++"-"] []

nn6ordalag :: String -> Substantive
nn6ordalag ordalag = no_singular (nn6 ordalag)

-- Declension 7

nn7musical :: String -> Substantive        
nn7musical musical = pldefvariants [musical +? "sarna"] (mk_subst musical (musical+?"en") (musical +? "s") (musical +? "sen"))

-- nn7jeans :: String -> Substantive          
-- nn7jeans jeans = nn7musical

-- no plural
nn0 :: String -> Substantive
nn0 mjölk = mk_subst mjölk (mjölk++"en") [] [] 

nn0oväsen :: String -> Substantive
nn0oväsen oväsen = sgdefexcepts  [oväsen++"det",oväsen++"et"] (nn0 oväsen)

nn0tröst :: String -> Substantive          
nn0tröst tröst = compvariant  [tröst++"e"] (nn0 tröst)

nn0adel :: String -> Substantive           
nn0adel adel = sgdefexcepts  [adel++"n"] (nn0 adel)

nn0skum :: String -> Substantive
nn0skum skum = sgdefexcepts  [skumm++"et"] (nn0 skum)
 where skumm = geminate skum

nn0skam :: String -> Substantive
nn0skam skam = sgdefexcepts [skamm++"en"] (nn0 skam) 
 where skamm = geminate skam

nn0manna :: String -> Substantive
nn0manna manna = sgdefexcepts [manna++"n", manna++"t"] (nn0 manna) 

nn0början :: String -> Substantive         
nn0början början = mk_substantive början början [] [] [] []

nn0uran :: String -> Substantive           
nn0uran uran = sgdefvariants  [uran++"et"] (nn0 uran)

-- nn0fåfänga  :: String -> Substantive       
-- nn0fåfänga  = nn0adel  

nn0biologi :: String -> Substantive        
nn0biologi biologi = sgdefvariants  [biologi++"en"]  (nn0adel biologi)

nn0brådska :: String -> Substantive        
nn0brådska brådska = compexcept  [brådske] (nn0adel brådska)
 where brådske = (tk 1 brådska) ++ "e"

nn0smör :: String -> Substantive           
nn0smör smör = sgdefexcepts [smör++"et"] (nn0 smör) 

nn0kaffe :: String -> Substantive          
nn0kaffe kaffe = sgdefexcepts  [kaffe++"t"] (nn0 kaffe)

nn0socker :: String -> Substantive        
nn0socker socker = sgdefexcepts  [drop_last_vowel socker ++ "et"] (nn0 socker)

nn0januari :: String -> Substantive        
nn0januari januari = mk_substantive januari januari [] [] januari []

nn0aktinium :: String -> Substantive      
nn0aktinium aktinium = sgdefexcepts [aktinium, aktinium++"et"] (nn0 aktinium) 

-- pending declension
nnvkansli :: String -> Substantive
nnvkansli kansli = pldefvariants [kansli++"en"] (combine (nn3parti kansli) (nn6akademiker kansli)) 

nnvfaktum :: String -> Substantive
nnvfaktum faktum = plvariants [fakt++"a"] [fakt++"a"] $
                    sgdefvariants [faktum] (nn6mil faktum) 
 where fakt = tk 2 faktum

nnvdistikon :: String -> Substantive
nnvdistikon distikon = plvariants  [distik++"a"] [distik++"a"] (nn6 distikon)
 where distik = tk 2 distikon

nnvabdomen :: String -> Substantive
nnvabdomen abdomen = plvariants  [abdom++"ina"] [abdom++"ina"] (mk_substantive abdomen abdomen abdomen abdomen abdomen abdomen)
 where abdom = tk 2 abdomen

nnvnomen :: String -> Substantive
nnvnomen nomen = plvariants [nom++"ina"] [nom++"ina"] (mk_substantive nomen (nomen++"et") nomen (nomen++"en") nomen nomen) 
 where nom = tk 2 nomen

nnvunderstatement :: String -> Substantive
nnvunderstatement understatement = 
 plvariants [understatement++"s"] [understatement++"sen"] (nn6 understatement) 
 
nnvfranc :: String -> Substantive          
nnvfranc franc = pldefvariants  [franc++"en"] (nnvunderstatement franc)

nnvcocktail :: String -> Substantive       
nnvcocktail cocktail = plvariants [cocktail++"ar"] [cocktail++"arna"]
                                  (nn7musical cocktail) 
             
nnvgangster :: String -> Substantive       
nnvgangster gangster = combine s_f ar_f
 where s_f     = mk_subst gangster (gangster++"n") (gangster++"s") (gangster++"sen")
       ar_f    = mk_subst gangster (gangster++"n") (gangstr++"ar") (gangstr++"arna")
       gangstr = drop_last_vowel gangster

nnvpartner :: String -> Substantive        
nnvpartner partner = plvariants  [partner] [partner ++"na",partner ++"sarna"]  $
    mk_subst partner (partner++"n") (partner++"s") (partner++"sen")
  -- $ (nn7musical partner)

nnvcentrum :: String -> Substantive        
nnvcentrum centrum = plvariants [centr++"a", centr++"er"]  
                                [centr++"ana", centr++"erna"] $
                                 sgdefvariants [centr++"et",centrum] um_f 
  where um_f  =  mk_subst centrum (centrum++"et") centrum (centrum++"en") 
        centr = tk 2 centrum

nnvtempo :: String -> Substantive          
nnvtempo tempo = mk_subst tempo tempo tempo (tempo++"na")

nnvantecedentia  :: String -> Substantive  
nnvantecedentia antecedentia = plvariants [antecedenti++"er"] [antecedenti++"erna"] (mk_subst [] [] antecedentia antecedentia)
                                         
  where antecedenti = tk 1 antecedentia

nnvtrall :: String -> Substantive
nnvtrall trall = combine (nn2 trall) (nn5 trall)

nnvbehå :: String -> Substantive
nnvbehå behå = combine (nn2 behå) (nn6akademiker behå)

nnvjojo :: String -> Substantive
nnvjojo jojo = combine (nn2 jojo) (nn4 jojo)

nnvsandwich :: String -> Substantive       
nnvsandwich sandwich = plvariants [sandwich++"es"] [sandwich++"esen"]
                                  (nn3 sandwich) 

nnvabc :: String -> Substantive
nnvabc = nn5abc

nnvgarn :: String -> Substantive
nnvgarn garn = combine (nn3vin garn) (nn6 garn)

nnvhuvud :: String -> Substantive
nnvhuvud huvud =  plindefvariants [huvud++"en"]
                   (mk_subst huvud (huvud++"et") huvud (huvud++"ena"))
                  
nnvkvantum :: String -> Substantive
nnvkvantum =  nnvcentrum

nnvspektrum :: String -> Substantive
nnvspektrum spektrum = plvariants [spektrum,spektr++"a"] 
                                  [spektrum++"en", spektr++"ana"] $
    sgdefvariants [spektr++"et", spektrum++"et"] $
       mk_subst spektrum spektrum (spektr++"er") (spektr++"ena") 
 where spektr = tk 2 spektrum

nnvblinker :: String -> Substantive
nnvblinker blinker = plvariants [blinker++"s"] [blinker++"sen",blinker++"sarna"] $
                     mk_subst blinker (blinker++"n") 
                             (blinkr++"ar") (blinkr++"arna") 
 where blinkr = mmr (drop_last_vowel blinker)

nnvdress :: String -> Substantive
nnvdress dress = plvariants [dress++"er"] [dress++"erna"] $ 
                 mk_subst dress (dress++"en") (dress++"ar") (dress++"arna")

nnvhambo :: String -> Substantive
nnvhambo hambo = plvariants [hambo++"r"] [hambo++"rna"] $
          mk_subst hambo (hambo++"n") (hambo++"er") (hambo++"erna")

nnvkaliber :: String -> Substantive
nnvkaliber kaliber = plvariants [kalibr++"er"] [kalibr++"erna"] $ 
                       mk_subst kaliber        (kaliber++"n") 
                               (kalibr++"ar") (kalibr++"arna")
 where kalibr = drop_last_vowel kaliber

nnvklammer :: String -> Substantive
nnvklammer klammer = plvariants [klamr ++ "ar"] [klamr++"arna"] $
               mk_subst klammer (klammer++"n") klammer (klammer++"na")
 where klamr = ungeminate (tk 2 klammer) ++ dp 1 klammer

nnvplayboy :: String -> Substantive
nnvplayboy playboy = plvariants [playboy++"s"] [playboy++"sen",playboy++"sarna"] $
    mk_subst playboy (playboy++"en") (playboy++"ar") (playboy++"arna")
                                    
nnvroller :: String -> Substantive
nnvroller roller =  plvariants [rollr++"ar"] [rollr++"arna"] $
    mk_subst roller (roller++"n") (roller) (roller++"na")
 where rollr = drop_last_vowel roller

nnvtrio :: String -> Substantive
nnvtrio trio = plvariants [trio++"s"] [trio++"sen"] $
    mk_subst trio (trio++"n") (trio++"r") (trio++"rna")

nnvborr :: String -> Substantive
nnvborr borr = combine (nn2 borr) (nn6 borr)

nnvtest :: String -> Substantive
nnvtest test = combine (nn3 test) (nn6 test)

-- irregular nouns
nnonarkotikum :: String -> Substantive
nnonarkotikum narkotikum = sgvariants [narkotik++"a"] [narkotik++"an"] $
                           (mk_subst narkotikum (narkotikum++"et") 
                                     (narkotik++"a") (narkotik++"a")) 
 where narkotik = tk 2 narkotikum

nnoexamen :: String -> Substantive
nnoexamen examen =  mk_subst examen examen (exam++"ina") (exam++"ina")
  where exam = tk 2 examen

nnoemeritus :: String -> Substantive       
nnoemeritus emeritus = mk_subst emeritus emeritus emeriti  (emeriti++"na") 
 where emeriti = tk 2 emeritus ++ "i"

nnofullmäktig :: String -> Substantive     
nnofullmäktig fullmäktig = mk_subst fullmäktig (fullmäktig++"en") (fullmäktig++"e") (fullmäktig++"e")

-- nnoprestanda :: String -> Substantive      
-- nnoprestanda prestanda = mk_substantive prestanda prestanda [] [] prestanda prestanda

nnoöga  :: String -> Substantive
nnoöga öga = mk_subst öga (öga++"t") ögon (ögon++"en")
 where ögon = tk 1 öga ++ "on"

nnodata :: String  -> Substantive
nnodata data_ = mk_subst [] [] (data_) (data_++"n")

nnoofficer :: String -> Substantive
nnoofficer officer = sgdefvariants [officer++"n"] (mk_subst officer (officer++"en") (officer++"are") (officer++"arna")) 

-- definite noun
nndkneken  :: String -> Substantive        
nndkneken  kneken = mk_subst [] kneken [] [] 

nndbrådrasket :: String -> Substantive     
nndbrådrasket brådrasket = mk_subst [] brådrasket [] [] 

-- no inflection

nni :: String -> Substantive
nni w = missing (nngfebruari w) [SF Sg Indef Gen]

nni_ns :: String -> Substantive
nni_ns w = compexcept' [w] [w] (missing (nngfebruari w) [SF Sg Indef Gen])

-- genitive inflection

nngfebruari :: String -> Substantive
nngfebruari februari = mk_subst februari [] [] []

-- multi-words 
nnm0 f xs (SFM t) = mapStr (rs++)  (f lw t)
 where lw = last xs
       rs = unwords (init xs) ++ " "

-- definite

nnmdu ::[String] -> (SubstM -> Str)
nnmdu = nnm0 (\w -> missing (nndkneken w) [InitComposite,MedComposite])

nnmdn ::[String] -> (SubstM -> Str)
nnmdn = nnm0 (\w -> missing (nndbrådrasket w) [InitComposite, MedComposite])

nnmg :: [String] -> (SubstM -> Str)
nnmg = nnm0 (\w -> missing (nngfebruari w) [InitComposite, MedComposite])

nnmi :: [String] -> (SubstM -> Str)
nnmi = nnm0 (\w -> missing (nni w) [InitComposite, MedComposite])

