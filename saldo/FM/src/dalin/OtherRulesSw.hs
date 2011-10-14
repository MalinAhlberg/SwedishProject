----------------------------------------------------------------------
-- |
-- Module      : OtherRulesSw
-- Maintainer  : Markus Forsberg
-- Stability   : (stability)
-- Portability : (portability)
--
--
-------------------------------------------------------------------------
module OtherRulesSw where

import GenRulesSw
import General  
import TypesSw  
import AdjRulesSw(av1blek)
import NounRulesSw(mk_case,mk_case_v)

-- adverbs

no_comp :: Adverb -> Adverb
no_comp f = missing f [AdComp, AdSMS]

mk_adverb :: [String] -> [String] -> [String] -> Adverb
mk_adverb bra battre bast af = strings $
   case af of
    (AdverbForm Posit)  -> bra
    (AdverbForm Compar) -> battre
    (AdverbForm Superl) -> bast
    AdComp              -> concat [[x,x +? "-"] | x <- bra]
    AdSMS               -> [x +? "-" | x <- bra]

abfrämst :: String -> Adverb
abfrämst främst = no_comp $ mk_adverb [] [] [främst]

abfint :: String -> Adverb
abfint = adjective_to_adverb . av1blek . tk 1

abfort fort = mk_adverb [fort] [fort +? "are"] [fort +? "ast"]


adjective_to_adverb :: Adjective -> Adverb
adjective_to_adverb adj =  
    giveValues  [adj (AF p Nom) | p <- [Pos (Strong (ASgNeutr)), 
                                        Comp, Super SupStrong]]

ab_i_no_comp :: String -> AdverbInv
ab_i_no_comp s AdverbInvForm = mkStr s
ab_i_no_comp s _             = nonExist

abm :: String -> AdverbMInv
abm till_exempel _ = mkStr $ till_exempel

-- pronouns

mk_pron_jag :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> PronPN
mk_pron_jag (jag,mig,min,mitt,mina) s c = strings $ case c of
  PNom            -> suff jag
  PAcc            -> suff mig
  PGen (ASgUtr)   -> suff min
  PGen (ASgNeutr) -> suff mitt
  PGen APl        -> suff mina
-- map (++ "a") min 
 where suff = apply_suffixes s

-- pron_refl = mk_pron_jag [] "sig" "sin" "sitt" `missing` [PNom]

mk_pron_han :: (Suffixes, Suffixes, Suffixes) -> String -> PronPN
mk_pron_han (han, honom, hans) s c = strings $ case c of
  PNom   -> suff han
  PAcc   -> suff honom
  PGen _ -> suff hans
 where suff = apply_suffixes s

mk_pron_nagon :: (Suffixes,Suffixes,Suffixes) -> String -> PronAdj
mk_pron_nagon (nagon,nagot,nagra) s (AP gn c) = strings $ map (mk_case c) $
    case gn of
     ASgUtr   -> suff nagon
     ASgNeutr -> suff nagot
     APl      -> suff nagra
 where suff = apply_suffixes s

mk_pn_o_den :: String -> PronAdj
mk_pn_o_den den (AP gn c) = strings $ 
    case (gn,c) of
     (ASgUtr,Nom)   -> [den]
     (ASgUtr,Gen)   -> [den++"s", de++"ss"] 
     (ASgNeutr,Nom) -> [de++"t"]
     (ASgNeutr,Gen) -> [de++"ss"]
     _               -> []
 where de = tk 1 den

mk_pron_dylik :: String -> PronAdj
mk_pron_dylik = mk_pron_nagon ([e ""], [e "t"], [e "a"])

-- proper names

mk_pm :: String -> (PNForm -> Str)
mk_pm s (PNForm c) = strings $ mk_case_v c [s]

mk_pm_alice :: String -> (PNForm -> Str)
mk_pm_alice s (PNForm Nom) = strings $ [s]
mk_pm_alice s (PNForm Gen) = strings $ [s,s +? "s"]

mk_pma :: String -> (PNAForm -> Str)
mk_pma s (PNAForm c) = strings $ mk_ccase c s

mk_ccase c s = case c of 
                Nom -> [s]
                Gen | elem (last s) "sxzSXZ" -> [s]
                _   -> [s ++ "-s",s ++ ":s",s++"s"]

mk_pmm0 :: [String] -> (PNMForm -> Str)
mk_pmm0 xs (PNMForm c) = strings [unwords ((init xs) ++ [r]) | r <- mk_case_v c [last xs]]

mk_pmam0 :: [String] -> (PNMForm -> Str)
mk_pmam0 xs (PNMForm c) =  strings [unwords ((init xs) ++ [g]) | g <- (mk_ccase c (last xs))]

mk_number :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> Number
mk_number (en,ett,första,förste,en_c) s (NumC) =
 let xs = apply_suffixes s en_c in 
     strings $ xs ++ [x ++ "-" | x <- xs]
mk_number (en,ett,första,förste,en_c) s (NumF c o) = 
  strings $ map (mk_case c) $ case o of
   Numeral         -> apply_suffixes s en
   NumeralN        -> apply_suffixes s ett
   Ordinal NoMasc  -> apply_suffixes s första
   Ordinal Sex_Masc -> apply_suffixes s förste

nln1 :: String -> Number
nln1 x1 (NumC) = mkStr [] 
nln1 x1 (NumF c o) = 
  mkStr $ case (o,c) of
   (Ordinal NoMasc,Nom)  -> x1 ++ ":a" 
   (Ordinal NoMasc,Gen)  -> x1 ++ ":as"
   (Ordinal Sex_Masc,Nom)    -> x1 ++ ":es" 
   (Ordinal Sex_Masc,Gen)    -> x1 ++ ":es" 
   (_,Nom)               -> x1
   (_,Gen)               -> x1 ++ ":s"

mk_number_ng :: (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> String -> Number
mk_number_ng suffs s = missing (mk_number suffs s) [NumF Gen o | o <- values]

{-
mk_number :: String -> String -> Number
mk_number tre tredje (NumF c o) = 
  mkStr $ mk_case c $ case o of
   Ordinal -> tredje
   Numeral -> tre

mk_numeral :: String -> Number
mk_numeral tre = mk_number tre []

inv_numeral :: String -> Number
inv_numeral s = missing (mk_numeral s) [NumF Gen Numeral]

inv_ordinal :: String -> Number
inv_ordinal s = missing (mk_ordinal s) [NumF Gen Ordinal]
  
mk_ordinal :: String -> Number
mk_ordinal tredje = mk_number [] tredje

nrtretton :: String -> Number
nrtretton tretton = mk_number tretton (tretton++"de")

nrtrettio :: String -> Number
nrtrettio trettio = mk_number trettio (trettio++"nde")

nrandra :: String -> Number
nrandra andra = variant (mk_number [] andra)
                 [(NumF Nom Ordinal, andre),  
                 (NumF Gen Ordinal, andre++"s")]
    where andre = tk 1 andra ++ "e"

nrfemte :: String -> Number
nrfemte femte = mk_number [] femte

nriii :: String -> Number
nriii iii (NumF Nom Numeral) = mkStr iii
nriii _   _                  = nonExist
-}

-- auxiliary verbs

aux_verb_gen :: Str -> Str -> Str -> Str -> AuxVerb
aux_verb_gen vilja vill ville velat = 
    giveValues [vilja, vill, ville, velat]

aux_verb :: String -> String -> String -> String -> AuxVerb
aux_verb vilja vill ville velat = 
    giveValues $ map mkStr [vilja, vill, ville, velat]


