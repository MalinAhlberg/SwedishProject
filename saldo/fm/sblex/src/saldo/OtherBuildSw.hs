module OtherBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

-- Proper names

pm_m :: String -> String -> Entry
pm_m tax s = entryI (mk_pm s) [prValue MascGen,tax] 

pm_f :: String -> String -> Entry
pm_f tax s = entryI (mk_pm s) [prValue FemGen,tax] 

pm_f_alice :: String -> String -> Entry
pm_f_alice tax s = entryI (mk_pm_alice s) [prValue FemGen,tax] 

pm_h :: String -> String -> Entry
pm_h tax s = entryI (mk_pm s) [prValue Human,tax] 

pm_w :: String -> String -> Entry
pm_w tax s = entryI (mk_pm s) [prValue PNeutr,tax] 

pm_p :: String -> String -> Entry
pm_p tax s = entryI (mk_pm s) [prValue GPl,tax] 

pm_v :: String -> String -> Entry
pm_v tax s = entryI (mk_pm s) [prValue Pend,tax] 

pm_u :: String -> String -> Entry
pm_u tax s = entryI (mk_pm s) [prValue Utr,tax]

pm_n :: String -> String -> Entry
pm_n tax s = entryI (mk_pm s) [prValue Neutr,tax]

pma_n :: String -> String -> Entry
pma_n tax s = entryI (mk_pma s) [prValue Neutr,tax]

pma_u :: String -> String -> Entry
pma_u tax s = entryI (mk_pma s) [prValue Utr,tax]

pma_h :: String -> String -> Entry
pma_h tax s = entryI (mk_pma s) [prValue Human,tax]

pma_w :: String -> String -> Entry
pma_w tax s = entryI (mk_pma s) [prValue PNeutr,tax] 

pma_m :: String -> String -> Entry
pma_m tax s = entryI (mk_pma s) [prValue PNeutr,tax] 

-- multi-word proper names

pmm_n :: String -> String -> Entry
pmm_n tax s = entryI (mk_pmm0 (words s)) [prValue Neutr,tax]

pmm_u :: String -> String -> Entry
pmm_u tax s = entryI (mk_pmm0 (words s)) [prValue Utr,tax]

pmm_h :: String -> String -> Entry
pmm_h tax s = entryI (mk_pmm0 (words s)) [prValue Human,tax]

pmm_m :: String -> String -> Entry
pmm_m tax s = entryI (mk_pmm0 (words s)) [prValue MascGen,tax] 

pmm_f :: String -> String -> Entry
pmm_f tax s = entryI (mk_pmm0 (words s)) [prValue FemGen,tax] 

pmm_v :: String -> String -> Entry
pmm_v tax s = entryI (mk_pmm0 (words s)) [prValue Pend,tax] 

pmm_p :: String -> String -> Entry
pmm_p tax s = entryI (mk_pmm0 (words s)) [prValue GPl,tax] 


pmam_m :: String -> String -> Entry
pmam_m tax s = entryI (mk_pmam0 (words s)) [prValue MascGen,tax] 

pmam_n :: String -> String -> Entry
pmam_n tax s = entryI (mk_pmam0 (words s)) [prValue Neutr,tax] 

-- adverbs

ab_bort :: String -> Entry
ab_bort = entry . f
 where f s AdCompI = strings [s,s+?"-"]
       f s AdSMSI  = strings [s+?"-"]
       f s _       = strings [s]

ab_i_aldrig :: String -> Entry
ab_i_aldrig = entry . ab_i_no_comp

ab_främst :: String -> Entry
ab_främst = entry . abfrämst

ab_bra :: [String] -> [String] -> [String] -> Entry
ab_bra x y z = entry $ mk_adverb x y z

ab_comp :: [String] -> [String] -> [String] -> [String] -> Entry
ab_comp x y z w = entry $ mk_adverb_c x y z w

ab_fint :: String -> Entry
ab_fint = entry  . abfint

ab_1_fort = entry . abfort

abm_i_till_exempel :: String -> Entry
abm_i_till_exempel till_exempel = entry $ abm till_exempel

-- compounds 

compound :: String -> Entry
compound = entry . f
 where f s CMP  = strings [s, s+?"-"]
       f s CSMS = strings [s+?"-"]

-- pronouns

pn_jag :: (Person,Numerus) -> (Suffixes, Suffixes,Suffixes,Suffixes,Suffixes) -> String -> Entry
pn_jag (p,n) (a,b,c,d,e) s = entryI (mk_pron_jag (a,b,c,d,e) s) [prValue p, prValue n]

pn_nagon :: (Suffixes, Suffixes, Suffixes) -> String -> Entry
pn_nagon (a,b,c) s = entry $ mk_pron_nagon (a,b,c) s

pn_han :: (Person,Numerus) -> (Suffixes, Suffixes, Suffixes) -> String -> Entry
pn_han (p,n) (a,b,c) s = entryI (mk_pron_han (a,b,c) s) [prValue p, prValue n]

pn_vem :: (Suffixes, Suffixes, Suffixes) -> String -> Entry
pn_vem (a,b,c) s = entry (mk_pron_han (a,b,c) s)

pn_o_den :: String -> Entry
pn_o_den = entry . mk_pn_o_den

pn_dylik :: String -> Entry
pn_dylik = entry . mk_pron_dylik

pn_interrog_inv :: String -> Entry
pn_interrog_inv = entry . (const :: Str -> InterrogInvForm -> Str) . mkStr

pn_inv :: String -> Entry
pn_inv = entry . (const :: Str -> PronInvForm -> Str) . mkStr

pnm_inv :: String -> Entry
pnm_inv = entry . (const :: Str -> PronMInvForm -> Str) . mkStr

pnm_gen :: String -> Entry
pnm_gen = entry . f
 where f s (PronMCForm Nom) = mkStr $ s
       f s (PronMCForm Gen) = mkStr $ case words s of
                                        (x:xs) -> unwords (x +? "s":xs)

pnm_o_den_här :: String -> Entry
pnm_o_den_här =  set_pos "pnm" . entry . pnm_o_den_här_rule

pnm_gen2 :: String -> Entry
pnm_gen2 = entry . f
 where f s (PronMCForm Nom) = mkStr $ s
       f s (PronMCForm Gen) = mkStr $ case reverse (words s) of
                                        (x:xs) -> unwords (reverse (x +? "s":xs))

-- numbers

number    ss = entry . mk_number ss

number_ng ss = entry . mk_number_ng ss

nl_n_1 = entry . nln1 

-- invariant closed classes

comma :: Param a => (a -> Str) -> (a -> Str)
comma f = \x -> strings $ concat $ map expand_comma (unStr (f x))
 where expand_comma      [] = [[]]
       expand_comma (',':s) = map (',':) (expand_comma s) ++ expand_comma s
       expand_comma   (x:s) = map (x:) (expand_comma s)

interj :: String -> Entry
interj = entry . comma . (const :: Str -> InterjForm -> Str) . mkStr

interjm :: String -> Entry
interjm aja_baja = set_pos "inm" $ interj (aja_baja)

conj :: String -> Entry
conj = entry . (const :: Str -> ConjForm -> Str) . mkStr

subj :: String -> Entry
subj = entry . (const :: Str -> SubForm -> Str) . mkStr

prep :: String -> Entry
prep = entry . (const :: Str -> PartForm -> Str) . mkStr

part :: String -> Entry
part = entry . (const :: Str -> PartForm -> Str) . mkStr

inf_mark :: String -> Entry
inf_mark = entry . (const :: Str -> InfMarkForm -> Str) . mkStr

invar :: String -> String -> Entry
invar pos = set_pos pos . entry . f
 where f s AdCompI = strings [s+?"-"]
       f s AdSMSI  = strings [s+?"-"]
       f s _       = strings [s]

-- abbreviations
aba_i_dvs :: String -> Entry
aba_i_dvs = entry . (const :: Str -> ABAForm -> Str) . mkStr

al_o_en :: String -> Entry
al_o_en den = 
 entry $ \(ArticleForm g s) -> strings $ 
                              case (g,s) of
                                (ASgUtr,Indef)   -> ["en"]
                                (ASgNeutr,Indef) -> ["ett"]
                                (APl,Indef)       -> ["ena"]
                                (ASgUtr,Def)   -> ["den"]
                                (ASgNeutr,Def) -> ["det"]
                                (APl,Def)      -> ["de","dom"]

