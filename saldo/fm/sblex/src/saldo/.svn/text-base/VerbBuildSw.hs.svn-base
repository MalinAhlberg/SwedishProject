module VerbBuildSw where

import General
import Dictionary
import TypesSw
import RulesSw
import GenRulesSw

verb :: Verb -> Entry
verb = entry 

verbM :: VerbM -> Entry
verbM = entry

verb_full :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ verb_prefixed n finna finner finne finn fann funne funnit funnen w 

verb_full_compound :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_compound n (finna, finne,finner, finn, fann, funne, funnit, funnen,finn_c) w = 
 verb $ verb_prefixed_compound n finna finner finne finn fann funne funnit funnen finn_c w 

verb_full_compound_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_compound_sform_variant n (finna, finne,finner, finn, fann, funne, funnit, funnen,finn_c) w = 
 verb $ ppvar n (verb_prefixed_compound n finna finner finne finn fann funne funnit funnen finn_c w) w 

verb_full_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_sform_variant n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ ppvar n (verb_prefixed n finna finner finne finn fann funne funnit funnen w) w

verb_full_no_sform :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_full_no_sform n (finna, finne,finner, finn, fann, funne, funnit, funnen) w = 
 verb $ no_passive $ verb_prefixed n finna finner finne finn fann funne funnit funnen w 

verb_weak :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_weak_no_passive :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_no_passive n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ no_vcomp $ no_part_pres $ no_passive $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_weak_compound :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_compound n (finna, finner, finn, fann, funnit, funnen,finn_c) w = 
 verb $ verb_prefixed_compound n finna finner [] finn fann [] funnit funnen finn_c w 

verb_weak_compound_sform_variant :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_compound_sform_variant n (finna, finner, finn, fann, funnit, funnen,finn_c) w = 
 verb $ ppvar n (verb_prefixed_compound n finna finner [] finn fann [] funnit funnen finn_c w) w 


vbm_inf w = set_pos "vbm" $ verb $ only (vb1 w) [VI (Inf Act)]

verb_vbm n wfs w = (set_pos "vbm" . verb_weak_no_passive n wfs) w

verb_weak_no_sform :: Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_no_sform n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ no_passive $ verb_prefixed n finna finner [] finn fann [] funnit funnen w 

verb_weak_sform_variant ::  Int -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
verb_weak_sform_variant n (finna, finner, finn, fann, funnit, funnen) w = 
 verb $ ppvar n (verb_prefixed n finna finner [] finn fann [] funnit funnen w) w

--verb_wc :: Int -> [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
--verb_wc n finna finner finn fann funnit funnen w = 
-- verb $ verb_prefixed n (map e finna) (map e finner) []
--                        (map e finn) (map e fann)  []
--                        (map e funnit) (map e funnen) w

vbm_part :: Int -> (Suffixes,Suffixes) -> (String -> Entry)
vbm_part n (avbländande,avbländad) w =
   verb $ vbmpart n avbländande avbländad w

verb_dwc :: Int -> [String] -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
verb_dwc n finna finn fann funnit funnen w = 
 verb $ no_part_pres $ no_active $ verb_prefixed n (map e finna) [] [] 
                                    (map e finn) (map e fann) [] 
                                    (map e funnit) (map e funnen) w 

verb_deponens :: Int -> (Suffixes, Suffixes, Suffixes, Suffixes, Suffixes) -> (String -> Entry)
verb_deponens n (finna,finn,fann,funnit,funnen) w = 
 verb $ no_part_pres $ no_active $ verb_prefixed n finna [] [] finn fann [] funnit funnen w 

-- verbs :: [String] -> Verb -> [Entry]
-- verbs prefs v = map verb [\f -> strings (map (p ++) (unStr (v f))) | p <- prefs] 
-- Conjugation 1

v1 :: String -> Entry
v1 = verb . no_konj . vb1

-- Conjugation 2

v2 :: String -> Entry
v2 = verb . no_konj .vb2

vb_2a_följa :: String -> Entry
vb_2a_följa  = verb . no_konj .vb2följa

vb_2a_sända :: String -> Entry
vb_2a_sända  = verb . no_konj .vb2sända

vb_2a_knäcka :: String -> Entry
vb_2a_knäcka = verb . no_konj .vb2knäcka

vb_2a_vända :: String -> Entry
vb_2a_vända  = verb . no_konj .vb2vända

vb_2a_dröja :: String -> Entry
vb_2a_dröja  = verb . no_konj .vb2dröja

vb_2a_göra :: String -> Entry
vb_2a_göra   = verb  . no_konj .vb2göra

-- Conjugation 3

v3 :: String -> Entry
v3 = verb . no_konj . vb3

vb_3a_klä :: String -> Entry
vb_3a_klä  = verb . no_konj . vb3klä

-- Conjugation 4

vb_4a_se :: String -> Entry
vb_4a_se    = verb  . vb4se

vb_4a_gå :: String -> Entry
vb_4a_gå    = verb  . vb4gå

vb_4a_bita :: String -> Entry
vb_4a_bita  = verb  . vb4bita

vb_4m_vina :: String -> Entry
vb_4m_vina  = verb  . vb4vina

vb_4a_krypa :: String -> Entry
vb_4a_krypa = verb  . vb4krypa

vb_4a_vinna :: String -> Entry
vb_4a_vinna = verb  . vb4vinna

vb_4a_giva :: String -> Entry
vb_4a_giva  = verb  . vb4giva

vb_4a_binda :: String -> Entry
vb_4a_binda = verb  . vb4binda

vb_4a_bliva :: String -> Entry
vb_4a_bliva = verb  . no_passive . vb4bliva

vb_4a_draga :: String -> Entry
vb_4a_draga = verb  . vb4draga

vb_4a_göra :: String -> Entry
vb_4a_göra  = verb  . vb4göra

vb_4m_stå :: String -> Entry
vb_4m_stå   = verb  . vb4stå

vb_4a_slå :: String -> Entry
vb_4a_slå   = verb  . vb4slå

vb_4a_ta :: String -> Entry
vb_4a_ta    = verb  . vb4taga

vb_4a_få :: String -> Entry
vb_4a_få    = verb  . vb4få

vb_4a_ha :: String -> Entry
vb_4a_ha    = verb  . vb4hava

vb_4a_vara :: String -> Entry
vb_4a_vara  = verb  . vb4vara

vb_4a_fara :: String -> Entry
vb_4a_fara  = verb  . vb4fara

vb_4a_komma :: String -> Entry
vb_4a_komma = verb  . vb4komma

vb_4a_äta :: String -> Entry
vb_4a_äta   = verb  . vb4äta

vb_4a_låta :: String -> Entry
vb_4a_låta  = verb  . vb4låta

vb_4a_falla :: String -> Entry
vb_4a_falla = verb  . vb4falla

vb_4a_supa :: String -> Entry
vb_4a_supa  = verb  . vb4supa

vb_4a_hålla :: String -> Entry
vb_4a_hålla = verb  . vb4hålla

-- deponent verbs

vb_1s_hoppas :: String -> Entry
vb_1s_hoppas   = verb . no_konj . vbdhoppas

vb_1s_lyckas :: String -> Entry
vb_1s_lyckas   = verb . no_konj . vbdlyckas

vb_1s_nalkas :: String -> Entry
vb_1s_nalkas   = verb . no_konj . vbdnalkas

vb_1s_färdas :: String -> Entry
vb_1s_färdas   = verb . no_konj . vbdfärdas

vb_2s_synas :: String -> Entry
vb_2s_synas    = verb . no_konj . vbdsynas

vb_4d_vederfås :: String -> Entry
vb_4d_vederfås = verb . no_konj . vbdvederfås

-- pending verbs

vb_va_koka :: String -> Entry
vb_va_koka   = verb . no_konj . vbvkoka 

vb_va_mista :: String -> Entry
vb_va_mista  = verb . no_konj . vbvmista


-- vb_va_sprida = verb . no_konj . vbvsprida

vb_va_bringa :: String -> Entry
vb_va_bringa = verb . no_konj . vbvbringa

vb_va_tala :: String -> Entry
vb_va_tala   = verb . no_konj . vbvtala

-- verb phrases
--vbm1_p  v pr   = verbM $ \(VM p) -> mapStr (\s -> unwords [s,pr] ) ((vb1 v) p) 
--vbm1_pr v pr r = verbM $ \(VM p) -> mapStr (\s -> unwords [s,pr,r] ) ((vb1 v) p)

vbm_dö_ut :: String -> Entry
vbm_dö_ut = verb . vbmdöut
