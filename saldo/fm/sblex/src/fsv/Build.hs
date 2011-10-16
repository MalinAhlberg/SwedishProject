module Build where

import General
import Dictionary
import Types
import Rules

-- combinators for lexicographers

noun :: NN -> Gender -> Paradigm -> Entry
noun n g = entryIP n [prValue g]

nnNeutrum s = noun s Neut
nnMasc s    = noun s Masc
nnFem s     = noun s Fem

nn_masc_biti s = nnMasc (biti_rule s) "masc_biti"

nn_masc_bryti s = nnMasc  (bryti_rule s) "masc_bryti"

nn_masc_landbo s = nnMasc  (landbo_rule s) "masc_landbo"

nn_fem_gata s = nnFem (gata_rule s) "fem_gata"

nn_fem_kyrkia s = nnFem  (kyrkia_rule s) "fem_kyrkia"

nn_fem_fru s = nnFem (fru_rule s) "fem_fru"

nn_fem_glaethi s = nnFem (glaethi_rule s) "fem_glaethi"

nn_neut_ogha s = nnNeutrum (ogha_rule s) "neut_ogha"

nn_masc_fisker s = nnMasc  (fisker_rule s) "masc_fisker"

nn_masc_aengil s = nnMasc (aengil_rule s) "masc_aengil"

nn_masc_sko s = nnMasc (sko_rule s) "masc_sko"

nn_masc_aptan s = nnMasc (aptan_rule s) "masc_aptan"

nn_masc_vaever s = nnMasc (vaever_rule s) "masc_vaever"

nn_masc_laegger s = nnMasc (laegger_rule s) "masc_laegger"

nn_masc_oeri s = nnMasc (oeri_rule s) "masc_oeri"

nn_masc_raetter s = nnMasc (raetter_rule s) "masc_raetter"

nn_masc_bondi s = nnMasc (bondi_rule s) "masc_bondi"

nn_masc_son s  =  nnMasc (son_rule s) "masc_son"

nn_masc_mather s  =  nnMasc (mather_rule s) "masc_mather"

nn_masc_foter s  =  nnMasc (foter_rule s) "masc_foter"

nn_masc_fathir s  =  nnMasc (fathir_rule s) "masc_fathir"

nn_masc_eghandi s  =  nnMasc (eghandi_rule s) "masc_eghandi"

nn_fem_agn s = nnFem (agn_rule s) "fem_agn"

nn_fem_bro s = nnFem (bro_rule s) "fem_bro"

nn_fem_gas s = nnFem (gas_rule s) "fem_gas"

nn_fem_bok s = nnFem (bok_rule s) "fem_bok"

nn_fem_ko  s = nnFem (ko_rule s) "fem_ko"

nn_fem_mothir  s = nnFem (mothir_rule s) "fem_mothir"

nn_fem_faerth s = nnFem (faerth_rule s) "fem_faerth"

nn_fem_heth s = nnFem (heth_rule s) "fem_heth"

nn_fem_aeg s = nnFem (aeg_rule s) "fem_aeg"

nn_neu_skip s = nnNeutrum (skip_rule s) "neu_skip"

nn_neu_bo s = nnNeutrum (bo_rule s) "neu_bo"

nn_neu_hovuth s = nnNeutrum (hovuth_rule s) "neu_hovuth"

nn_neu_trae s = nnNeutrum (trae_rule s) "neu_trae"

nn_neu_skaer s = nnNeutrum (skaer_rule s) "neu_skaer"

nn_neu_minne s = nnNeutrum (minne_rule s) "neu_minne"

nn_neu_aepli s = nnNeutrum (aepli_rule s) "neu_aepli"

av_blar s = entryP (blar_rule s) "blar"

av_langer s = entryP (langer_rule s) "langer"

av_helagher s =  entryP (helagher_rule s) "helagher"

ab_opta s =  entryP (opta_rule s) "opta"

ab_laenger s =  entryP (laenger_rule s) "længer"

ab_yter s =  entryP (yter_rule s) "yter"

ord_fyrsti s = entryP (ord_fyrsti_rule s) "fyrsti"

numeral :: [[String]] -> Entry
numeral xs = entryP f "numeral"
 where f :: Numeral
       f = giveValues (map strings xs)

vb_aelska s = entryP (aelska_rule s) "aelska"

vb_kraevia s = entryP (kraevia_rule s) "kraeva"

vb_foera  s = entryP (foera_rule s) "foera"

vb_liva   s = entryP (liva_rule s) "liva"

vb_vika s = entryP (vika_rule s) "vika"

vb_laesa s = entryP (laesa_rule s) "laesa"

pn_sin :: String -> Entry
pn_sin = entry . sin_rule

pn_var :: String -> Entry
pn_var      _ = entry var_rule

pn_ithar :: String -> Entry
pn_ithar    _ = entry ithar_rule

pn_thaen :: String -> Entry
pn_thaen    _ = entry thaen_rule

pn_thaeni :: String -> Entry
pn_thaeni   _ = entry thaeni_rule

pn_hvar :: String -> Entry
pn_hvar     _ = entry hvar_rule

pn_hvilken :: String -> Entry
pn_hvilken  _ = entry hvilkin_rule

pn_nokor :: String -> Entry
pn_nokor    _ = entry nokor_rule

pn_aegin :: String -> Entry
pn_aegin    _ = entry aengin_rule

pn_hvaer :: String -> Entry
pn_hvaer    _ = entry hvaer_rule

pn_hvarghin :: String -> Entry
pn_hvarghin _ = entry hvarghin_rule

pn_person :: String -> Entry
pn_person   _ = entry person_rule

pn_hanhon :: String -> Entry
pn_hanhon   _ = entry hanhon_rule

num_en :: String -> Entry
num_en _ = numeral 
  [
   ["en","æn","in"], ["ens"],   ["enum","enom"], ["en","æn","in"],
   ["en"],           ["ennar"], ["enni","enne"], ["ena"],
   ["et","æt","it"], ["ens"],   ["enu","eno"],   ["et","æt","it"]
  ]

num_twe :: String -> Entry
num_twe _ = numeral 
  [
    ["twe","twer","twa"], ["twæggia","twiggia"],   ["twem"], ["twa"],
   ["twa","twar"],["twæggia","twiggia"], ["twem"], ["twa","twar"],
   ["tu"],["twæggia","twviggia"] ,   ["twem"],   ["tu"]
  ]

num_thri :: String -> Entry
num_thri _ = numeral 
  [
    ["þri","þrir","þre"], ["þriggia"], ["þrim","þrem"], ["þre","þrea"],
    ["þrea","þrear","þre"],["þriggia"],["þrim","þrem"], ["þrea","þrear","þre"],
    ["þry"],["þriggia"] ,   ["þrim","þrem"],   ["þry"]
  ]
  
num_fiuri :: String -> Entry
num_fiuri _ = numeral 
  [
   ["fiuri","fiurir","fyri","fyrir"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiura","fyra"],
   ["fiura","fiurar","fyra","fyrar"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiura","fiurar","fyra","fyrar"],
   ["fiughur"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiughur"]
  ]


num_bathi :: String -> Entry
num_bathi _ = 
  numeral 
  [
    ["baþi","baþir"], ["bæggia"], ["baþum","baþom"], ["baþa"],
    ["baþa","baþar"],["bæggia"],["baþum","baþom"], ["baþa","baþar"],
     ["baþi","baþin","baþe","baþen","bæþi","bæþin"],
     ["bæggia"],["baþum","baþom"], ["baþi","baþin","baþe","baþen"]
  ]

instance Dict NN_Form where 
    category _ = "nn"
    dictword f = case unStr (f (NN_C Sg Nom Indef)) of
                   (x:_) -> x
                   _     -> getDictWord f

instance Dict VerbForm where
    category _ = "vb"
    dictword f = case unStr (f (Inf Act)) of
                   (x:_) -> x
                   _     -> getDictWord f


instance Dict NumeralForm where
    category _ = "num"

instance Dict OrdinalForm where
    category _ = "ord"

instance Dict AdjForm where
    category _ = "av"
    dictword f = case unStr (f (StrongPos Masc Sg Nom)) of
                   (x:_) -> x
                   _     -> getDictWord f

instance Dict PNForm where
    category _ = "pn"

instance Dict PPNForm where
    category _ = "pn"

instance Dict AdverbForm where
    category _ = "ab"
