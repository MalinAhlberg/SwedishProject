module Dict where

import Build
import Rules
import Types
import General
import Dictionary

dict :: Dictionary
dict = dictionary $ [] 
       
{- moved internal to external
map identifier
 ([
  entry ord_annar_rule
 ] ++ num ++ pron)
 where identifier e = set_lemma_id (get_head e ++ ".." ++ get_pos e ++ ".1") e

-- Numeraler
num :: [Entry]
num = 
 [
  numeral 
  [
   ["en","æn","in"], ["ens"],   ["enum","enom"], ["en","æn","in"],
   ["en"],           ["ennar"], ["enni","enne"], ["ena"],
   ["et","æt","it"], ["ens"],   ["enu","eno"],   ["et","æt","it"]
  ],
  numeral 
  [
   ["twe","twer","twa"], ["twæggia","twiggia"],   ["twem"], ["twa"],
   ["twa","twar"],["twæggia","twiggia"], ["twem"], ["twa","twar"],
   ["tu"],["twæggia","twviggia"] ,   ["twem"],   ["tu"]
  ],
  numeral 
  [
   ["þri","þrir","þre"], ["þriggia"], ["þrim","þrem"], ["þre","þrea"],
   ["þrea","þrear","þre"],["þriggia"],["þrim","þrem"], ["þrea","þrear","þre"],
   ["þry"],["þriggia"] ,   ["þrim","þrem"],   ["þry"]
  ],
  numeral 
  [
   ["fiuri","fiurir","fyri","fyrir"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiura","fyra"],
   ["fiura","fiurar","fyra","fyrar"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiura","fiurar","fyra","fyrar"],
   ["fiughur"], ["figuhura","figuhurra","fiura","fyra"], 
   ["fiurum","fyrom"], ["fiughur"]
  ],
  numeral 
  [
   ["baþi","baþir"], ["bæggia"], ["baþum","baþom"], ["baþa"],
   ["baþa","baþar"],["bæggia"],["baþum","baþom"], ["baþa","baþar"],
   ["baþi","baþin","baþe","baþen","bæþi","bæþin"],
   ["bæggia"],["baþum","baþom"], ["baþi","baþin","baþe","baþen"]
  ]
 ]


-- Pronomen
pron = 
    [
     entry (sin_rule "sin"),
     entry (sin_rule "min"),
     entry (sin_rule "þin"),
     entry var_rule,
     entry ithar_rule,
     entry thaen_rule,
     entry thaeni_rule,
     entry hvar_rule,
     entry hvilkin_rule,
     entry nokor_rule,
     entry aengin_rule,
     entry hvaer_rule,
     entry hvarghin_rule,
     entry person_rule,
     entry hanhon_rule
    ]
-}
