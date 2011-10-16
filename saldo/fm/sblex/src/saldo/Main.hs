module Main where

import CommonMain
-- import DictSw
import CommandsSw
import Frontend
import TestSw
import Compound
import Multiwords
import qualified Data.Set as Set
import Dictionary(Entry,set_lemma_id)
import qualified Dict.Abs as Abs
import EncodeSw
import Attr
import PrintSw
import GenRulesSw

main :: IO ()
main = commonMain SAL

data SAL = SAL
 deriving Show

instance Language SAL where
 morphology_header _ = "FM-SALDO 2.0\n  L. Borin, M. Forsberg & A. Ranta, 2010, under GNU LGPL 3.0 or CC-SA 2.5 Generic"
 name         _ = "swe"
-- internDict   _ = swedishDict
 paradigms    _ = foldr insertCommand emptyC commands
 composition  _ = Just $ compDesc
 testBench _ = tests
 dup_id_exceptions _ = sal_id_exceptions
 encoding _ = sw_encodings
 termParser _ ts e = add_id ts e
 affixes _ = sal_affixes
 sandhi_rules _ = saldo_sandhi
 word_attr    _ = [h_attr,w_attr,wp_attr]
 lprinter     _ = print_table
 dictionary_postprocessing _ = multiwords
 paradigm_dup_exception _ = Set.fromList ["av_1_akut",
                                          "vbm_1ic_laga","vbm_1sp2_andas","vbm_2mi2_leda","vbm_2ms1_skilja", -- unimplemented
                                          "vbm_4ms1_förhålla","vbm_4ms1_utfalla","vbm_4msp1_finnas","vbm_vmsp1_ryckas","vbm_vsp1_talas",
                                          "vbm_2mps1_göra","vbm_2mps1_väga","vbm_1mps1_laga","vbm_2mps1_hyra","vbm_2mps1_mysa",
                                          "vbm_2mps1_sätta","vbm_4mps1_ge","vbm_4mps1_slå","vbm_4mps1_komma"
                                         ]

compDesc :: CompDesc
compDesc = [
 [ attr [h_attr, w_attr, wp_attr] ],
 [ attr [init_attr,wp_attr,c_attr], star [medial_attr], attr [h_attr,wp_attr,s_attr] ]
 ]

saldo_sandhi :: (String,String) -> [(String,String)]
saldo_sandhi (x,y@(c:_)) = 
    case reverse x of
      (c1:c2:_) | c1 == c2 && c2 == c -> []
      (c1:_)    | is_consonant c1 && c1 == c -> [(x ++ [c],y),(x,y)]
      _                               -> [(x,y)]
saldo_sandhi x = [x]

sal_affixes :: Set.Set String
sal_affixes = Set.fromList ["en","ens","ar","ars","or","ors"]

 -- wordGuesser  _ = test

-- test s
--  | last s == 'x' = ["A strange word"]
--  | otherwise     = []

add_id :: [Abs.Term] -> Entry -> Entry
add_id [Abs.TermC _ [(Abs.TermA (Abs.NStr s))]]  e = set_lemma_id s e
add_id _                                         e = e
