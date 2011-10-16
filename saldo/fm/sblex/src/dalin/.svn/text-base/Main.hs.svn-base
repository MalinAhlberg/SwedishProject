module Main where

import CommonMain
-- import DictSw
import CommandsSw
import Frontend
import Compound
import qualified Data.Set as Set
import Dictionary(Entry,set_lemma_id)
import qualified Dict.Abs as Abs
import Attr
import GenRulesSw
import TestSw
import PrintSw
import EncodeSw

main :: IO ()
main = commonMain DALIN

data DALIN = DALIN
 deriving Show

instance Language DALIN where
 morphology_header _ = "FM-DALIN 1.0\n  L. Borin & M. Forsberg, under GNU LGPL 3.0 or CC-SA 2.5 Generic"
 name         _ = "swe"
 paradigms    _ = foldr insertCommand emptyC commands
 composition  _ = Nothing --Just $ compDesc
 termParser _ ts e = add_id ts e
 sandhi_rules _ = saldo_sandhi
 testBench _    = tests
 encoding _ = sw_encodings
 lprinter     _ = print_table
 word_attr    _ = [h_attr,w_attr,wp_attr]
 
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

add_id :: [Abs.Term] -> Entry -> Entry
add_id [Abs.TermC _ [(Abs.TermA (Abs.NStr s))]]  e = set_lemma_id s e
add_id _                                         e = e
