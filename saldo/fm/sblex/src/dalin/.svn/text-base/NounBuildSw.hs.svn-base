module NounBuildSw where

import General
import Dictionary
import GenRulesSw
import TypesSw
import RulesSw
import Char
import Attr

substantive :: Substantive -> Genus -> Entry
substantive n g = entryI (hyphenate_compounds n) [prValue g]

noun_f :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_f n g (apa,apan,apor,aporna) s = noun_compound n g (apa,apan,apor,aporna,apa,apa) s 

noun_compound_ng :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_compound_ng n g (apa,apan,apor,aporna,ap,ap2) s = substantive (missing n_f [(SF n d Gen) | n <- values, d <- values]) g 
  where n_f p = (mk_substantive_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff ap) (suff ap2) (suff ap)) p
        suff    = apply_suffixes (tk n s)

noun_compound :: Int -> Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_compound n g (apa,apan,apor,aporna,ap,ap2) s = substantive n_f g 
  where n_f p = (mk_substantive_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff ap) (suff ap2) (suff ap)) p
        suff    = apply_suffixes (tk n s)

noun_no_genitive :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
noun_no_genitive g (apa,apan,apor,aporna) s = substantive (missing n_f [(SF n d Gen) | n <- values, d <- values]) g 
  where n_f p = (mk_subst_v (suff apa) (suff apan) (suff apor) (suff aporna) (suff apa)) p
        suff    = apply_suffixes s

nna :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
nna g (apa,apas,apan,apans,apor,apors,aporna,apornas) s = 
 replace_attr wp_attr w_attr $ replace_attr h_attr w_attr $ set_pos "nna" $ substantive n_f g 
  where n_f p = (mk_nna (suff apa) (suff apas) (suff apan) (suff apans) (suff apor) (suff apors) 
                        (suff aporna) (suff apornas)) p
        suff          = apply_suffixes s . connect s
        connect s end = concat [[(f,(':':e)),(f,('-':e)),(f,e)]  | (f,e@(_:_)) <- end] ++ (filter (null . snd) end)


nnac :: Genus -> (Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes,Suffixes) -> (String -> Entry)
nnac g (apa,apas,apan,apans,apor,apors,aporna,apornas) s = 
 replace_attr wp_attr w_attr $ replace_attr h_attr w_attr $ set_pos "nna" $ substantive n_f g 
  where n_f p = (mk_nna (suff apa) (suff apas) (suff apan) (suff apans) (suff apor) (suff apors) 
                        (suff aporna) (suff apornas)) p
        suff          = apply_suffixes s . connect s
        connect s end = concat [[(f,(':':e)),(f,e)]  | (f,e@(_:_)) <- end] ++ (filter (null . snd) end)

noun :: Genus -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
noun g apa apan apor aporna s = nounC g apa apan apor aporna apa s 

nounC :: Genus -> [String] -> [String] -> [String] -> [String] -> [String] -> (String -> Entry)
nounC g apa apan apor aporna ap s = substantive n_f g 
  where n_f p = (mk_subst_v apa' apan' apor' aporna' ap') p
        suff    = (s ++)
        apa'    = map suff apa
	apan'   = map suff apan
        apor'   = map suff apor
	aporna' = map suff aporna
	ap'     = map suff ap

