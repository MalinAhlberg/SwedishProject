module TestSw where

import General
import List
import Frontend
import Char
import Maybe
import TestGenSw
import qualified Data.Set as Set

tests :: (PositiveTests, NegativeTests)
tests = (pos,neg)
 where 
 pos = pos_gen_rules ++ pos_noun_rules ++ pos_verb_rules ++ pos_adj_rules
 neg = neg_gen_rules ++ neg_noun_rules ++ neg_verb_rules ++ neg_adj_rules

-- General rules
pos_gen_rules = [--pos_id_para, multi_word, head_lemma,
                 paradigm_pos]

neg_gen_rules = [three_of_same, symbols, mmn,sms_missing_hyphen,end_with_h]

-- Is POS in lemma id = paradigm id?
pos_id_para :: TestInput -> Result
pos_id_para t 
 | not ( is_a_word t) = pass
 | p_pos t == id_pos t = pass
 | otherwise           = message t "lemma_id pos - paradigm pos mismatch"

-- Have the multi words more than one word?
multi_word :: TestInput -> Result
multi_word t
 | not (is_multi_word t)                             = pass
 | length (words (thead t)) > 1                      = pass
 | otherwise                                         = message t "singleton multi word"

--  Is head == lemma id head?
head_lemma :: TestInput -> Result
head_lemma t 
 | not ( is_a_word t) = pass
 | elem (tid t) ["SS_433..pmm.1"]                = pass
 | norm_id_lemma t == head_to_lemma_id t         = pass
 | otherwise                                     = message t "lemma_id_head - head mismatch"

-- paradigm pos == pos?
paradigm_pos :: TestInput -> Result
paradigm_pos t
 | not ( is_a_word t) = pass
 | p_pos t == tcat t = pass
 | otherwise = message t "paradigm_pos - pos mismatch"

-- every word, with some exceptions, should contain a vowel.
contains_vowel :: TestInput -> Result
contains_vowel t
  | is_abbr t || elem (tcat t) ["nl","in"]            = pass
  | length (thead t) <= 1                             = pass
  | or [is_vowel c | c <- w t] || null (w t)          = pass
  | otherwise                                         = message t "no vowel"

-- every word, with some exceptions, should contain a consonant.
contains_consonant :: TestInput -> Result
contains_consonant t
 | not ( is_a_word t) = pass
 | is_abbr t || (tcat t) == "nl"                = pass
 | length (thead t) <= 1                        = pass
  | or [is_consonant c | c <- w t] || null (w t) = pass
 | otherwise                                    = message t "no consonant"

-- not three of same.
three_of_same :: TestInput -> Result
three_of_same t
 | is_abbr t || (tcat t) == "nl" = pass
 | traverse (w t) 
    $ \s -> (case s of 
              (c:c1:c2:_) | (c == c1) && (c1 == c2) -> True
              _           -> False) = message t "three of same"
 | otherwise = pass

-- no consecutive symbols.
symbols :: TestInput -> Result
symbols t
 | elem (tid t) ["HCO3-..nna.1","OH-..nna.1","e-..nna.1"] = pass
 | traverse (w t) 
   $ \s -> (case s of 
             (c:c1:_) | c == c1 && elem c "-:\"+." -> True
             _        -> False) = message t "consecutive symbols"
 | otherwise = pass


-- no mmn substring.
mmn :: TestInput -> Result
mmn t
 | member_str "mmn" (w t) = message t "mmn"
 | otherwise                         = pass

-- Noun Rules
pos_noun_rules = [--decl1,decl2,decl3,decl4,decl5,decl7,missing_noun_cases,
                  paradigm_gender]

neg_noun_rules = [decl0,no_sing, gender_d,medial_compound,gender_u,gender_n]

no_sing :: TestInput -> Result
no_sing t
 | is_a_word t && (tcat t == "nn" || tcat t == "nnm") && (gender t 'p' || gender t 'd') && param t ["sg"] = message t "invalid singular"
 | otherwise = pass


plural_def :: TestInput -> Result
plural_def t 
 | gender t 'u' && elem '7' (tpara t) = pass
 | is_a_word t && (tcat t == "nn") && param t ["def","pl","nom"] && 
   gender t 'u' && not (isSuffixOf "na" (w t)) = message t $ "invalid definite plural"
 | is_a_word t && (tcat t == "nn") && param t ["def","pl","nom"] && 
   gender t 'n' && not (isSuffixOf "a" (w t) || isSuffixOf "en" (w t)) = message t $ "invalid definite plural"
 | is_a_word t && param t ["pl"] && (isSuffixOf "aor" (w t) || isSuffixOf "aorna" (w t)) = message t $ "invalid 'a'"
 -- | is_a_word t && param t ["pl"] && (isSuffixOf "ear" (w t) || isSuffixOf "earna" (w t)) = message t $ "invalid 'e'"
 | otherwise = pass

in_declension :: TestInput -> Char -> String -> Result
in_declension t c suff
 | is_a_word t && (tcat t == "nn") && decl t c && param t ["indef","pl","nom"] && not (isSuffixOf suff (w t)) = message t $ "invalid plural in declension " ++ [c]
 | otherwise = pass

not_in_declension :: TestInput -> Char -> [String] -> Result
not_in_declension t c xs
 | is_a_word t && (tcat t == "nn") && decl t c && param t xs = message t $ "invalid \"" ++ unwords xs ++ "\" in declension " ++ [c]
 | otherwise = pass

decl1 t = in_declension t '1' "or"
decl2 t = in_declension t '2' "ar"
decl3 t = in_declension t '3' "er"
decl4 t = in_declension t '4' "r"
decl5 t = in_declension t '5' "n"
decl7 t = in_declension t '7' "s"

decl0 t 
 | tcat t == "nn" = not_in_declension t '0' ["pl"]
 | otherwise      = pass

gender_d t
 | is_a_word t && gender t 'd' && not (param t ["pl","def"]) = message t  "gender 'd' implies pl def"
 | otherwise                                                 = pass

gender_u t
 | elem (tpara t) ["nn_0u_praxis", "nn_0u_samverkan"] = pass
 | last (tcat t) == 'm' = pass 
 | is_a_word t && gender t 'u' && (param t ["sg","def","nom"]) && (last (w t) == 't') = message t  " invalid sg def form"
 | otherwise                                                   =   pass


gender_n t
 | elem (tpara t) ["nn_6n_deponens","nn_6n_aber","nn_3n_dominion","nn_0n_gluten","nn_0u_praxis","nn_0n_bitumen"] = pass
 | last (tcat t) == 'm' = pass 
 | is_a_word t && gender t 'n' && (param t ["sg","def","nom"]) && (last (w t) == 'n') = message t  " invalid sg def form"
 | otherwise                                                   =   pass


medial_compound t 
  | is_a_word t && param t ["cm"] &&  last (w t) == 's' && bad_medials (w t) = message t  "invalid binding s"
  | otherwise                                        = pass
 where bad_medials = f . reverse . take 2 . reverse . init
       f [_,'x'] = True
       f [_,'z'] = True
       f ['s',x] = is_consonant x
       f ['x',x] = is_consonant x
       f _       = False

sms_missing_hyphen t
 | is_a_word t && param t ["sms"] && not (last (w t) == '-') = message t "missing hyphen in sms"
 | otherwise                                                 = pass


-- (x:y:_) | ((is_consonant x) && y == 's') || ((is_consonant x) && y == 'x') || [y,x] == "ch" || x=='x' || x =='z' -> []
-- decli t = not_in_declension t 'i' ["gen"]

missing_noun_cases :: TestInput -> Result
missing_noun_cases t
 -- | param t ["ci"] || param t ["cm"] = pass
  | elem (tpara t) ["nna_6n_kg","nn_6u_mat"] = pass 
  | not_a_word t && (tcat t == "nn" || tcat t == "nna") &&  
    elem (head (p_annotation t)) "1234567" && (not (gender t 'p' || gender t 'd')) = message t "missing noun case"
  | otherwise                                = pass

paradigm_gender :: TestInput -> Result
paradigm_gender t
 | elem (tcat t) ["nn","nna","nnm"] = if gender t (head (head (tinhs t))) then pass else message t "paradigm gender-gender mismatch"
 | otherwise = pass

--- Verb Rules
pos_verb_rules = [conj1, conj2, conj3, pret_part,konj]
neg_verb_rules = [no_pret_part, deponens,mms,end_with_h,konj_e, konj_2_es]

in_conjugation :: TestInput -> Char -> String -> String -> Result
in_conjugation t c suff suff2
 | is_a_word t && (tcat t == "vb") && conj t c && param t ["pres","ind","aktiv","1p"] && not (isSuffixOf suff (w t))  = message t $ "invalid ending in conjugation " ++ [c]
 | is_a_word t && (tcat t == "vb") && conj t c && param t ["pret","ind","aktiv","1p"] && not (isSuffixOf suff2 (w t)) = message t $ "invalid ending in conjugation " ++ [c]
 | otherwise = pass

conj1 t = in_conjugation t '1' "ar" "ade"
conj2 t 
 | elem (tid t) ["tåla..vb.1","må..vb.2"] = pass
 | otherwise = in_conjugation t '2' "r"  ""
conj3 t = in_conjugation t '3' "r"  "de"
conj4 t = in_conjugation t '4' "er" ""

konj :: TestInput -> Result
konj t
 | not_a_word t && tcat t == "vb" && (conj t '4' || conj t 'v') && param t ["konj", "aktiv"] && not (is_verb_sd t) = message t "missing konj"
 | otherwise                            = pass

konj_e :: TestInput -> Result
konj_e t
 | is_a_word t &&  tcat t == "vb" && param t ["konj", "aktiv","1p"]  && (last (w t) /= 'e') = message t "missing 'e' on konj"
 | otherwise                            = pass

end_with_h :: TestInput -> Result
end_with_h t
 | is_a_word t && tcat t == "vb" &&  elem (dp 2 (w t)) ["ch","sh"] = pass
 | is_a_word t && tcat t == "vb" && last (w t) == 'h' = message t "word ending with h"
 | otherwise                        = pass

konj_2_es :: TestInput -> Result
konj_2_es t
 | is_a_word t && tcat t == "vb" && conj t '2' && (param t ["s-form","pres","p1"]) && 
   ((dp 2 (w t)) /= "es") && (dp 1 (tk 1 (thead t)) == "s") = message t "s-form not ending with 'es' in conj. 2"
 | otherwise                        = pass

deponens t 
 | param t ["pret_part"] || param t ["pres_part"] || param t ["imper"] = pass
 | is_a_word t && tcat t == "vb" = case p_annotation t of
                     (_:'s':_) | not (param t ["s-form"]) -> message t $ "invalid deponent"
                     _ -> pass
 | otherwise = pass

is_deponens t =  case p_annotation t of
                   (_:'s':_) -> True
                   _        -> False

no_pret_part :: TestInput -> Result
no_pret_part t
 | is_a_word t && is_no_pret_part t && param t ["pret_part"]  = message t "invalid participle"
 | otherwise = pass

pret_part :: TestInput -> Result
pret_part t
 | not_a_word t && is_pret_part t && param t ["pret_part"] = message t "missing participle"
 | otherwise = pass

vbm_paradigm :: TestInput -> Result
vbm_paradigm t
 | tcat t == "vbm" = 
     case p_annotation t of
       (x:y:a:_) | elem x "01234ov" && elem y "asmd" && elem a "pldztq" -> pass
       _ -> message t "invalid vbm paradigm identifier"
 | otherwise = pass

-- no mms suffix.
mms :: TestInput -> Result
mms t
 | (tcat t == "vb") && isSuffixOf "mms" (w t) = message t "mms suffix"
 | otherwise                                  = pass

-- Adjective Rules
pos_adj_rules = [komp]
neg_adj_rules = [no_komp,no_genitive_avm]

no_komp :: TestInput -> Result
no_komp t
 | is_a_word t && is_no_komp t && not (null [x | x <- (tparam t), elem x ["komp","super"]]) = message t "invalid comparative"
 | otherwise                            = pass

komp :: TestInput -> Result
komp t
 | elem (tpara t) ["avm_1p1_bred","avm_1p1_akut"] && param t ["gen"] = pass
 | not_a_word t && is_komp t && not (null [x | x <- (tparam t), elem x ["komp","super"]]) = message t "missing comparative"
 | otherwise                            = pass

no_genitive_avm :: TestInput -> Result
no_genitive_avm t
 | is_a_word t && tcat t == "avm" = 
     case p_annotation t of
       (_:'p':_) | elem "gen" (tparam t) -> message t "invalid genitive"
       _ -> pass
 | otherwise = pass
