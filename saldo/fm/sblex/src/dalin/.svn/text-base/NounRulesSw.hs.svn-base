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

-- additional compound form
compvariants :: [NComp] -> [NComp] -> Substantive -> Substantive
compvariants compi compm f = variants f [
                              (InitComposite,strings compi),
                              (MedComposite,strings compm),
                              (SMS,strings [ x | x@(_:_) <- compi,last x=='-'])
                             ] 

