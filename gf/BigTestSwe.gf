--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigTestSwe of BigTest = 
  NounSwe, -- - [DetNP],
  VerbSwe -[PassV2,ComplSlash,ReflVP], --,ComplVA], -- [ComplVS],
  AdjectiveSwe -[UseComparA],
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, --[UseCl, UseRCl], 
  QuestionSwe,
  RelativeSwe, -- [IdRP, RelSlash],
  ConjunctionSwe,
  PhraseSwe, -- [UttImpSg, UttImpPl],
  TextX,
  TenseX,
  StructuralSwe, 
  ExtraSwe,
  LexiconSwe
  ** open IrregSwe, ParamX, CommonScand in {
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;

  lin
   
    begin_VV    = mkVV (mkV "börja") ;
    tankaTill_V = partV (mk2V "tänka" "tänkte") "till" ;
    become_V2 = mkV2 (mkV "bli" "blir""bli" "blev" "blivit" "bliven") ;
    numberOf_N2 = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep ;
    gallande_A  = mkA "gällande" "gällande" "gällande" "gällande" "gällande";
      -- suprelativ..
    johan_PN = regPN "johan" ;
    do_VV = mkVV (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord") ;
    peppad_A = compoundA "peppad" "peppat" "peppade";


    sadana_PronAQ = mkA "sådan" ;
    fler_PronAD   = mkA "flera" "flera" "flera" "fler" "flest" ;
    hela_Predet   = {s  = \\_,_ => "hela" ; p = [] ; a = PNoAg} ;
    varenda_Det = {s  = \\_,_ => "varenda" ; sp = \\_,_ => "varenda en" ; 
                   n = Sg ; det = DDef Indef};
    vardera_Det = {s,sp = \\_,_ => "vardera" ; n = Sg ; det = DDef Indef};
    sjaelva_Predet   = {s  = \\_,_ => "själva" ; p = [] ; a = PNoAg} ;
    samma_Predet   = {s  = \\_,_ => "samma" ; p = [] ; a = PNoAg} ;
    ena_Det = {s,sp = \\_,_ => "ena" ; n = Sg ; det = DDef Def};
    noll_Det = {s,sp = \\_,_ => "noll" ;
                n = Pl ; det = DDef Indef};
} ;
