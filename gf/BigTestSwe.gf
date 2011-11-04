--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigTestSwe of BigTest = 
  NounSwe -- - [DetNP],
  {-
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
  TextX - [Tense,Temp] ,
  TenseSwe,
  StructuralSwe, 
  ExtraSwe,
  LexiconSwe
  -}
  ** open IrregSwe, ParamX CommonSwe in {};
   {-
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;
      -}
{-
  lin
   
    begin_VV    = mkVV (mkV "börja") ;
    tankaTill_V = partV (mk2V "tänka" "tänkte") "till" ;
    become_V2 = mkV2 (mkV "bli" "blir""bli" "blev" "blivit" "bliven") ;
    numberOf_N2 = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep ;
    gallande_A  = mkA "gällande" "gällande" "gällande" "gällande" "gällande";
      -- suprelativ..
    johan_PN = regPN "johan" ;
    do_VV = mkVV (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord") ;
    peppad_A = regA "peppad" ; -- "peppat" "peppade";
    CP_skadad_av_11_A = compoundA (regA "CP-skadad") ;


    bara_AdVFoc = mkAdv "bara" ;

    sadana_PronAQ = mkA "sådan" ;
    fler_PronAD   = mkA "flera" "flera" "flera" "fler" "flest" ;

    hela_Predet    = {s  = \\_,_ => "hela" ; p = [] ; a = PNoAg} ;
    samma_Predet   = {s  = \\_,_ => "samma" ; p = [] ; a = PNoAg} ;

    sjaelva_Quant = {s = \\_,_,_,_ => "själva" ;
                    sp = \\_,_,_,_ => variants {};
                     det = DDef Def } ;

    vardera_Det  = {s,sp = \\_,_ => "vardera" ; n = Sg ; det = DDef Indef};
    ena_Det      = {s,sp = \\_,_ => "ena" ; n = Sg ; det = DDef Def};
    baegge_Det   = {s,sp = \\_,_ => "bägge" ; n = Pl ; det = DDef Def} ;
    baada_Det    = {s,sp = \\_,_ => "båda" ; n = Pl ; det = DDef Def} ;
    varannan_Det = {s,sp = \\_,_ => "varannan" ; n = Sg ; det = DDef Indef} ;
    somliga_Det  = {s,sp = \\_,_ => "somliga" ; n = Pl ; det = DDef Indef} ;
    dylika_Det   = {s,sp = \\_,_ => "dylika" ; n = Pl ; det = DDef Indef} ;
    oovriga_Det  = {s,sp = \\_,_ => "övriga" ; n = Pl ; det = DDef Indef} ;
    samtliga_Det = {s,sp = \\_,_ => "samtliga" ; n = Pl ; det = DDef Indef} ;
    aatskilliga_Det = {s,sp = \\_,_ => "åtskilliga" ; n = Pl ; det = DDef Indef} ;
    varenda_Det     = {s  = \\_,_ => "varenda" ; sp = \\_,_ => "varenda en" ; 
                       n = Sg ; det = DDef Indef};


    noll_Det = {s,sp = \\_,_ => "noll" ; n = Pl ; det = DDef Indef};
} ;
    -}
