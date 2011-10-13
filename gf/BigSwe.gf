--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

concrete BigSwe of Big = 
  NounSwe,
  VerbSwe -[PassV2,ComplSlash,ReflVP],
  AdjectiveSwe -[UseComparA],
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, 
  QuestionSwe,
  RelativeSwe,
  ConjunctionSwe,
  PhraseSwe, 
  TextX,
  TenseX,
  StructuralSwe, 
  ExtraSwe,
  LexiconExtSwe
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
    johan_PN = regPN "johan" ;
    do_VV = mkVV (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord") ;
    sadana_X = mkA "sådan" ; 
} ;
