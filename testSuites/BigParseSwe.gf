--# -path=./gf:.:../gf:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigParseSwe of BigParse = 
  NounSwe- [DetNP,ApposCN],
  VerbSwe -[PassV2,ComplSlash,ReflVP,ComplNP], --,ComplVA], -- [ComplVS],
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
  ExtraSwe - [PredVPS] ,
  TestLex
  ** open IrregSwe, ParamX, CommonScand in {
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;

  lin
   
    begin_VV    = mkVV (mkV "börja") ;
    tankaTill_V = partV (mk2V "tänka" "tänkte") "till" ;
    become_V2 = mkV2 (mkV "bli" "blir""bli" "blev" "blivit" "bliven") ;
    numberOf_N2 = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep ;
    gallande_A  = compoundA (mkA "gällande" "gällande" "gällande" "gällande" "gällande");

    johan_PN = regPN "johan" ;
    do_VV = mkVV (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord") ;

    uppskatta_V2 = dirV2 (mkV "uppskattar") ;

} ;
