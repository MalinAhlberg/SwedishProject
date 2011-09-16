--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

concrete BigNewSwe of BigNew = 
  NounSwe,
  VerbSwe - [ComplVS],
  AdjectiveSwe,
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, --[UseCl, UseRCl], 
  QuestionSwe,
  RelativeSwe - [IdRP, RelSlash],
  ConjunctionSwe,
  PhraseSwe - [UttImpSg, UttImpPl],
  TextX,
  TenseX,
  StructuralSwe, 
  ExtraSwe,
  LexiconExtSwe
--  LexiconVerbSwe    --uses all verbs in LexiconVerb.gf
  ** {

flags startcat = Phr ;  unlexer = text ; lexer = text ;

} ;
