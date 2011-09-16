--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigTestSwe of BigTest = 
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
  LexiconSwe
  ** {

flags startcat = Phr ;  unlexer = text ; lexer = text ;

} ;
