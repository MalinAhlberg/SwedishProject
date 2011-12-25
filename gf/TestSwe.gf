--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete TestSwe of Test = 
  NounSwe, 
  VerbSwe,
  AdjectiveSwe,
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe,
  QuestionSwe,
  RelativeSwe,
  ConjunctionSwe,
  PhraseSwe,
  TextX - [Tense,Temp] ,
  TenseScand,
  StructuralSwe,
--  -- ExtraSwe,
  LexiconSwe
  ** open IrregSwe, ParamX, CommonScand in {
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;
} ;
