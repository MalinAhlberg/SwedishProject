--# -path=./gf:.:norwegian:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete TestNor of TestNorAbs = 
  NounNor, 
  VerbNor,
  AdjectiveNor,
  AdverbNor,
  IdiomNor,
  NumeralNor,
  SentenceNor,
  QuestionNor,
  RelativeNor,
  ConjunctionNor,
  PhraseNor,
  TenseX,
  StructuralNor,
  LexiconNor
  ** open IrregNor, ParamX, CommonScand in {
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;
} ;
