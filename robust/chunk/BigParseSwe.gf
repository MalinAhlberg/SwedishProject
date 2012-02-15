--# -path=./gf:.:../../gf:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigParseSwe of BigParse = 
  ExtraSwe,
  NounSwe- [DetNP,PossPron] ,
  VerbSwe -[PassV2,ComplSlash,ReflVP],
  AdjectiveSwe -[UseComparA],
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, 
  QuestionSwe,
  RelativeSwe,
  PhraseSwe,
  TextX - [Tense,Temp,Adv] , 
  TenseSwe,
  ConjunctionSwe,
  StructuralSwe,
  TestLex
  ** 
open IrregSwe, ParamX, CommonScand in {

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;

} ;
