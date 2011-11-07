--# -path=./gf:.:danish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete TestDan of TestDanAbs = 
  NounDan, 
  VerbDan,
  AdjectiveDan,
  AdverbDan,
  IdiomDan,
  NumeralDan,
  SentenceDan,
  QuestionDan,
  RelativeDan,
  ConjunctionDan,
  PhraseDan,
  TextX - [Tense,Temp] ,
  TenseScand,
  StructuralDan,
  LexiconDan
  ** open IrregDan, ParamX, CommonScand in {
      -- Irreg needed for göra

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;
} ;
