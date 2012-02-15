--# -path=./gf:.:../gf:swedish:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract BigParse = 
  ExtraSweAbs, 
  Noun-[DetNP,PossPron], 
  Verb -[PassV2,AdvVPSlash,ComplSlash, ReflVP],
  Adjective -[UseComparA],
  Adverb,
  Idiom,
  Numeral,
  Sentence,
  Question,
  Relative,
  Conjunction,
  Phrase,
  Structural,
  Tense -[Adv],
  Text - [Adv],
  TestLexAbs
  **
  {
  flags startcat=Phr ;
  
  } ;

