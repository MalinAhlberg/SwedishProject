--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract BigTest = 
  Lexicon,
  ExtraSweAbs,
  Noun,
  Verb,
  Adjective,
  Adverb,
  Idiom,
  Numeral,
  Sentence,
  Question,
  Relative,
  Conjunction,
  Phrase,
  Structural, 
  Text,
  Tense
  ** {
  flags startcat=Phr ;
  
  fun
  begin_VV : VV ;
  } ;
