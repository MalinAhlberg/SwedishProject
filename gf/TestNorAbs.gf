--# -path=./gf:.:norwegian:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract TestNorAbs = 
  Lexicon,
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
   } ;

