--# -path=./gf:.:common:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract Test = 
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
  -- ExtraSweAbs,
  Tense
  ** {
  flags startcat=Phr ;
   } ;

