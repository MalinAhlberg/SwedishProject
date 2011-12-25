--# -path=./gf:.:danish:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract TestDanAbs = 
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
  ExtraDanAbs - [TFutKommer],
  Tense - [SFutKommer]
  ** {
  flags startcat=Phr ;
   } ;

