--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian

abstract BigNew = 
--  LexiconVerb --uses all verbs in LexiconVerb.gf
  LexiconExt,
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
  } ;
