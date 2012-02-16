--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
abstract BigTest = 
  Lexicon,
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
  Text - [Adv]
  ** {
  flags startcat=Phr ;
  
  fun
  begin_VV : VV ;
  tankaTill_V : V ;
  become_V2 : V2 ;
  numberOf_N2 : N2 ;
  gallande_A : A ;
  johan_PN : PN ;
  do_VV : VV ;
  faa_VV : VV ;
  gaa_VV : VV ;

  } ;

