--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian

abstract Big = 
  LexiconExt,
  ExtraSweAbs,
  Noun ,
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
  Text,
  Tense
  ** {
  flags startcat=Phr ;
  
  fun
  begin_VV : VV ;
  tankaTill_V : V ;
  become_V2 : V2 ;
  numberOf_N2 : N2 ;
  gallande_A : A ;
  johan_PN : PN ;
  do_VV : VV;
  sadana_X : X ;
  } ;
