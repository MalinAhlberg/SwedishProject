--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian

abstract Big = 
  DictSweAbs,
  ExtraSweAbs,
  Noun -[DetNP] ,
  Verb -[PassV2,AdvVPSlash,ComplSlash, ReflVP,SlashV2VNP,Slash2V3,Slash3V3,ComplSlash,CompNP], 
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
  } ;
