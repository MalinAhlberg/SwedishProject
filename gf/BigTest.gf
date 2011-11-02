--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract BigTest = 
  Lexicon,
  ExtraSweAbs,
  Noun, -- -[DetNP] ,
  Verb -[PassV2,AdvVPSlash,ComplSlash, ReflVP], --,ComplVA],
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
  peppad_A : A ;

  ---
  bara_AdVFoc : AdvFoc ;

  sadana_PronAQ : PronAQ ;
  fler_PronAD : PronAD ;
  -- overgenerating: alla hela katter. should not be ok.
  -- predets should be able to decide definites?
  hela_Predet : Predet ;  --hela horder/hela katten  -- both
  sjaelva_Quant : Quant ; -- själva kungen/själva öronen -- def
  samma_Predet : Predet ; -- samma katter/samma öra 
  varenda_Det : Det ;
  vardera_Det : Det ;
  ena_Det : Det ;
  baegge_Det : Det ;
  baada_Det : Det ;
  varannan_Det : Det ;
  somliga_Det : Det ;
  dylika_Det : Det ;
  oovriga_Det : Det ;
  aatskilliga_Det : Det ;
  samtliga_Det : Det ;

-- remove 'noll'?
  noll_Det : Det ;
  } ;

