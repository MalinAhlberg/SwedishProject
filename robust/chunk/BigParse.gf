--# -path=./gf:.:../../gf:swedish:prelude:alltenses:abstract:scandinavian

-- for testing grammar, using just the test lexicon
abstract BigParse = 
  ExtraSweAbs, 
  Noun-[DetNP,PossPron,ApposCN], 
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
  Structural - [therefore_PConj,otherwise_PConj],  --these require subordinate word order, not for Utt
  Tense -[Adv],
  Text - [Adv],
  TestLexAbs
  **
  {
  flags startcat=Phr ;
  
  cat PhrText ;
  fun
  isUtt : Phr -> PhrText ;
  isText : Text -> PhrText ;
  annars_Adv : Adv ; --remove
  Y_PN  : Int -> PN ; 
  Xs_PN : Int ->  PN ; --same genitive as nominative
  npMeta : NP ;
  nMeta : N ;
  npsubMeta : NPTyped Subject ;
  npobjMeta : NPTyped Object ;
  vMeta : V ;
  v2 : V2 ;
  v3 : V3 ;
  va : VA ;
  vs : VS ;
  vq : VQ ;
  vv : VV ;
  v2a : V2A ;
  v2v : V2V ;
  v2s : V2S ;
  v2q : V2Q ;
  apMeta : AP ;
  vpMeta : VP ;
  icompMeta : IComp ;
  compMeta :  Comp ;
  conjMeta : Conj ;
  advMeta : Adv ;
  advsMeta : Adv ;
  iadvMeta : IAdv ;
  adVMeta : AdV ; 
  sMeta : S ;
 
  
 
  } ;

