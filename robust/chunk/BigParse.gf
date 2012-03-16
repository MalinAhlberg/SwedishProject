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
  npMeta : String -> NP ;
  cnMeta : String -> CN ;
  nMeta :String ->  N ;
  npsubMeta : String -> NPTyped Subject ;
  npobjMeta : String -> NPTyped Object ;
  vMeta : String -> V ;
  v2 : String -> V2 ;
  v3 : String -> V3 ;
  va : String -> VA ;
  vs : String -> VS ;
  vq : String -> VQ ;
  vv : String -> VV ;
  v2a : String -> V2A ;
  v2v : String -> V2V ;
  v2s : String -> V2S ;
  v2q : String -> V2Q ;
  apMeta : String -> AP ;
  vpMeta : String -> VP ;
  icompMeta : String -> IComp ;
  compMeta :  String -> Comp ;
  conjMeta : String -> Conj ;
  advMeta : String -> Adv ;
  advsMeta : String -> Adv ;
  iadvMeta : String -> IAdv ;
  adVMeta : String -> AdV ; 
  sMeta : String -> S ;
  phrTextMeta : String -> PhrText ;
 
  
 
  } ;

