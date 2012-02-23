--# -path=./gf:.:../gf:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
abstract BigParse = 
--  TestLexAbs,
--  Lexicon,
--  BigValLexAbs,
  ExtraSweAbs,
  Noun -[DetNP] ,
  Verb -[PassV2,AdvVPSlash,ComplSlash, ReflVP], --[PassV2,AdvVPSlash,ComplSlash, ReflVP,SlashV2VNP,Slash2V3,Slash3V3,ComplSlash,CompNP], --,ComplVA],
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
  Tense - [Adv] ,
  Text - [Adv]
  ** {
  flags startcat=Phr ; literal=VPCompl ;
  
  cat VPParse ;
      MetaAgr ;
      VPCompl ;

  fun
  VPMeta : MetaAgr -> VPCompl -> NPTyped Object -> VPParse ;
  Y_PN  : Int -> PN ; 
  Xs_PN : Int ->  PN ; --same genitive as nominative
  npMeta : NP ;
  npsubMeta : NPTyped Subject ;
  npobjMeta : NPTyped Object ;
  vMeta : V ;
  apMeta : AP ;
  vpMeta : VP ;
  icompMeta : IComp ;
  compMeta :  Comp ;
  conjMeta : Conj ;
  advMeta : Adv ;
  iadvMeta : IAdv ;
  adVMeta : AdV ; 
  {-
  vMeta   : V ;
  vvMeta  : VV ;
  vsMeta  : VS ;
  v2Meta  : V2 ;
  v3Meta  : V3 ;
  vaMeta  : VA ;
  vqMeta  : VQ ;
  v2sMeta : V2S ;
  v2qMeta : V2Q ;
  v2aMeta : V2A ;
  -}
 
  } ;

