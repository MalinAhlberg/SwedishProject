--# -path=./gf:.:../../gf:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigParseSwe of BigParse = 
  ExtraSwe,
  NounSwe- [DetNP,PossPron,ApposCN],
  VerbSwe -[PassV2,ComplSlash,ReflVP],
  AdjectiveSwe -[UseComparA],
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, 
  QuestionSwe,
  RelativeSwe,
  PhraseSwe,
  TextX - [Tense,Temp,Adv], 
  TenseSwe,
  ConjunctionSwe,
  StructuralSwe - [therefore_PConj,otherwise_PConj], --these require subordinate word order, not for Utt
  TestLex
  ** 
open CommonScand, Prelude in {

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ;

  lincat PhrText = {s : Str} ;
  lin
  isUtt s = s ;
  isText s = s ;
     

  annars_Adv = mkAdv "annars" ; --remove
  Xs_PN i = {s = \\_ => "X"++i.s    ; g = (variants {utrum | neutrum})} ;
  Y_PN  i = {s = table {Gen => "Ys"++i.s ;
                        Nom => "Y" ++i.s };
             g = (variants {utrum | neutrum})} ;

  npMeta =  mkNP' (variants { "?np" |  "?nps" }) ;
  nMeta = {s = \\_,_,_ => "?nps" ; g = variants {utrum | neutrum}} ;
  
  npsubMeta =  mkNP' (variants { "?npsub" | "?nps" }) ;
  npobjMeta = mkNP' (variants { "?npobj" | "?nps" }) ;
  vMeta  = mkV "?v" ;
  apMeta = {s = \\_,_ => "?ap" ; isPre = variants {True | False}};
  vpMeta = {s = \\_,_ => {fin = "?vp" ; inf = "?vp"} ;
            a0 = "" ;
            a1 = \\_ => "" ;
            n2 = \\_ => "" ;
            a2 = \\_ => "" ;
            ext = "" ;
            voice = Act ; en2,ea2,eext = variants {True | False}}; 

  icompMeta = {s = \\_ => "?icomp"} ;
  compMeta  = {s = \\_ => "?comp"} ;
  compsMeta = {s = \\_ => "?comps"} ;
  icompsMeta = {s = \\_ => "?icomps"} ;
  conjMeta  = {s1 = ""; s2 =  "?conj" ; n = variants {Sg | Pl}} ;
  advMeta   = mkAdv "?adv" ;
  advsMeta  = mkAdv "?advs" ;  --these three are ambigouos
  iadvMeta  = {s = "?advs"} ;
  adVMeta   = {s = "?advs"} ; 
  sMeta     = {s = \\_ => "?s" };
  {-
  v2 = mkV2 "?v2" ;
  v3 = mkV2 "?v3" ;
  va =  mkV2 "?va" ;
  vs =  mkVS (mkV "?vs") ;
  vq =  mkVQ (mkV "?vq") ;
  vv =  mkVV (mkV "?vv") ;
  v2a =  mkVA "?v2a" ;
  v2v =  mkVV "?v2v" ;
  v2s =  mkVS "?v2s" ;
  v2q =  mkVQ "?v2q" ;
 
 -}

  oper mkNP' : Str ->  {s : NPerson => NPForm => Str ; a : Agr} = 
      \str -> {s = \\_,_ => str ; a = {n = variants {Sg | Pl} ;
                                       g = variants {utrum | neutrum} ;
                                       p = variants {P1 | P2 | P3}}} ;


} ;
