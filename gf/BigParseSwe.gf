--# -path=./gf:.:../gf:swedish:prelude:alltenses:abstract:scandinavian:common

-- for testing grammar, using just the test lexicon
concrete BigParseSwe of BigParse = 
  ExtraSwe,
  NounSwe - [DetNP,PossPron] ,
  VerbSwe -[PassV2,ComplSlash,AdvVPSlash,ReflVP],
  AdjectiveSwe -[UseComparA],
  AdverbSwe,
  IdiomSwe,
  NumeralSwe,
  SentenceSwe, 
  QuestionSwe,
  RelativeSwe,
  PhraseSwe,
  TextX - [Tense,Temp,Adv] , 
  TenseSwe,
  ConjunctionSwe,
  StructuralSwe
--  TestLex
--  LexiconSwe
--  BigValLex
  ** open CommonScand, Prelude in {

flags startcat = Phr ;  unlexer = text ; lexer = text ; coding=utf8;
      optimize=values ; literal=VPCompl ;

  lincat   VPParse  = {s : Str } ;
           MetaAgr = {s : Str ; a : NPerson } ;
   --        VPCompl = String ;

  lin
  --assume that this works.. TODO
  VPMeta agr str np = {s = str.s ++ np.s ! agr.a ! accusative } ;

  Xs_PN i = {s = \\_ => "X"++i.s    ; g = (variants {utrum | neutrum})} ;
  Y_PN  i = {s = table {Gen => "Ys"++i.s ;
                        Nom => "Y" ++i.s };
             g = (variants {utrum | neutrum})} ;

  npMeta =  mkNP' "?np" ;
  
  npsubMeta = mkNP' "?npsub" ;
  npobjMeta = mkNP' "?npobj" ;
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
  conjMeta  = {s1 = ""; s2 =  "?conj" ; n = variants {Sg | Pl}} ;
  advMeta   = mkAdv "?adv" ;
  advsMeta  = mkAdv "?advs" ;  --these three are ambigouos
  iadvMeta  = {s = "?advs"} ;
  adVMeta   = {s = "?advs"} ; 
  

  oper mkNP' : Str ->  {s : NPerson => NPForm => Str ; a : Agr} = 
      \str -> {s = \\_,_ => str ; a = {n = variants {Sg | Pl} ;
                                       g = variants {utrum | neutrum} ;
                                       p = variants {P1 | P2 | P3}}} ;



} ;
