--# -path=:.:prelude:alltenses:swedish:common:scandinavian:abstract
concrete PosGFCnc of PosGFAbs = CatSwe ** open  ResSwe, CommonSwe, NumeralSwe in {

 lincat Meta = {s : Str} ;

 lin 
    XConj d = {s1,s2 = "XConj"++ str d  ; n = Sg } ;
    XAdV d = {s = "XAdV"++str d} ;
    XIAdv d = {s = "XIAdv"++str d} ;
    XAdA  d = {s = "XAdA"++str d}  ;
    XCAdv d = {s = "XCAdv"++str d; p = []}  ;
    
  --_ABRA">Adverb, relative</value> -- när, där ... =( Subj, IdRP.


--- START here, use regA?
    mkA : (liten,litet,lilla,sma,mindre,minst,minsta : Str) -> A -- worst case
    XA d = mkA "XA" "XA" "XA" "XAsup" "XAsup" 
    
    
    {s = table { (AF (ASuperl _) c) => "XAsup"+strCase c ++str d  ;
                        (AF ACompar     c) => "XAcomp"+strCase c++str d ;
                        (AF (APosit _)  c) => "XA"+strCase c++str d }};
      oper strCase : Case -> Str = \c -> case c of {Nom => ""; Gen => "gen"} ;

{-
lin
-- {s : Number => Species => Case => Str ; g : NGender}
    XNgen d = {s = \\ }  ;
    XN d = N ;
    XNgendef d = N ;
    XNdef d = N ;
    XNdef d = N ;
    -}

   oper str : {s : CardOrd => Str ; n : Number}-> Str = \d ->  d.s ! NCard utrum ;
   {- 
    XPunkt d = Text ;

    XMeta d = Meta ;

   XPN d = PN ; -- this could be used to compare our ne
   XPNgen d = PN ;

   XQuant d = Quant ; -- here we should also try to find out the number afterwards
   XQuantDef d = Quant ;

   XPrep d = Prep ; 

   XDigit d = Digits ;
   XOrd   d = Ord ;

   XVPtPret d = V ; -- do not know the valency
   XVpass d = V ; 
   XV d = V ;
   XVimp d = V ;
   XVinf d = V ;
   XVinfPass d = V ;
   XVpres d = V ;
   XVpresPass d = V ;
   XVpret d = V ;
   XVpretPass d = V ;
   XVSup d = V ; 
   XVSupPass d = V ; 
   -}
}
