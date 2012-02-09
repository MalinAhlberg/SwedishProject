--# -path=:.:prelude:alltenses:swedish:common:scandinavian:abstract
concrete PosGFCnc of PosGFAbs = CatSwe  ** open  Prelude, ResSwe, CommonSwe, ParadigmsSwe in {

 lincat Meta = {s : Str} ;

 lin 
 {-
    XConj d = {s1,s2 = "XConj"++ intToStr d  ; n = Sg } ;
    XAdV d = {s = "XAdV"++intToStr d} ;
    XIAdv d = {s = "XIAdv"++intToStr d} ;
    XAdA  d = {s = "XAdA"++intToStr d}  ;
    XCAdv d = {s = "XCAdv"++intToStr d; p = []}  ;
    -}
    
  --_ABRA">Adverb, relative</value> -- när, där ... =( Subj, IdRP.


    XA d = mkA "XA" "XA" "XA" "XA" 
               "XAcomp" "XAsup" "XASup" ; 
      {-mkXA d;
    XA d = mkA ("XA"++ str d) ("XA"+str d) ("XA"++str d)  ("XA"++str d) 
               ("XAcomp"++str d) ("XAsup" ++str d)("XASup"++str d) ; 
        oper mkXA : {s : CardOrd => Str ; n : Number} -> {s : CS.AForm => Str; isComp : Bool} ;
             mkXA d =
             {s = table { (CS.AF (CS.ASuperl _) c) => "XAsup"+strCase c ++str d  ;
                        (CS.AF CS.ACompar     c) => "XAcomp"+strCase c++str d ;
                        (CS.AF (CS.APosit _)  c) => "XA"+strCase c++str d };
                isComp = True} ;
      oper strCase : CS.Case -> Str = \c -> case c of {CS.Nom => ""; CS.Gen => "gen"} ;
      -}


--    XA d = mkA ("XA"++str d) ("XA"++str d)  ("XA"++str d)  ("XA"++str d)
--               ("XAcomp"++str d) ("XAsup" ++str d)("XASup"++str d) ; 
   
-- {s : Number => Species => Case => Str ; g : NGender}
    XN d = mkN "XN" "XNdef" "XN" "XNdef" ;
--    XN d = mkN ("XN"++str d) ("XNdef"++str d) ("XN"++str d) ("XNdef"++str d) ;

    --XPunkt d = {s = "XPunkt"++ intToStr d} ;

    XMeta d =  {s = "XMeta"++ d.s} ;

    XPN d = mkPN "XPN" ; 
   -- {s = table {Nom => "XPN" ; Gen => "XPNgen"}}; -- this could be used to compare our ne

    x_Quant d = {s,sp = \\_,_,_,_,_ => "XQuant" ;
              det = CommonSwe.DIndef} ; -- | DDef Indef}};
--
--   XQuantDef d = {s,sp = \\_,_,_,_,_ => "XQuant" ;
--                  det = DDef Def };

   --oper intToStr : Int -> Str = "" ;-- \i -> i.s ; --{s : CardOrd => Str ; n : Number}-> Str = \d ->  d.s ! NCard utrum ;
   {- 




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
