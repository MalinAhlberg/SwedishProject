--# -path=:.:prelude:alltenses:abstract:common
abstract PosGFAbs = Cat ** {

 cat Meta ;
 fun 
    XConj : Digits -> Conj ;
    
    XAdV : Digits -> AdV ; --kan vara Adv också
    XIAdv : Digits -> IAdv ;
    XAdA  : Digits -> AdA ;
    XCAdv : Digits -> CAdv ;
    
  --_ABRA">Adverb, relative</value> -- när, där ... :( Subj, IdRP.

    --XAgen : Digits -> A ;
    XA    : Digits -> A ;
    --XAcomp : Digits -> A ;
    --XAgcompen : Digits -> A ;
    --XAsup : Digits -> A ;

    XNgen : Digits -> N ;
    XN : Digits -> N ;
    XNgendef : Digits -> N ;
    XNdef : Digits -> N ;
    XNdef : Digits -> N ;

    XPunkt : Digits -> Text ;

    XMeta : Digits -> Meta ;

   XPN : Digits -> PN ; -- this could be used to compare our ne
   XPNgen : Digits -> PN ;

   XQuant : Digits -> Quant ; -- here we should also try to find out the number afterwards
   XQuantDef : Digits -> Quant ;

   XPrep : Digits -> Prep ; 

   XDigit : Digits -> Digits ;
   XOrd   : Digits -> Ord ;

   XVPtPret : Digits -> V ; -- do not know the valency
   XVpass : Digits -> V ; 
   XV : Digits -> V ;
   XVimp : Digits -> V ;
   XVinf : Digits -> V ;
   XVinfPass : Digits -> V ;
   XVpres : Digits -> V ;
   XVpresPass : Digits -> V ;
   XVpret : Digits -> V ;
   XVpretPass : Digits -> V ;
   XVSup : Digits -> V ; 
   XVSupPass : Digits -> V ; 
}
