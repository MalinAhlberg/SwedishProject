--# -path=:.:prelude:alltenses:abstract:common
abstract PosGFAbs = Cat ** {

 cat Meta ;
 fun 
    XConj : Int -> Conj ;
    
    XAdV : Int -> AdV ; --kan vara Adv också
    XIAdv : Int -> IAdv ;
    XAdA  : Int -> AdA ;
    XCAdv : Int -> CAdv ;
    
  --_ABRA">Adverb, relative</value> -- när, där ... :( Subj, IdRP.

-- +s in Gen
    --XAgen : Int -> A ;
    XA    : Int -> A ;
    --XAcomp : Int -> A ;
    --XAgcompen : Int -> A ;
    --XAsup : Int -> A ;

-- +s in Gen
--    XNgen : Int -> N ;
    XN : Int -> N ;
--    XNgendef : Int -> N ;
--    XNdef : Int -> N ;
--    XNdef : Int -> N ;

    XPunkt : Int -> Text ;

    XMeta : Int -> Meta ;

-- +s in Gen
   XPN : Int -> PN ; -- this could be used to compare our ne

   x_Quant : Int -> Quant ; -- here we should also try to find out the number afterwards
   XQuantDef : Int -> Quant ;
   XPredet : Int -> Quant ;

   XPrep : Int -> Prep ; 

   XDigit : Int -> Int ;
   XOrd   : Int -> Ord ;

   XVPtPret : Int -> V ; -- do not know the valency
   XVpass : Int -> V ; 
   XV : Int -> V ;
   XVimp : Int -> V ;
   XVinf : Int -> V ;
   XVinfPass : Int -> V ;
   XVpres : Int -> V ;
   XVpresPass : Int -> V ;
   XVpret : Int -> V ;
   XVpretPass : Int -> V ;
   XVSup : Int -> V ; 
   XVSupPass : Int -> V ; 
}
