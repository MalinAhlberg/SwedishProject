concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe ** open CommonScand, ResSwe, ParamX, Prelude in {

lin
    FocVP vp np = {
      s = \\t,a,p =>
        let
          subj = np.s ! nominative ;
          agr  = np.a ;
          vps  = vp.s ! VPFinite t a ;
          verb = case <<t,a> : ParamX.Tense * Anteriority> of {
            <Pres,Simul> => {fin = "g�r"    ; inf = vps.inf} 
            ; --# notpresent
            <Past,Simul> => {fin = "gjorde" ; inf = vps.inf} ; --# notpresent
            _ => vps --# notpresent
            } ;
          vfin = verb.fin ;
          vinf = verb.inf ;
          neg  = vp.a1 ! p ;
          comp = vp.n2 ! agr ++ vp.a2 ++ vp.ext
        in
        vinf ++ comp ++ vfin ++ subj ++ neg
      } ;

lin

  CompoundNomN a b = {
    s = \\n,d,c => a.s ! Sg ! Indef ! Nom ++ BIND ++ b.s ! n ! d ! c ;
    g = b.g
    } ;

  CompoundGenN a b = {
    s = \\n,d,c => a.s ! Sg ! Indef ! Gen ++ BIND ++ b.s ! n ! d ! c ;
    g = b.g
    } ;

  CompoundAdjN a b = {
    s = \\n,d,c => a.s ! AF (APosit (Strong (GSg Utr))) Nom ++ BIND ++ b.s ! n ! d ! c ;
    g = b.g
    } ;

}
