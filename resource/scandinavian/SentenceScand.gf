incomplete concrete SentenceScand of Sentence = 
  CatScand ** open CommonScand, ResScand, Prelude in {

  flags optimize=all_subs ;

  lin
    PredVP np vp = mkClause (np.s ! nominative) np.a vp ;

    PredSCVP sc vp = mkClause sc.s (agrP3 Neutr Sg) vp ;

    ImpVP vp = {
      s = \\pol,n => 
        let 
          agr   = {g = Utr ; n = n ; p = P2} ;
          verb  = vp.s ! VPImperat ;
        in
        verb.fin ++ vp.a1 ! pol ++ verb.inf ++ vp.n2 ! agr ++ vp.a2 ++ vp.ext
    } ;

    SlashVP np vp = 
      mkClause 
        (np.s ! nominative) np.a 
        vp **
      {n3 = vp.n3 ; c2 = vp.c2} ;

    AdvSlash slash adv = {
      s  = \\t,a,b,o => slash.s ! t ! a ! b ! o ++ adv.s ;
      n3 = slash.n3 ;
      c2 = slash.c2
    } ;

    SlashPrep cl prep = cl ** {n3 = \\_ => [] ; c2 = {s = prep.s ; hasPrep = True}} ;

    SlashVS np vs slash = 
      mkClause
        (np.s ! nominative) np.a 
        (insertObj (\\_ => conjThat ++ slash.s ! Sub) (predV vs)) **
      {n3 = slash.n3 ; c2 = slash.c2} ;

    EmbedS  s  = {s = conjThat ++ s.s ! Sub} ;
    EmbedQS qs = {s = qs.s ! QIndir} ;
    EmbedVP vp = {s = infMark ++ infVP vp (agrP3 Utr Sg)} ; --- agr

    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! o
    } ;
    UseQCl t p cl = {
      s = \\q => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! q
    } ;
    UseRCl t p cl = {
      s = \\r,rc => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! r ! rc ;
      c = cl.c
    } ;
    UseSlash t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! o ;
      n3 = cl.n3 ;
      c2 = cl.c2
    } ;

    AdvS a s = {s = \\o => a.s ++ s.s ! Inv} ;
    ExtAdvS a s = {s = \\o => a.s ++ "," ++ s.s ! Inv} ;

    RelS s r = {s = \\o => s.s ! o ++ "," ++ r.s ! agrP3 Neutr Sg ! RPrep True } ; --- vilket

}
