--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe ,
                                   ParadigmsSwe - [nominative] **
 open CommonScand, ResSwe, ParamX, VerbSwe, Prelude, DiffSwe, StructuralSwe, MorphoSwe,
      NounSwe, Coordination in {

lincat
 ReflNP  = NP ;
--  ReflGenPron = {s : Agr => Str} ;

lin
  FocVP vp np = {
      s = \\t,a,p =>
        let
          subj = np.s ! CommonScand.nominative ;
          agr  = np.a ;
          vps  = vp.s ! VPFinite t a ;  -- Fut Simul -- t a
          vf = case <<t,a> : ParamX.Tense * Anteriority> of {
            <Pres,Simul> => vps.fin;
            <Past,Simul> => vps.fin;
            <_   ,Simul> => vps.inf;
            <Pres,Anter> => vps.inf;
            <Past,Anter> => vps.inf;
            <_   ,Anter> => (vp.s ! VPFinite Past Anter).inf
            };
          verb = mkClause subj agr (predV do_V) ;                        
          comp = vp.n2 ! agr ++ vp.a2 ++ vp.ext     
        in
        vf ++ comp ++ (verb.s ! t ! a ! p ! Inv) ++ vp.a1 ! Pos 
      } ;

  oper do_V : V = mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord" ;

lin
  FocAP ap np    = 
  {s = \\t,a,p => 
   let vp = UseComp (CompAP ap);
       vps = vp.s ! VPFinite t a;
       npAgr = np.a in
    vp.n2 ! npAgr ++ vps.fin ++ np.s ! NPNom 
    ++ negation ! p++ vps.inf };


  FocVV vv vp np = 
  {s = \\t,a,p =>
    let vps = vp.s ! VPInfinit Simul ;
        vvp = UseV vv ;
        vvs = vvp.s ! VPFinite t a ; 
        always = vp.a1 ! Pos ++ vvp.a1 ! Pos ;
        already = vp.a2 ++ vvp.a2 in
   vps.inf ++ vp.n2 ! np.a ++ vvs.fin ++ np.s ! NPNom 
   ++ vv.c2.s ++ always ++ negation ! p ++ already ++ vvs.inf
   };  


lin
  PrepCN prep cn = {s = prep.s ++ cn.s ! Sg ! DIndef ! Nom } ;
 
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

  dethaer_NP= this_NP "det här" Neutr Sg;
  detdaer_NP= this_NP "det där" Neutr Sg;
  dedaer8utr_NP= this_NP "de där" Utr Pl ;
  dedaer8neut_NP= this_NP "de där" Neutr Pl ;
  denhaer_NP= this_NP "den här" Utr Sg;
  dendaer_NP= this_NP "den där" Utr Sg ;


  it8utr_Pron = MorphoSwe.regNP "den" "dess" Utr   Sg  ;
  
  this8denna_Quant = 
    {s,sp = table {
      Sg => \\_,_ => genderForms ["denna"] ["detta"] ; 
      Pl => \\_,_,_ => ["dessa"]
      } ;
    det = DDef Indef
    } 
    ;

  DropAttVV vv =  {s = vv.s ; part = vv.part ; vtype = vv.vtype ; c2 = mkComplement [] ; lock_VV = <>} ;

  SupCl np vp pol = let subj = np.s ! nominative ;
                        verb = (vp.s ! VPFinite Pres Anter).inf ;
                        neg  = vp.a1 ! pol.p ;
                        compl = vp.n2 ! np.a ++ vp.a2 ++ vp.ext in
    {s = \\_ => subj ++ neg ++ verb ++ compl };
    

  PassV2 v2 = predV (depV v2);
  PassV2Be v = insertObj 
        (\\a => v.s ! VI (VPtPret (agrAdjNP a DIndef) Nom)) 
        (predV verbBecome) ;

  UseComparA a = {
      s = \\ap => case a.isComp of {
        True => compMore ++ a.s ! AF (APosit ap) Nom ;
        _    => a.s ! AF ACompar Nom
        } ;
      isPre = True
      } ;


 
-- does not allow you to say "kattens som bor här"
  RelNP' np vp tmp pol =
    let cl = mkClause (np.s ! nominative ++ "som") np.a vp in 
      {s = \\_ => cl.s ! tmp.t ! tmp.a ! pol.p ! Sub ;
       a = np.a} ;

  -- not adV, but for normal advers, 'han åt redan äpplet'
  AdvVPSlash vp adv = insertAdV adv.s vp ** {n3 = vp.n2;
                                             c2 = vp.c2} ;


  ReflCN num cn = 
        let g = cn.g ;
            m = cn.isMod ;
            dd = DDef Indef ;
      in lin NP {
      s = \\c => cn.s ! num.n ! dd ! caseNP c ++ num.s ! g ; 
      a = agrP3 (ngen2gen g) num.n -- ?
      } ;

ReflSlash vp np = let vp_l = lin VPSlash vp ;
                      np_l = lin NP np   in
    lin VP (insertObj (\\a => vp.c2.s ++ reflForm a np.a ++ np.s ! NPNom) vp) ; 




 oper reflForm : Agr -> Agr -> Str = \aSub,aObj   ->
    case <aSub.p,aObj.g,aObj.n> of {
     <P3,Neutr,Sg> => "sitt" ;
     <P3,Utr ,Sg>  => "sin" ;
     <P3,_   ,Pl>  => "sina" ;
     _             => reflGenPron aSub.p aSub.n aObj.n aObj.g};
  
  oper reflGenPron : Person -> (subnum,objnum : ParadigmsSwe.Number)
                     -> NGender -> Str =
   \p,subnum,objnum,g -> let pn = getPronoun p subnum
      in pn.s ! NPPoss (gennum g Pl) Nom ;



      
  this_NP : Str -> Gender -> Number -> NP =
  \denna,g,n -> lin NP {s = table {NPPoss gn c => denna+"s";
                                   _           => denna};
                           a = agrP3 g n};

  getPronoun : Person -> ParadigmsSwe.Number -> Pron = 
   \p,n ->  case <p,n> of {
      <P1,Sg>   => i_Pron ;
      <P2,Sg>   => youSg_Pron ;
      <P3,Sg>   => he_Pron ;
      <P1,Pl>   => we_Pron ;
      <P2,Pl>   => youPl_Pron ;
      <P3,Pl>   => they_Pron } ;


}

