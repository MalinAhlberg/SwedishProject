--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe - [GenNP, FocAdv] ,
                                   ParadigmsSwe - [nominative] **
 open CommonScand, ResSwe, ParamX, VerbSwe, Prelude, DiffSwe, StructuralSwe, MorphoSwe,
      NounSwe, Coordination, AdjectiveSwe in {

lincat
 ReflNP  = NP ;
 PronAQ = A ; -- 'en sådan' 
 PronAD = A ; -- 'fler' 
 AdvFoc = Adv ;
 
 

lin

 
  -- maybe not the best way, but the adverb should always
  -- be befor the finite verb
  -- needs changes in VP, fix
  AdvFocVP adv vp = {s = \\vpf => {fin = adv.s ++ (vp.s ! vpf).fin ;
                                 inf = (vp.s ! vpf).inf};
                   a1 = vp.a1 ; n2 = vp.n2 ; a2 = vp.a2 ; ext = vp.ext ;
                   en2 = vp.en2 ; ea2 = vp.ea2; eext = vp.eext } ;
  PredetAdvF adv = {s = \\_,_ => adv.s ; p = [] ; a = PNoAg} ;
  
  QuantPronAQ x =  
   let utr = x.s ! AF (APosit (Strong (GSg Utr))) Nom ;
       ntr = x.s ! AF (APosit (Strong (GSg Neutr))) Nom ;
       pl  =  x.s ! AF (APosit (Strong GPl)) Nom 
   in
   {s =
     table {Sg => \\_,_ => genderForms ("en"++utr) 
                                       ("ett"++ntr) ;
            Pl => \\_,_,_ => pl} ;
   sp = table {Sg => \\_,_ => genderForms utr ntr;
               Pl => \\_,_,_ => pl};
     det = DDef Indef};

 -- those cannot be compared 
  CompPronAQ x = CompAP (PositA (lin A x)) ; 

  DetPronAD x = lin Det {s,sp = \\_,_ => x.s ! AF (APosit (Strong GPl)) Nom ;
            n = Pl ; det = DDef Indef} ;

  CompPronAD x = CompAP (PositA (lin A x)) ; 

  -- de blev sådana
 ComplVAPronAQ v ap = insertObj (\\a => (PositA ap).s ! agrAdjNP a DIndef) (predV v) ;
 -- de blev fler
 ComplVAPronAD v ap = insertObj (\\a => (UseComparA ap).s ! agrAdjNP a DIndef) (predV v) ;

 ComplSlash vp np = 
       insertObjPost
         (\\_ => vp.c2.s ++ np.s ! accusative ++ vp.n3 ! np.a) vp ;

 ReflVP vp = insertObjPost (\\a => vp.c2.s ++ reflPron a ++ vp.n3 ! a) vp ;

lin
  FocVP vp np = {
      s = \\t,a,p =>
        let
          subj = np.s ! CommonScand.nominative ;
          agr  = np.a ;
          vps  = vp.s ! VPFinite t a ;  
          vf = case <<t,a> : ParamX.Tense * Anteriority> of {
            <Pres,Simul> => vps.fin;
            <Past,Simul> => vps.fin;
            <_    ,Simul> => vps.inf;
            <Pres,Anter> => vps.inf;
            <Past,Anter> => vps.inf;
            <_    ,Anter> => (vp.s ! VPFinite Past Anter).inf
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
   let vp = UseComp ap ; --(CompAP ap);
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

  SupCl np vp pol = let sub = np.s ! nominative ;
                        verb = (vp.s ! VPFinite Pres Anter).inf ;
                        neg  = vp.a1 ! pol.p ++ pol.s ;
                        compl = vp.n2 ! np.a ++ vp.a2 ++ vp.ext in
    {s = \\_ => sub ++ neg ++ verb ++ compl };
    

  PassV2 v2 = predV (depV v2);

  PassV2Be v = insertObj 
        (\\a => v.s ! VI (VPtPret (agrAdjNP a DIndef) Nom)) 
        (predV verbBecome) ;

   
 
  -- not adV, but for normal advers, 'han åt redan äpplet'
  AdvVPSlash vp adv = insertAdV adv.s vp ** {n3 = vp.n2;
                                             c2 = vp.c2} ;

  AdvComp comp adv = {s = \\agr => adv.s ++ comp.s ! agr} ;
 
  -- be callled PPartAP?
  PPartAP v2 =
    {s     = \\aform => v2.s ! VI (VPtPret aform Nom);
     isPre = True} ; 

  ReflCN num cn = 
        let g = cn.g ;
            m = cn.isMod ;
            dd = DDef Indef ;
      in lin NP {
      s = \\c => cn.s ! num.n ! dd ! caseNP c ++ num.s ! g ; 
      a = agrP3 (ngen2gen g) num.n -- ?
      } ;

  ReflSlash vp np = let vp_l = lin VPSlash vp ;
                        np_l = lin NP np      ;
                        obj  = vp.n3 ! np.a   in
    lin VP (insertObjPost (\\a => vp.c2.s ++ reflForm a np.a ++ np.s ! NPNom++obj) vp) ; 




 oper reflForm : Agr -> Agr -> Str = \aSub,aObj   ->
    case <aSub.p,aObj.g,aObj.n> of {
     <P3,Neutr,Sg> => "sitt" ;
     <P3,Utr ,Sg>  => "sin" ;
     <P3,_   ,Pl>  => "sina" ;
     _             => reflGenPron aSub.p aSub.n aObj.n aObj.g};
  
  oper reflGenPron : Person -> (subnum,objnum : ParadigmsSwe.Number)
                     -> NGender -> Str =
   \p,subnum,objnum,g -> let pn = getPronoun p subnum
      in pn.s ! NPPoss (gennum g objnum) Nom ;



      
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

