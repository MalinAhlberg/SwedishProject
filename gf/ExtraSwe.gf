--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe - [FocAdv] ,
                                   ParadigmsSwe - [nominative] **
 open CommonScand, ResSwe, ParamX, VerbSwe, Prelude, DiffSwe, StructuralSwe, MorphoSwe,
      NounSwe, Coordination, AdjectiveSwe, SentenceSwe, RelativeSwe in {

lincat
 ReflNP  = NP ;
 PronAQ = A ; -- 'en sådan' 
 PronAD = A ; -- 'fler' 
 AdvFoc = Adv ** {x : Str} ;  -- x dummy field to avoid metas
 RelVSCl = {s : Agr => RCase => Str};
 N2P = CN ** {c2 : Complement ; det : DetSpecies ; num : Number} ; 
 N2' = N2 ** {det : DetSpecies ; num : Number} ;


lin

  --test
  --ApposNP np1 np2 = {s = \\f => np1.s ! NPNom ++ np2.s ! f ; a = np2.a } ;

  ComplN2P det n2 cn = 
     let npdet = DetCN det n2 in 
     {
      s = \\nform => npdet.s ! nform ++ n2.c2.s ++ cn.s ! n2.num ! n2.det ! Nom ;
      a = {g = cn.g ; n = n2.num ; p = P3 } 
      } ;

  AdjN2 ap n2 =  let g = n2.g in 
   lin CN {
      s = \\n,d,c =>
            preOrPost ap.isPre 
             (ap.s ! agrAdj (gennum (ngen2gen g) n) d) 
             (n2.s ! n ! d ! c) ;
      g = g ;
      isMod = True ;
      c2 = n2.c2 ;
      det = n2.det ;
      num = n2.num 
      } ;

   UseN2P n2 = lin CN {
      s = \\n,d,c => n2.s ! n ! specDet d ! c ; 
           ---- part app wo c shows editor bug. AR 8/7/2007
      g = n2.g } ** {
      isMod = False ;
      c2 = n2.c2 ;
      det = n2.det ;
      num = n2.num 
      } ;

   N2N noun = {
      s = \\n,d,c => noun.s ! n ! d ! c ; 
      g = noun.g ;
      isMod = False
      } ;

 
  --

  VS_it vs = insertObj (\\_ => it_Pron.s ! NPNom ) (predV vs) ; 
  VV_it vs = insertObj (\\_ => vs.c2.s ++ it_Pron.s ! NPNom ) (predV vs) ; 

   SelfAdV   = mkAdV "själv" ; 
   SelfNP np = {s = \\f => np.s ! f ++ sjaelv ! np.a ;
                a = np.a } ;
     oper sjaelv : Agr => Str =
      \\a => case <a.g,a.n> of {
                  <_  ,Pl > => "själva" ;
                  <Neutr,_> => "självt" ;
                  _         => "själv"  };


 lin
    GenCN np num cn = let n = num.n in {
      s = \\nf => np.s ! NPPoss (gennum (ngen2gen cn.g) n) Nom 
                  ++ num.s ! cn.g 
                  ++ cn.s ! n ! DDef Indef ! (caseNP nf)  ; 
      a = agrP3 (ngen2gen cn.g) n 
      } ;

  
  PredGen sub gen = PredVP sub (UseComp (mkComp gen sub.a)) ;
   oper mkComp : NP -> Agr -> Comp = 
    \gen,agr -> lin Comp {s = \\_ => gen.s ! (NPPoss (gennumAgr agr) Nom)} ; 

lin 
  DetNP_utr = detNP utrum Sg ;

  oper detNP : NGender -> Number -> Det -> NP  =
   \g,num,det -> let 
          m = True ;  ---- is this needed for other than Art?
      in lin NP {
        s = \\c => det.sp ! m ! g;
        a = agrP3 (ngen2gen g) num
      } ;



 lin
  
  VarandraVP vp = insertObj (\\a => vp.c2.s ++ "varandra" ++ vp.n3 ! agrP3 Neutr Pl) vp  ;
  SlashV3Varandra v3 = Slash3V3 v3 varandra ;
    oper varandra : NP = lin NP {s = \\_ => "varandra"; a = agrP3 Neutr Pl} ;
                           
 lin
  RelVS s rvs = {s = \\o => s.s ! o ++ "," ++ rvs.s ! agrP3 Neutr Sg ! RPrep True} ; 
  RelSlashVS t p np vs = let cl     = PredVP np (predV vs) ; 
                             vilket = IdRP.s ! Neutr ! Sg ! (RPrep True) in
    {s = \\ag,rc => t.s ++ p.s ++ vilket ++ cl.s ! t.t ! t.a ! p.p ! Sub } ;

  TFutKommer = {s = []} ** {t = SFutKommer} ;   --# notpresent
 
  -- maybe not the best way, but the adverb should always
  -- be before the finite verb
  -- needs changes in VP, fix
  -- problematic, try parse "sova ville han inte"
  AdvFocVP  = insertAdvFoc ;
  
  oper insertAdvFoc : AdvFoc -> VP -> VP 
   = \adv, vp -> lin VP {
      s = \\vpf => {fin = (vp.s ! vpf).fin ;
                    inf = (vp.s ! vpf).inf};
      a0 = adv.s ;
      a1 = vp.a1 ; n2 = vp.n2 ; a2 = vp.a2 ; ext = vp.ext ;
      en2 = vp.en2 ; ea2 = vp.ea2; eext = vp.eext; act = vp.act } ;

 lin
  PredetAdvF adv = {s = \\_,_ => adv.s ; p = [] ; a = PNoAg} ;
  AdvFocAdv adv = {s = adv.s} ;

  
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
          vps  = vp.s ! VPFinite t a vp.act ;  
          vf = case <<t,a> : STense * Anteriority> of {
            <SPres,Simul> => vps.fin;
            <SPast,Simul> => vps.fin;
            <_    ,Simul> => vps.inf;
            <SPres,Anter> => vps.inf;
            <SPast,Anter> => vps.inf;
            <_    ,Anter> => (vp.s ! VPFinite SPast Anter vp.act ).inf
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
       vps = vp.s ! VPFinite t a vp.act ;
       npAgr = np.a in
    vp.n2 ! npAgr ++ vps.fin ++ np.s ! NPNom 
    ++ negation ! p++ vps.inf };


  FocVV vv vp np = 
  {s = \\t,a,p =>
    let vps = vp.s ! VPInfinit Simul vp.act;
        vvp = UseV vv ;
        vvs = vvp.s ! VPFinite t a vp.act ; 
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
                        verb = (vp.s ! VPFinite SPres Anter vp.act).inf ;
                        neg  = vp.a1 ! pol.p ++ pol.s ;
                        compl = vp.n2 ! np.a ++ vp.a2 ++ vp.ext in
    {s = \\_ => sub ++ neg ++ verb ++ compl };
    

  PassV3 v3 =
      lin VPSlash (predV (depV v3)) ** 
        {n3 = \\_ => [] ; c2 = v3.c3} ;  -- to preserve the order of args

  PassV2 v2 = lin VP (predV (depV v2)); 
  {-{
    s = vp.s ;
    a0 = vp.a0 ; 
    a1 = \\b => vp.a1 ! b ;
    n2 = \\a => vp.n2 ! a ++vp.n3 ! a ;
    a2 = vp.a2 ;
    ext = vp.ext ;
    en2 = vp.en2 ;
    ea2 = vp.ea2 ;
    eext = vp.eext ;
    act = Pass
    } ;-}


  PassV2Be v = insertObj 
        (\\a => v.s ! VI (VPtPret (agrAdjNP a DIndef) Nom)) 
        (predV verbBecome) ;

   
 
-- not adV, but for normal advers, 'han åt redan äpplet'
  AdvVPSlash vp adv = insertAdV adv.s vp ** {n3 = vp.n3;
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

----------------- Predeterminers,Quantifiers,Determiners

  lin
    bara_AdvFoc = (mkAdv "bara") ** {x = ""} ;

    sadana_PronAQ = mkA "sådan" ;
    fler_PronAD   = mkA "flera" "flera" "flera" "fler" "flest" ;

    hela_Predet    = {s  = \\_,_ => "hela" ; p = [] ; a = PNoAg} ;
    samma_Predet   = {s  = \\_,_ => "samma" ; p = [] ; a = PNoAg} ;

    sjaelva_Quant = {s  = \\_,_,_,_ => "själva" ;
                     sp = \\_,_,_,_ => NONEXIST;
                     det = DDef Def } ;

    vardera_Det  = {s,sp = \\_,_ => "vardera" ; n = Sg ; det = DDef Indef};
    ena_Det      = {s  = \\_,_ => "ena" ; 
                    sp = \\_ => genderForms ["den ena"] ["det ena"] ; 
                    n = Sg ; det = DDef Def};
    baegge_Det   = {s,sp = \\_,_ => "bägge" ; n = Pl ; det = DDef Def} ;
    baada_Det    = {s,sp = \\_,_ => "båda" ; n = Pl ; det = DDef Def} ;
    varannan_Det = {s,sp = \\_,_ => "varannan" ; n = Sg ; det = DDef Indef} ;
    somliga_Det  = {s,sp = \\_,_ => "somliga" ; n = Pl ; det = DDef Indef} ;
    dylika_Det   = {s,sp = \\_,_ => "dylika" ; n = Pl ; det = DDef Indef} ;
    oovriga_Det  = {s,sp = \\_,_ => "övriga" ; n = Pl ; det = DDef Indef} ;
    samtliga_Det = {s,sp = \\_,_ => "samtliga" ; n = Pl ; det = DDef Indef} ;
    aatskilliga_Det = {s,sp = \\_,_ => "åtskilliga" ; n = Pl ; det = DDef Indef} ;
    varenda_Det     = {s  = \\_ => genderForms ["varenda"] ["vartenda"] ; 
                       sp = \\_ => genderForms ["varenda en"] ["vartenda ett"] ; 
                       n = Sg ; det = DDef Indef};


    noll_Det = {s,sp = \\_,_ => "noll" ; n = Pl ; det = DDef Indef};

    numberOf = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep **
                {num = Pl; det = DDef Indef };
}

