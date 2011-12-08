--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe - [FocAdv] ,
                                   ParadigmsSwe - [nominative] **
 open CommonScand, ResSwe, ParamX, VerbSwe, Prelude, DiffSwe, StructuralSwe, MorphoSwe,
      NounSwe, Coordination, AdjectiveSwe, SentenceSwe, RelativeSwe in {

lincat
 PronAQ = A ; -- 'en sådan' 
 PronAD = A ; -- 'fler' 
 AdvFoc = {s : Str ; x : Str} ; 
 RelVSCl = {s : Agr => RCase => Str};
 N2P = CN ** {c2 : Complement ; det : DetSpecies ; num : Number} ; 
 N2' = N2 ** {det : DetSpecies ; num : Number} ;
 SimpleVP = VP ;
 Obj = {s : Agr => Str };

lin

  DetNP _ det = 
      let 
        g = neutrum ; 
        m = True ;
      in {
        s = \\a => table {NPPoss _ _ => det.sp ! a ! m ! g ++ BIND ++ "s" ;
                   _          => det.sp ! a ! m ! g };
        a = agrP3 (ngen2gen g) det.n
      } ;


  TFutKommer = {s = []} ** {t = SFutKommer} ;   --# notpresent
-------------------------------------------------------------------------------
-- For objects          
-------------------------------------------------------------------------------
     ComplSlash np vp = 
       insertObjPost (\\a => vp.c2.s ++ np.s ! (getNPerson a) ! accusative ++ vp.n3 ! a) vp ;

  -- ReflIdGen should be 'sin'. sp-field : han ser sin. Or 'han ser sig'?
  -- maybe just 'sin', since 'sig' not a quantifier
  -- ReflIdNP shoud be 'sig'. Genitive form? Do not want 'sin' again, it
  -- is not a NP. But then what? 'sigs'?
    ReflIdNP    = {s = \\a => table {NPPoss g _ => reflGenForm a g ;
                                     _          => reflForm a }; 
                   a = agrP3 utrum Sg} ;
    ReflIdGen =  {s,sp = \\a,n,m,d,g => reflGenForm a (gennum g n) ;
                  det = DIndef} ;

------------------------------------------------------------------------------
  lin
  SuperlA _ a = {
     s = \\ag,ap => a.s ! AF (ASuperl SupStrong) Nom ;
     isPre = True
     };

  LeaveOutObj vps = lin VP (insertObj vps.n3 vps) ;
-------------------------------------------------------------------------------
-- Formal subjects
-------------------------------------------------------------------------------
  SimpleV v = lin VP (predV v) ;
  Pass2VSimple v2 = lin VP (predV (depV v2)); 
  AdvSimpleVP vp = AdvVP (lin VP vp) ;
  AdVSimpleVP vp adv = AdVVP adv (lin VP vp) ;
  FormalSub vp det cn = case det.det of {
      DIndef => let np = detCN  det cn in
                    mkClause "det" (agrP3 neutrum Sg) (insertObj   
                       (\\a => np.s ! aNPerson ! accusative) vp) ;
      DDef _ => {s = \\_,_,_,_ => NONEXIST ; agr = agrP3 utrum Sg}} ;


-------------------------------------------------------------------------------
-- Test for 'antalet'
-------------------------------------------------------------------------------
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

 
-------------------------------------------------------------------------------
-- Varandra
-------------------------------------------------------------------------------
  varandra = {s = table {Per1 Sg => \\_ => NONEXIST ; Per2 Sg => \\_ => NONEXIST ;
                         _ => \\_ => "varandra"}  ;
              a = {g = Utr ; n = Pl ; p = P2}};  -- obs!! Person?

-------------------------------------------------------------------------------
-- Relatives
-------------------------------------------------------------------------------
 lin
  RelVS s rvs = {s = \\o => s.s ! o ++ "," ++ rvs.s ! agrP3 Neutr Sg ! RPrep True} ; 
  RelSlashVS t p np vs = let cl     = PredVP np (predV vs) ; 
                             vilket = IdRP.s ! Neutr ! Sg ! (RPrep True) in
    {s = \\ag,rc => t.s ++ p.s ++ vilket ++ cl.s ! t.t ! t.a ! p.p ! Sub } ;
 RelCNNP _ num cn rs = let g = cn.g ; n = num.n in {
      s = \\a,c => num.s ! g ++ det n g 
                 ++ cn.s ! n ! DIndef ! (caseNP c) ++ rs.s ! agrP3 (ngen2gen g) n ! RNom ;
      a = agrP3 (ngen2gen g) n
      } ;
  oper det : Number -> Gender -> Str = 
    \n,g -> case <n,g> of {
                 <Sg,Utr>   => "den" ;
                 <Sg,Neutr> => "det" ;
                 <Pl,_>     => "de" };



-------------------------------------------------------------------------------
-- Focusing adverbs
-------------------------------------------------------------------------------
lin
 AdvFocVP  = insertAdvFoc ;
  
  oper insertAdvFoc : AdvFoc -> VP -> VP 
   = \adv, vp -> lin VP {
      s = vp.s ;
      a0 = adv.s ;
      a1 = vp.a1 ; n2 = vp.n2 ; a2 = vp.a2 ; ext = vp.ext ;
      en2 = vp.en2 ; ea2 = vp.ea2; eext = vp.eext } ;

 lin
  PredetAdvF adv = {s = \\_,_ => adv.s ; p = [] ; a = PNoAg} ;
  AdvFocAdV adv = {s = adv.s} ;


-------------------------------------------------------------------------------
-- For determiners and quantifiers
-------------------------------------------------------------------------------
lin 
  DetNP_utr _ = detNP utrum Sg ;

  oper detNP : NGender -> Number -> Det -> NP  =
   \g,num,det -> let 
          m = True ;  ---- is this needed for other than Art?
      in lin NP {
        s = \\a => table {NPPoss _ _ => det.sp ! a ! m ! g ++ BIND ++ "s";
                          c          => det.sp ! a ! m ! g };
        a = agrP3 (ngen2gen g) num
      } ;

 lin 
 -- those cannot be compared 
  CompPronAQ x = CompAP (PositA (lin A x)) ; 

  DetPronAD x = lin Det {s,sp = \\a,_,_ => x.s ! AF (APosit (Strong GPl)) Nom ;
            n = Pl ; det = DDef Indef} ;

  CompPronAD x = CompAP (PositA (lin A x)) ; 

  -- de blev sådana
 ComplVAPronAQ v ap = insertObj (\\a => (PositA ap).s ! getNPerson a ! agrAdjNP a DIndef) (predV v) ;
 -- de blev fler
 ComplVAPronAD v ap = insertObj (\\a => (UseComparA ap).s !  getNPerson a ! agrAdjNP a DIndef) (predV v) ;

 
  it8utr_Pron = regPron "den" "dess" Utr   Sg  ;
  
  this8denna_Quant = 
    {s,sp = \\_ => table {
      Sg => \\_,_ => genderForms ["denna"] ["detta"] ; 
      Pl => \\_,_,_ => ["dessa"]
      } ;
    det = DDef Indef
    } 
    ;

-------------------------------------------------------------------------------
-- Implemented in ExtraScand
-------------------------------------------------------------------------------
lin
  FocVP vp np = {
      s = \\t,a,p =>
        let
          subj = np.s ! CommonScand.nominative ;
          agr  = np.a ;
          vps  = vp.s ! VPFinite t a  ;  
          vf = case <<t,a> : STense * Anteriority> of {
            <SPres,Simul> => vps.fin;
            <SPast,Simul> => vps.fin;
            <_    ,Simul> => vps.inf;
            <SPres,Anter> => vps.inf;
            <SPast,Anter> => vps.inf;
            <_    ,Anter> => (vp.s ! VPFinite SPast Anter  ).inf
            };
          verb = mkClause subj agr (predV do_V) ;                        
          comp = vp.n2 ! agr ++ vp.a2 ! np.a ++ vp.ext     
        in
        vf ++ comp ++ (verb.s ! t ! a ! p ! Inv) ++ vp.a1 ! Pos 
      } ;

  oper do_V : V = mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord" ;

lin
  FocAP ap np    = 
  {s = \\t,a,p => 
   let vp = UseComp ap ; --(CompAP ap);
       vps = vp.s ! VPFinite t a  ;
       npAgr = np.a in
    vp.n2 ! npAgr ++ vps.fin ++ np.s !  NPNom 
    ++ negation ! p++ vps.inf };


  FocVV vv vp np = 
  {s = \\t,a,p =>
    let bara = vp.a0 ;
        vps = vp.s ! VPInfinit Simul ;
        vvp = UseV vv ;
        vvs = vvp.s ! VPFinite t a  ; 
        always = vp.a1 ! Pos ++ vvp.a1 ! Pos ;
        already = vp.a2 ! np.a ++ vvp.a2 ! np.a in
   bara ++ vps.inf ++ vp.n2 ! np.a ++ vvs.fin ++ np.s ! NPNom 
   ++ vv.c2.s ++ always ++ negation ! p ++ already ++ vvs.inf
   };  


lin
  PrepCN prep cn = {s = \\a => prep.s ++ cn.s ! Sg ! DIndef ! Nom } ;
 
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

  
-------------------------------------------------------------------------------
-- Various functions
-------------------------------------------------------------------------------
  ComplBareVV vv vp =  insertObj (\\a => infVP vp a) (predV vv) ;

  SupCl np vp pol = let sub = np.s ! nominative ;
                        verb = (vp.s ! VPFinite SPres Anter).inf ;
                        neg  = vp.a1 ! pol.p ++ pol.s ;
                        compl = vp.n2 ! np.a ++ vp.a2 ! np.a ++ vp.ext in
    {s = \\_ => sub ++ neg ++ vp.a0 ++ verb ++ compl };
    
  
  PassV3 v3 =
      lin VPSlash (predV (depV v3)) ** 
        {n3 = \\_ => [] ; c2 = v3.c3} ;  -- to preserve the order of args

  PassV2 v2 = lin VP (predV (depV v2)); 
  
  PassV2Be v = insertObj 
        (\\a => v.s ! VI (VPtPret (agrAdjNP a DIndef) Nom)) 
        (predV verbBecome) ;

   
 
  PPartAP _ v2 =
    {s     = \\a,aform => v2.s ! VI (VPtPret aform Nom);
     isPre = True} ; 

  ReflSlash vp np = let vp_l = lin VPSlash vp ;
                        np_l = lin NP np      ;
                        obj  = vp.n3 ! np.a   in
    lin VP (insertObjPost (\\a => vp.c2.s ++ reflForm a np.a ++ np.s ! NPNom++obj) vp) ; 



-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

 oper reflForm : NPerson -> Str = 
   \a -> case a of {
     Per3  => "sig" ;
     _   => (getPronoun a).s ! NPAcc } ;


 oper reflGenForm : NPerson -> GenNum -> Str = \aSub,aObj   ->
    case <aSub,aObj> of {
     <Per3,GSg Neutr> => "sitt" ;
     <Per3,GSg Utr>   => "sin" ;
     <Per3,GPl>       => "sina" ;
     _              => reflGenPron aSub aObj};
  
  oper reflGenPron : NPerson -> GenNum -> Str =
   \sub,gn -> let pn = getPronoun sub
      in pn.s ! NPPoss gn Nom ;


      
  this_NP : Str -> Gender -> Number -> NP =
  \denna,g,n -> lin NP {s = \\_ => table {NPPoss gn c => denna+"s";
                                   _           => denna};
                           a = agrP3 g n};

  getPronoun : NPerson -> Pron = 
   \a ->  case a of {
      Per1 Sg  => i_Pron ;
      Per2 Sg  => youSg_Pron ;
      Per1 Pl  => we_Pron ;
      Per2 Pl  => youPl_Pron ;
      _         => they_Pron } ; -- this last case will not happen

-------------------------------------------------------------------------------
-- Predeterminers,Quantifiers,Determiners
-------------------------------------------------------------------------------

  lin
    bara_AdvFoc = (ss "bara") ** {x = ""} ;

    sadana_PronAQ = mkA "sådan" ;
    fler_PronAD   = mkA "flera" "flera" "flera" "fler" "flest" ;

    hela_Predet    = {s  = \\_,_ => "hela" ; p = [] ; a = PNoAg} ;
    samma_Predet   = {s  = \\_,_ => "samma" ; p = [] ; a = PNoAg} ;

    sjaelva_Quant = {s  = \\_,_,_,_,_ => "själva" ;
                     sp = \\_,_,_,_,_ => NONEXIST;
                     det = DDef Def } ;

    vardera_Det  = {s,sp = \\_,_,_ => "vardera" ; n = Sg ; det = DDef Indef};
    ena_Det      = {s  = \\_,_,_ => "ena" ; 
                    sp = \\_,_ => genderForms ["den ena"] ["det ena"] ; 
                    n = Sg ; det = DDef Def};
    baegge_Det   = {s,sp = \\_,_,_ => "bägge" ; n = Pl ; det = DDef Def} ;
    baada_Det    = {s,sp = \\_,_,_ => "båda" ; n = Pl ; det = DDef Def} ;
    varannan_Det = {s,sp = \\_,_ => genderForms ["varannan"] ["varannat"] ; 
                    n = Sg ; det = DDef Indef} ;
    somliga_Det  = {s,sp = \\_,_,_ => "somliga" ; n = Pl ; det = DDef Indef} ;
    dylika_Det   = {s,sp = \\_,_,_ => "dylika" ; n = Pl ; det = DDef Indef} ;
    oovriga_Det  = {s,sp = \\_,_,_ => "övriga" ; n = Pl ; det = DDef Indef} ;
    samtliga_Det = {s,sp = \\_,_,_ => "samtliga" ; n = Pl ; det = DDef Indef} ;
    aatskilliga_Det = {s,sp = \\_,_,_ => "åtskilliga" ; n = Pl ; det = DDef Indef} ;
    varenda_Det     = {s  = \\_,_ => genderForms ["varenda"] ["vartenda"] ; 
                       sp = \\_,_ => genderForms ["varenda en"] ["vartenda ett"] ; 
                       n = Sg ; det = DDef Indef};

   annan_Quant = { s,sp = \\_ => table {Sg => \\_,_ => genderForms ["en annan"] ["ett annat"] ;
                               Pl => \\_,_,_ => "andra"} ;
                   det = DIndef };




                    

    noll_Det = {s,sp = \\_,_,_ => "noll" ; n = Pl ; det = DDef Indef};

    --annnan/andra?
    numberOf = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep **
                {num = Pl; det = DDef Indef };

   likna_V2 = dirV2 (mkV "liknar") ;
   akta_V3  = dirV3 (mkV "aktar") (mkPrep "för") ; 
   flicka_N = mkN "flicka" ;
   komma_V = mkV "komma" "kom" "kommit" ;
   frysa_V = mkV "frysa" "frös" "frusit" ;

}

