--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
-- unnecessarily complicated type of AdvNP???
concrete ExtraSwe of ExtraSweAbs = ExtraScandSwe - [TopAdv] ,
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
  TFutKommer = {s = []} ** {t = SFutKommer} ;   --# notpresent
-------------------------------------------------------------------------------
-- For objects          
-------------------------------------------------------------------------------

  ComplSlash vp np = 
       insertObjPost (\\a => vp.c2.s ++ np.s ! (getNPerson a) ! accusative ++ vp.n3 ! np.a) vp ;

   ReflIdPron =  {s = \\a => table {NPPoss g _ => reflGenForm a g ;
                                     _          => reflForm a }; 
                   a = agrP3 utrum Sg} ;


------------------------------------------------------------------------------
  lin
  SuperlA a = {
     s = \\ag,ap => a.s ! AF (ASuperl SupStrong) Nom ;
     isPre = True
     };

  ComparAP a = {
     s = \\ag,ap => a.s ! AF ACompar Nom ;
     isPre = True
     };
   
  --LeaveOutObj vps = lin VP (insertObj vps.n3 vps) ;

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
      voice = vp.voice ;
      en2 = vp.en2 ; ea2 = vp.ea2; eext = vp.eext } ;

 lin
  PredetAdvF adv = {s = \\_,_ => adv.s ; p = [] ; a = PNoAg} ;
  AdvFocAdV adv = {s = adv.s} ;


-------------------------------------------------------------------------------
-- For determiners and quantifiers
-------------------------------------------------------------------------------
lin 
  DetNP_utr _ = detNP utrum ;
  DetNP     _ = detNP neutrum ;

 oper detNP : NGender -> Det -> NP  =
   \g,det -> let 
          m = True ; 
      in lin NP {
        s = \\a => table {NPPoss _ _ => det.sp ! a ! m ! g ++ BIND ++ "s";
                          c          => det.sp ! a ! m ! g };
        a = agrP3 (ngen2gen g) det.n
      } ;
 lin
  QuantPronAQ _ x =
   let utr = x.s ! AF (APosit (Strong (GSg Utr))) Nom ;
       ntr = x.s ! AF (APosit (Strong (GSg Neutr))) Nom ;
       pl = x.s ! AF (APosit (Strong GPl)) Nom
   in
   {s = \\_ =>
     table {Sg => \\_,_ => genderForms ("en"++utr)
                                       ("ett"++ntr) ;
            Pl => \\_,_,_ => pl} ;
   sp = \\_ => table {Sg => \\_,_ => genderForms utr ntr;
               Pl => \\_,_,_ => pl};
     det = DDef Indef};


 lin 
 -- those cannot be compared 
  CompPronAQ x = CompAP (PositA (lin A x)) ; 

  DetPronAD _ x = lin Det {s,sp = \\a,_,_ => x.s ! AF (APosit (Strong GPl)) Nom ;
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
  TopVP vp np = {
      s = \\t,a,p =>
        let
          subj = np.s ! aNPerson ! CommonScand.nominative ;
          agr  = np.a ;
          vps  = vp.s ! vp.voice ! VPFinite t a  ;  
          vf = case <<t,a> : STense * Anteriority> of {
            <SPres,Simul> => vps.fin;
            <SPast,Simul> => vps.fin;
            <_    ,Simul> => vps.inf;
            <SPres,Anter> => vps.inf;
            <SPast,Anter> => vps.inf;
            <_    ,Anter> => (vp.s ! vp.voice ! VPFinite SPast Anter  ).inf
            };
          verb = mkClause subj agr (predV do_V) ;                        
          comp = vp.n2 ! agr ++ vp.a2 ! np.a ++ vp.ext     
        in
        vf ++ comp ++ (verb.s ! t ! a ! p ! Inv) ++ vp.a1 ! Pos 
      } ;

  oper do_V : V = mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord" ;

lin
  TopAP ap np    = 
  {s = \\t,a,p => 
   let vp = UseComp ap ; --(CompAP ap);
       vps = vp.s ! vp.voice ! VPFinite t a  ;
       npAgr = np.a in
    vp.n2 ! npAgr ++ vps.fin ++ np.s ! aNPerson !  NPNom 
    ++ negation ! p++ vps.inf };


  TopVV vv vp np = 
  {s = \\t,a,p =>
    let bara = vp.a0 ;
        vps = vp.s ! vp.voice ! VPInfinit Simul ;
        vvp = UseV vv ;
        vvs = vvp.s ! vvp.voice ! VPFinite t a  ; 
        always = vp.a1 ! Pos ++ vvp.a1 ! Pos ;
        already = vp.a2 ! np.a ++ vvp.a2 ! np.a in
   bara ++ vps.inf ++ vp.n2 ! np.a ++ vvs.fin ++ np.s ! aNPerson ! NPNom 
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

  SupCl np vp pol = let sub = np.s ! aNPerson ! nominative ;
                        verb = (vp.s ! vp.voice ! VPFinite SPres Anter).inf ;
                        neg  = vp.a1 ! pol.p ++ pol.s ;
                        compl = vp.n2 ! np.a ++ vp.a2 ! np.a ++ vp.ext in
    {s = \\_ => sub ++ neg ++ vp.a0 ++ verb ++ compl };
    
   PassVP vp = {
    s = vp.s ;
    a0 = vp.a0 ; 
    a1 = vp.a1 ;
    n2 = \\a => vp.n2 ! a ++ vp.n3 ! a ++ vp.c2.s ;
    a2 = vp.a2 ;
    ext = vp.ext ;
    voice = Pass ;
    en2 = vp.en2 ;
    ea2 = vp.ea2 ;
    eext = vp.eext
   } ;   
  
  PassV2 v = insertObj 
        (\\a => v.s ! VI (VPtPret (agrAdjNP a DIndef) Nom)++v.part) 
        (predV verbBecome) ;

   
 
  PPartAP v2 =
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
     _   => (getPronoun a).s ! aNPerson ! NPAcc } ;


 oper reflGenForm : NPerson -> GenNum -> Str = \aSub,aObj   ->
    case <aSub,aObj> of {
     <Per3,GSg Neutr> => "sitt" ;
     <Per3,GSg Utr>   => "sin" ;
     <Per3,GPl>       => "sina" ;
     _              => reflGenPron aSub aObj};
  
  oper reflGenPron : NPerson -> GenNum -> Str =
   \sub,gn -> let pn = getPronoun sub
      in pn.s ! aNPerson ! NPPoss gn Nom ;


      
  this_NP : Str -> Gender -> Number -> NP =
  \denna,g,n -> lin NP {s = \\_ => table {NPPoss gn c => denna+"s";
                                   _           => denna};
                           a = agrP3 g n};

  getPronoun : NPerson -> {s : NPerson => NPForm => Str ; a : Agr} = 
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
    tillochmed_AdvFoc = (ss "till och med") ** {x = ""} ;



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
  boerja_med_VV = mkVV (partV (mkV "börjar") "med") ;
  --ge_V3' = mkV3 (irregV "ge" "gav" "gett")  ;
    numberOf = mkN2 (mkN "antal" "antalet" "antalen" "antalena") noPrep **
                {num = Pl; det = DDef Indef };

   likna_V2 = dirV2 (mkV "liknar") ;
   akta_V3  = dirV3 (mkV "aktar") (mkPrep "för") ; 
   flicka_N = mkN "flicka" ;
   komma_V = mkV "komma" "kom" "kommit" ;
   frysa_V = mkV "frysa" "frös" "frusit" ;

}

