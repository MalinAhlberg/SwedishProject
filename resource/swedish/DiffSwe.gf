instance DiffSwe of DiffScand = open CommonSwe, Prelude in {

-- Parameters.

  oper
    NGender = Gender ; 
    ngen2gen g = g ;
    utrum = Utr ; 
    neutrum = Neutr ;

    detDef : Species = Def ;

    Verb : Type = {
      s : VForm => Str ;
      part : Str ;
      vtype : VType
      } ;

    hasAuxBe _ = False ;



-- Strings.

    conjThat = "att" ;
    conjThan = "�n" ;
    conjAnd = "och" ;
    infMark  = "att" ;
    compMore = "mera" ;

    subjIf = "om" ;

    artIndef : NGender => Str = table {
      Utr => "en" ;
      Neutr => "ett"
      } ;
    detIndefPl = "n�gra" ;

    verbHave = 
      mkVerb "ha" "har" "ha" "hade" "haft" "havd" "havt" "havda" ** noPart ;
    verbBe = 
      mkVerb "vara" "�r" "var" "var" "varit" "varen" "varet" "varna" 
      ** noPart ;
    verbBecome = 
      mkVerb "bli" "blir" "bli" "blev" "blivit" "bliven" "blivet" "blivna"
      ** noPart ;

    -- auxiliary
    noPart = {part = []} ;

    auxFut = "ska" ;      -- "skall" in ExtSwe
    auxCond = "skulle" ;

    negation : Polarity => Str = table {
      Pos => [] ;
      Neg => "inte"
      } ;

    genderForms : (x1,x2 : Str) -> NGender => Str = \all,allt -> 
      table {
        Utr => all ;
        Neutr => allt
        } ;

    relPron : Gender => Number => RCase => Str = \\g,n,c => case c of {
      RNom | RPrep False => "som" ;
      RGen  => "vars" ;
      RPrep True => gennumForms "vilken" "vilket" "vilka" ! gennum g n
      } ;

    pronSuch = gennumForms "s�dan" "s�dant" "s�dana" ;

    reflPron : Agr -> Str = \a -> case <a.n,a.p> of {
      <Pl,P1> => "oss" ;
      <Pl,P2> => "er" ;
      <Sg,P1> => "mig" ;
      <Sg,P2> => "dig" ;
      <_, P3> => "sig"
      } ;

    hur_IAdv = {s = "hur"} ;
    -------
-- For $Verb$.
  VP = {
      s : VPForm => {
        fin : Str ;          -- V1 har  ---s1
        inf : Str            -- V2 sagt ---s4
        } ;
      a1 : Polarity => Str ; -- A1 inte ---s3
      n2 : Agr => Str ;      -- N2 dig  ---s5  
      a2 : Str ;             -- A2 idag ---s6
      ext : Str ;            -- S-Ext att hon g�r   ---s7
      --- ea1,ev2,           --- these depend on params of v and a1
      en2,ea2,eext : Bool    -- indicate if the field exists
      } ;

  insertObj : (Agr => Str) -> VP -> VP = \obj,vp -> {
    s = vp.s ;
    a1 = vp.a1 ;
    n2 = \\a => obj ! a ++ vp.n2 ! a ;
    a2 = vp.a2 ;
    ext = vp.ext ;
    en2 = True ;
    ea2 = vp.ea2 ;
    eext = vp.eext
    } ;

  insertObjPost : (Agr => Str) -> VP -> VP = \obj,vp -> {
    s = vp.s ;
    a1 = vp.a1 ;
    n2 = \\a => vp.n2 ! a ++ obj ! a ;
    a2 = vp.a2 ;
    ext = vp.ext ;
    en2 = True ;
    ea2 = vp.ea2 ;
    eext = vp.eext
    } ;

  insertAdv : Str -> VP -> VP = \adv,vp -> {
    s = vp.s ;
    a1 = vp.a1 ;
    n2 = vp.n2 ;
    a2 = vp.a2 ++ adv ;
    ext = vp.ext ;
    en2 = vp.en2 ;
    ea2 = True ;
    eext = vp.eext
    } ;

  insertAdV : Str -> VP -> VP = \adv,vp -> {
    s = vp.s ;
    a1 = \\b => vp.a1 ! b ++ adv ;
    n2 = vp.n2 ;
    a2 = vp.a2 ;
    ext = vp.ext ;
    en2 = vp.en2 ;
    ea2 = vp.ea2 ;
    eext = vp.eext
    } ;

  infVP : VP -> Agr -> Str = \vp,a -> 
    vp.a1 ! Pos ++ (vp.s ! VPInfinit Simul).inf ++ vp.n2 ! a ++ vp.a2 ++ vp.ext ; --- a1

mkVerb : (x1,_,_,_,_,_,_,x8 : Str) -> {s : VForm => Str ; vtype : VType} = 
   \finna,finner,finn,fann,funnit,funnen,funnet,funna -> {
   s = table {
    VF (VPres Act)  => finner ;
    VF (VPres Pass) => mkVoice Pass finn ;
    VF (VPret v)    => mkVoice v fann ;  --# notpresent
    VF (VImper v)   => mkVoice v finn ;
    VI (VInfin v)   => mkVoice v finna ;
    VI (VSupin v)   => mkVoice v funnit ;  --# notpresent
    VI (VPtPret a c)=> mkCase c (mkAdjPos a funnen funnet funna funna)
    } ;
   vtype = VAct
   } ;

-- Used in $DiffScand.predV$.

  vFin : STense -> Voice -> VForm = \t,v -> case t of {
    SPres => VF (VPres v) 
      ; --# notpresent
    SPast => VF (VPret v) ;  --# notpresent
    _ => VI (VInfin v) --# notpresent
    } ;
 
-- For $Sentence$.

  Clause : Type = {
    s : STense => Anteriority => Polarity => Order => Str
    } ;


 mkClause : Str -> Agr -> VP -> Clause = \subj,agr,vp -> {
      s = \\t,a,b,o => 
        let 
          verb  = vp.s  ! VPFinite t a ;
          neg   = vp.a1 ! b ;
          compl = vp.n2 ! agr ++ vp.a2 ++ vp.ext
        in
        case o of {
          Main => subj ++ verb.fin ++ neg ++ verb.inf ++ compl ;
          Inv  => verb.fin ++ subj ++ neg ++ verb.inf ++ compl ;
          Sub  => subj ++ neg ++ verb.fin ++ verb.inf ++ compl
          }
    } ;



param
  VForm = 
     VF VFin
   | VI VInf ;

  VFin =
     VPres Voice
   | VPret Voice   --# notpresent
   | VImper Voice
   ;

  VInf = 
     VInfin Voice
   | VSupin Voice  --# notpresent
   | VPtPret AFormPos Case
   ;

  VPForm = 
     VPFinite STense Anteriority
   | VPImperat
   | VPInfinit Anteriority ;


   STense =
     SPres 
   | SPast   --# notpresent
   | SFut    --# notpresent
   | SFut'   --# notpresent -- komma att
   | SCond   --# notpresent
   ;

 VType = VAct | VPass | VRefl ;


}
