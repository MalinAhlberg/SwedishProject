interface DiffScand = open CommonScand, Prelude in {

--1 Differences between Scandinavian languages

-- Norway has three genders, Danish and Swedish have two.

  oper
    NGender : PType ;
 
    ngen2gen : NGender -> Gender ;

    neutrum, utrum : NGender ;

---    gennum : Gender -> Number -> GenNum ;

-- This is the form of the noun in "det stora berget"/"det store berg".

    detDef : Species ;

-- Danish and Norwegian verbs, but not Swedish verbs, 
-- have two possible compound-tense auxiliaries ("have" or "være").

    Verb : Type ;

    hasAuxBe : Verb -> Bool ;

-- The rest of the parameters are function words used in the syntax modules.

    conjThat : Str ;
    conjThan : Str ;
    compMore : Str ;
    conjAnd  : Str ;
    infMark  : Str ;

    subjIf : Str ;

    artIndef : NGender => Str ;
    detIndefPl : Str ;

    verbHave : Verb ;
    verbBe   : Verb ;

    verbBecome : Verb ;

    auxFut : Str ;  
    auxCond : Str ;

    negation : Polarity => Str ;

-- For determiners; mostly two-valued even in Norwegian.

    genderForms : (x1,x2 : Str) -> NGender => Str ;

-- The forms of a relative pronoun ("som", "vars", "i vilken").

    relPron : Gender => Number => RCase => Str ;

-- Pronoun "sådan" used in $Relative.RelCl$.

    pronSuch : GenNum => Str ;

    reflPron : Agr -> Str ;

    hur_IAdv : {s : Str} ;

-----------
-- For $Verb$.
  VP = {
      s : VPForm => {
        fin : Str ;          -- V1 har  ---s1
        inf : Str            -- V2 sagt ---s4
        } ;
      a1 : Polarity => Str ; -- A1 inte ---s3
      n2 : Agr => Str ;      -- N2 dig  ---s5  
      a2 : Str ;             -- A2 idag ---s6
      ext : Str ;            -- S-Ext att hon går   ---s7
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


 mkClause : Str -> Agr -> VP -> Clause ;



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





}

