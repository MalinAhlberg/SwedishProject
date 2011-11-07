--1 Scandinavian auxiliary operations

interface ResScand = DiffScand ** open CommonScand, Prelude in {

--2 Constants uniformly defined in terms of language-dependent constants

  param
    CardOrd = NCard NGender | NOrd AFormSup ; -- sic! (AFormSup)

  oper  
    agrP3 : Gender -> Number -> Agr = \g,n -> {
      g = g ;
      n = n ;
      p = P3
      } ;

    Noun = {s : Number => Species => Case => Str ; g : NGender} ;

-- This function is here because it depends on $verbHave, auxFut, auxCond$.

   predV : Verb -> VP = \verb -> 
    let
      diath = case verb.vtype of {
        VPass => Pass ;
        _ => Act
        } ;
      vfin : Tense -> Str = \t -> verb.s ! vFin t diath ;
      vsup = verb.s ! VI (VSupin diath) ; --# notpresent  
      vinf = verb.s ! VI (VInfin diath) ;

      auxv = case hasAuxBe verb of {
        True => verbBe.s ;
        _ => verbHave.s
        } ;

      har : Tense -> Str = \t -> auxv ! vFin t Act ;
      ha  : Str = auxv ! VI (VInfin Act) ;

      vf : Str -> Str -> {fin,inf : Str} = \fin,inf -> {
        fin = fin ; inf = inf ++ verb.part
        } ;

    in {
    s = table {
      VPFinite t Simul => case t of {
--        Pres | Past => vf (vfin t) [] ; -- the general rule
        Past => vf (vfin t) [] ;    --# notpresent
        Fut  => vf auxFut vinf ;    --# notpresent
        Cond => vf auxCond vinf ;   --# notpresent
        Pres => vf (vfin t) []
        } ;
      VPFinite t Anter => case t of {    --# notpresent
        Pres | Past => vf (har t) vsup ; --# notpresent
        Fut  => vf auxFut (ha ++ vsup) ; --# notpresent
        Cond => vf auxCond (ha ++ vsup)  --# notpresent
        } ;                              --# notpresent
      VPImperat => vf (verb.s ! VF (VImper diath)) [] ;
      VPInfinit Anter => vf [] (ha ++ vsup) ;  --# notpresent
      VPInfinit Simul => vf [] vinf
      } ;
    a1  : Polarity => Str = negation ;
    n2  : Agr => Str = \\a => case verb.vtype of {
      VRefl => reflPron a ;
      _ => []
      } ;
    a2  : Str = [] ;
    ext : Str = [] ;
    en2,ea2,eext : Bool = False   -- indicate if the field exists
    } ;

-- needed for VP conjunction
  param
    VPIForm = VPIInf | VPISup ; ---- sup not yet used

---- herefrom

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

    VType = VAct | VPass | VRefl ;

-- Used in $DiffScand.predV$.

  vFin : STense -> Voice -> VForm = \t,v -> case t of {
    Pres => VF (VPres v) 
      ; --# notpresent
    Past => VF (VPret v) ;  --# notpresent
    _ => VI (VInfin v) --# notpresent
    } ;
 

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


-- For $Sentence$.

  Clause : Type = {
    s : Tense => Anteriority => Polarity => Order => Str
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



}