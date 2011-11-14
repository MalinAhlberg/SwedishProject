--1 Auxiliary operations common for Scandinavian languages.
--
-- This module contains operations that are shared by the Scandinavian
-- languages, without dependence on parameters.
-- Without functions not used by new Swedish

resource CommonScand2 = ParamX ** open Prelude in {

  flags optimize=all ;

param
  Species = Indef | Def ;
  Case    = Nom | Gen ;
  Voice   = Act | Pass ;

-- The principal word orders in predication: main clause, inverted, subordinate.

  Order   = Main | Inv | Sub ;

-- The types of noun definiteness required by determiners. Examples:
-- "ett stort hus" (DIndef), "mitt stora hus" (DDef Indef), 
-- "det stora huset" (DDed Def).

  DetSpecies = DIndef | DDef Species ;

-- These are the gender-number combinations needed for adjective inflection,
-- minimizing the number of forms in the lexicon: there is no gender dependency
-- in the plural, and only two genders in the singular even in Norwegian.

  GenNum  = GSg Gender | GPl ;

  Gender  = Utr | Neutr ;

  AForm   = AF AFormGrad Case ;

  AFormGrad =
     APosit  AFormPos
   | ACompar  
   | ASuperl AFormSup ;

-- The $Number$ in $Weak$ only matters in "lilla"/"sm�".

  AFormPos = Strong GenNum | Weak Number ;
  AFormSup = SupStrong | SupWeak ;
 
  NPForm = NPNom | NPAcc | NPPoss GenNum Case ;

  RCase = RNom | RGen | RPrep Bool ;

  RAgr = RNoAg | RAg Gender Number Person ;

  PredetAgr = PNoAg | PAg Number ;



oper
  Complement : Type = {s : Str ; hasPrep : Bool} ;

  Agr : PType = {g : Gender ; n : Number ; p : Person} ;

  nominative : NPForm = NPNom ;
  accusative : NPForm = NPAcc ;

  caseNP : NPForm -> Case = \np -> case np of {
    NPPoss _ _ => Gen ;
    _ => Nom
    } ;

  specDet : DetSpecies -> Species = \d -> case d of {
    DDef Def => Def ;
    _ => Indef
    } ;

  mkComplement : Str -> Complement = \s -> {
    s = s ;
    hasPrep = case s of {
      "" => False ;
      _ => True
      }
    } ;

-- Used in $Noun.AdjCN$.

  agrAdjNP : Agr -> DetSpecies -> AFormPos = \a ->
    agrAdj (gennumAgr a) ;

  agrAdj : GenNum -> DetSpecies -> AFormPos = \gn,d -> 
    case <<gn,d> : GenNum * DetSpecies> of {
      <_,  DIndef> => Strong gn ;
      <GPl,DDef _> => Weak Pl ;
      _            => Weak Sg
    } ;

  gennum : Gender -> Number -> GenNum = \g,n ->
      case n of {
        Sg => GSg g ;
        Pl => GPl
        } ;

  gennumAgr : Agr -> GenNum = \a -> gennum a.g a.n ;

   
-- Used in $ConjunctionScand$.

  conjGender : Gender -> Gender -> Gender = \g,h -> case g of {
    Utr => h ;
    _ => Neutr 
    } ;

  conjAgr : (_,_ : Agr) -> Agr = \a,b -> {
    g = conjGender a.g b.g ;
    n = conjNumber a.n b.n ;
    p = conjPerson a.p b.p
    } ;

---

-- For $Lex$.

-- For each lexical category, here are the worst-case constructors.
--
-- But $mkNoun$ is fully defined only for each language, since
-- $Gender$ varies.

  nounForms : (x1,_,_,x4 : Str) -> (Number => Species => Case => Str) = 
      \man,mannen,men,mennen -> \\n,d,c => case <n,d> of {
        <Sg,Indef> => mkCase c man ;
        <Sg,Def>   => mkCase c mannen ;
        <Pl,Indef> => mkCase c men ;
        <Pl,Def>   => mkCase c mennen
        } ;

  Adjective : Type = {s : AForm => Str} ;

  mkAdjective : (x1,_,_,_,_,_,x7 : Str) -> {s : AForm => Str} = 
    \liten, litet, lilla, sma, mindre, minst, minsta -> {
    s = table {
      AF (APosit a) c          => mkCase c (mkAdjPos a liten litet lilla sma) ;
      AF ACompar c             => mkCase c mindre ;
      AF (ASuperl SupStrong) c => mkCase c minst ;
      AF (ASuperl SupWeak) c   => mkCase c minsta
      } 
    } ;

-- These are useful auxiliaries.

  mkCase : Case -> Str -> Str = \c,f -> case c of {
      Nom => f ;
      Gen => f + case last f of {
        "s" | "z" | "x" => [] ;
        _ => "s"
        }
      } ;

  mkAdjPos : AFormPos -> (s1,_,_,s4 : Str) -> Str =
    \a, liten, litet, lilla, sma ->
    case a of {
      Strong gn => case gn of {
        GSg Utr => liten ;
        GSg Neutr => litet ;
        GPl => sma
      } ;
     Weak Sg => lilla ;
     Weak Pl => sma
   } ;

  mkVoice : Voice -> Str -> Str = \v,s -> case v of {
    Act => s ;
    Pass => s + case last s of {
      "s" => "es" ;
      _   => "s"
      }
    } ;


-- For $Noun$.

  artDef : GenNum -> Str = \gn -> gennumForms "den" "det" "de" ! gn ;

  mkNP : (x1,_,_,_,x5 : Str) -> Gender -> Number -> Person -> 
         {s : NPForm => Str ; a : Agr} = \du,dig,din,ditt,dina,g,n,p -> {
    s = table {
      NPNom => du ;
      NPAcc => dig ;
      NPPoss h c => mkCase c (gennumForms din ditt dina ! h)
      } ;
    a = {
      g = g ;
      n = n ;
      p = p
      }
    } ;

  gennumForms : (x1,x2,x3 : Str) -> GenNum => Str = \den,det,de -> 
    table {
      GSg Utr => den ;
      GSg Neutr => det ;
      _ => de
    } ;  

  detForms : (x1,x2,x3 : Str) -> Gender => Number => Str = \den,det,de -> 
    \\g,n => gennumForms den det de ! gennum g n ;

  regNP : Str -> Str -> Gender -> Number -> {s : NPForm => Str ; a : Agr} =
    \det,dess,g,n ->
    mkNP det det dess dess dess g n P3 ;


}
