concrete StructuralSwe of Structural = CatSwe ** 
  open MorphoSwe, ParadigmsSwe, MakeStructuralSwe,
  (X = ConstructX), Prelude in {

  flags optimize=all ;

  lin
  above_Prep = ss "ovanf�r" ;
  after_Prep = ss "efter" ;
  by8agent_Prep = ss "av" ;
  all_Predet = {s = detForms "all" "allt" "alla" ; p = [] ; a = PNoAg} ;
  almost_AdA, almost_AdN = ss "n�stan" ;
  although_Subj = ss "fast" ;
  always_AdV = ss "alltid" ;
  and_Conj = {s1 = [] ; s2 = "och" ; n = Pl} ;
  because_Subj = ss "eftersom" ;
  before_Prep = ss "f�re" ;
  behind_Prep = ss "bakom" ;
  between_Prep = ss "mellan" ;
  both7and_DConj = sd2 "b�de" "och" ** {n = Pl} ;
  but_PConj = ss "men" ;
  by8means_Prep = ss "med" ;
  can8know_VV, can_VV = 
    mkV "kunna" "kan" "kunn" "kunde" "kunnat" "kunnen" **
    {c2 = mkComplement [] ; lock_VV = <>} ;
  during_Prep = ss "under" ;
  either7or_DConj = sd2 "antingen" "eller" ** {n = Sg} ;
  everybody_NP = regNP "alla" "allas" Utr Pl ;
  every_Det = {
    s = \\_,_ => "varje" ; 
    sp = \\_,_ => "var och en" ; ----
    n = Sg ; 
    det = DIndef
    } ;
  everything_NP = regNP "allting" "alltings" Neutr Sg ;
  everywhere_Adv = ss "�verallt" ;
  few_Det  = {s,sp = \\_,_ => "f�" ; n = Pl ; det = DDef Indef} ;
---  first_Ord = {s = "f�rsta" ; isDet = True} ;
  for_Prep = ss "f�r" ;
  from_Prep = ss "fr�n" ;
  he_Pron = MorphoSwe.mkNP "han"  "honom"  "hans" "hans" "hans"  Utr Sg P3 ;
  here_Adv = ss "h�r" ;
  here7to_Adv = ss "hit" ;
  here7from_Adv = ss "h�rifr�n" ;
  how_IAdv = ss "hur" ;
  how8much_IAdv = ss "hur mycket" ;
  how8many_IDet = {s = \\_ => ["hur m�nga"] ; n = Pl ; det = DDef Indef} ;
  if_Subj = ss "om" ;
  in8front_Prep = ss "framf�r" ;
  i_Pron = MorphoSwe.mkNP "jag"  "mig"  "min" "mitt" "mina"  Utr Sg P1 ;
  in_Prep = ss "i" ;
  it_Pron = MorphoSwe.regNP "det" "dess" Neutr Sg ;
  less_CAdv = X.mkCAdv "mindre" "�n" ;
  many_Det = {s,sp = \\_,_ => "m�nga" ; n = Pl ; det = DDef Indef} ;
  more_CAdv = X.mkCAdv "mer" "�n" ;
  most_Predet = {s = detForms ["den mesta"] ["det mesta"] ["de flesta"] ; p = [] ; a = PNoAg} ;
  much_Det = {s,sp = \\_,_ => "mycket" ; n = Pl ; det = DDef Indef} ;
  must_VV = 
    mkV "f�" "m�ste" "f�" "fick" "m�st" "m�st" ** 
    {c2 = mkComplement [] ; lock_VV = <>} ;
  no_Utt = ss ["nej"] ;
  on_Prep = ss "p�" ;
---  one_Quant = {s = \\_,_ => genderForms ["en"] ["ett"] ; n = Sg ; det = DIndef} ;
  only_Predet = {s = \\_,_ => "bara" ; p = [] ; a = PNoAg} ;
  or_Conj = {s1 = [] ; s2 = "eller" ; n = Sg} ;
  otherwise_PConj = ss "annars" ;
  part_Prep = ss "av" ;
  please_Voc = ss "tack" ; ---
  possess_Prep = ss "av" ;
  quite_Adv = ss "ganska" ;
  she_Pron = MorphoSwe.mkNP "hon" "henne" "hennes" "hennes" "hennes"  Utr Sg P3 ;
  so_AdA = ss "s�" ;
  someSg_Det = {s,sp = \\_ => genderForms "n�gon" "n�got" ; n = Sg ; det = DIndef} ;
  somePl_Det = {s,sp = \\_,_ => "n�gra" ; n = Pl ; det = DIndef} ;
  somebody_NP = regNP "n�gon" "n�gons" Utr Sg ;
  something_NP = regNP "n�got" "n�gots" Neutr Sg ;
  somewhere_Adv = ss "n�gonstans" ;
  that_Quant = 
    {s,sp = table {
       Sg => \\_,_ => genderForms ["den d�r"] ["det d�r"] ; 
       Pl => \\_,_,_ => ["de d�r"]
       } ;
     det = DDef Def
    } ;
  there_Adv = ss "d�r" ;
  there7to_Adv = ss "dit" ;
  there7from_Adv = ss "d�rifr�n" ;
  therefore_PConj = ss "d�rf�r" ;
  they_Pron = MorphoSwe.mkNP "de" "dem" "deras" "deras" "deras" Utr Pl P1 ;
  this_Quant = 
    {s,sp = table {
       Sg => \\_,_ => genderForms ["den h�r"] ["det h�r"] ; 
       Pl => \\_,_,_ => ["de h�r"]
       } ;
     det = DDef Def
    } ;
  through_Prep = ss "genom" ;
  too_AdA = ss "f�r" ;
  to_Prep = ss "till" ;
  under_Prep = ss "under" ;
  very_AdA = ss "mycket" ;
  want_VV = 
    mkV "vilja" "vill" "vilj" "ville" "velat" "velad" ** 
    {c2 = mkComplement [] ; lock_VV = <>} ;
  we_Pron = MorphoSwe.mkNP "vi"  "oss"  "v�r" "v�rt" "v�ra"  Utr Pl P1 ;
  whatSg_IP = {s = \\_ => "vad" ; g = Neutr ; n = Sg} ; ---- infl, g
  whatPl_IP = {s = \\_ => "vad" ; g = Neutr ; n = Pl} ; ---- infl, g
  when_IAdv = ss "n�r" ;
  when_Subj = ss "n�r" ;
  that_Subj = ss "att" ;
  where_IAdv = ss "var" ;
  which_IQuant = {
    s = table {
      Sg => genderForms "vilken" "vilket" ;
      Pl => \\_ => "vilka" 
      } ; 
    det = DIndef
    } ;
  whoSg_IP = {s = vem.s ; g = Utr ; n = Sg} ;
  whoPl_IP = {s = \\_ => "vilka" ; g = Utr ; n = Pl} ;
  why_IAdv = ss "varf�r" ;
  without_Prep = ss "utan" ;
  with_Prep = ss "med" ;
  yes_Utt = ss ["ja"] ;
  youSg_Pron = MorphoSwe.mkNP "du" "dig" "din" "ditt" "dina" Utr Sg P2 ;
  youPl_Pron = MorphoSwe.mkNP "ni" "er" "er" "ert" "era"  Utr Pl P2 ;
  youPol_Pron = MorphoSwe.mkNP "ni" "er" "er" "ert" "era"  Utr Sg P2 ; --- wrong in refl

-- Auxiliaries that are used repeatedly.

  oper
    vem = MorphoSwe.mkNP "vem" "vem" "vems" "vems" "vems" Utr Sg P3 ;

lin
  not_Predet = {s = \\_,_ => "inte" ; p = [] ; a = PNoAg} ;
  no_Quant = 
    {s,sp = table {
       Sg => \\_,_ => genderForms "ingen" "inget" ; 
       Pl => \\_,_,_ => "inga"
       } ;
     det = DIndef
    } ;

  if_then_Conj = {s1 = "om" ; s2 = "s�" ; n = singular} ;
  nobody_NP = regNP "ingen" "ingens" Utr Sg ;
  nothing_NP = regNP "inget" "ingets" Neutr Sg ;

  at_least_AdN = ss "minst" ;
  at_most_AdN = ss "h�gst" ;

  except_Prep = ss "utom" ;

  as_CAdv = X.mkCAdv "lika" "som" ;
  have_V2 = dirV2 (mkV "ha" "har" "ha" "hade" "haft" "haft") ; ---- pp

  lin language_title_Utt = ss "svenska" ;

}

