--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs -[FocAdv,FocAP] ** {

cat ReflNP ; 
    PronAD ; -- relational pronouns which can act like adjectives and determiners. 'fler'
             -- can be compared as both: de flesta katter, katterna är flest
             --                             Predet?
             -- få sådana katter. not parsable, since få determiner.
    PronAQ ; -- relational pronouns which can act like adjectives and quantifiers. 'sådan'
  

    AdvFoc ; -- foucsing adverbs 'bara'. acts as predeterminers, normal adverbs or before finite verb
    
    RelVSCl ; 

fun
 
  RelVS : S -> RelVSCl -> S ; -- hon sover, vilket vi vet
  RelSlashVS : Temp -> Pol -> VS -> NP -> RelVSCl ;  -- vilket vi vet

  AdvFocVP : AdvFoc -> VP -> VP ; -- (han) bara log
  PredetAdvF : AdvFoc -> Predet ; -- bara (barn), inte ens (katten)
  FocAP : Comp -> NP -> Foc ; -- changed from AP -> NP -> Foc
                              -- to allow 'sådan är han'
                              -- also allows 'här är han' , 'katt är han'
                              -- which might actually be good
                              -- can remove FocAdv


  DetPronAD : PronAD -> Det ;
  QuantPronAQ : PronAQ -> Quant ;
  CompPronAQ : PronAQ -> Comp ;
  CompPronAD : PronAD -> Comp ;
  -- de blev sådana
  ComplVAPronAQ : VA -> PronAQ -> VP ;
 -- de blev fler
  ComplVAPronAD : VA -> PronAD -> VP ;


  CompoundNomN : N -> N -> N ;  -- fot+boll
  CompoundGenN : N -> N -> N ;  -- yrkes+musiker
  CompoundAdjN : A -> N -> N ;  -- vit+vin
 
  it8utr_Pron   : Pron ;
  this8denna_Quant : Quant ;
  
  -- alla sina syskon? sin brors bok?
  ReflCN : Num -> CN -> ReflNP ;
  ReflSlash : VPSlash -> ReflNP -> VP ;
  
  
  DropAttVV : VV -> VV ;      -- började att äta --> började äta. 
  SupCl  : NP -> VP -> Pol -> S ; -- när jag sovit
  -- needs to allow RelNP' to use it!
  
  
 
  UseComparA  : A -> AP ;
  PassV2   : V2 -> VP ; --VPSlash -> VP ;  -- äts 
  PassV2Be : V2 -> VP ;  -- bli äten
  
  -- not needed, RelCN handles this
  -- RelNP'   : Temp -> Pol ->  NP -> VP -> NP ; -- flickan som inte åt äpplen
  ---- RelNP "flickan, sådan att hon inte åt äpplen"
 
  ComplSlash : VPSlash -> NP -> VP ;
  ReflVP   : VPSlash -> VP ;

  {- here it would be nice with VPSlash -> AP
       'han är äten', 'han är given till henne', 
       'han är ombedd att gå' 'den är tänkt att sitta där'
     but VPSlash does not hold information about this verbform
     You can also say 'de gågna åren','de tänkta bostäderna',
     so maybe there should be a function V -> AP too. -}
  -- be callled PPartAP?
  PPartAP : V2 -> AP ; --VPSlash -> AP ;
   
  -- jag äter redan äpplet 
  -- does not work for Focused. 'det gör redan kvinnan' 632
  AdvVPSlash : VPSlash -> Adv -> VPSlash ;

  -- jag är redan här
  -- add possibility of saying 'det är här som jag äter' (CleftAdv + som)?
  AdvComp : Comp -> Adv -> Comp ;

  dethaer_NP : NP ;
  detdaer_NP : NP ;
  dedaer8utr_NP : NP ;
  dedaer8neut_NP : NP ;
  denhaer_NP : NP ;
  dendaer_NP : NP ;



}
