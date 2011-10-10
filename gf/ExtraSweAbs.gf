-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs ** {

cat ReflNP ;

fun
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
   
  -- does not work for Focused. 'det gör redan kvinnan' 632
  AdvVPSlash : VPSlash -> Adv -> VPSlash ;

  -- add possibility of saying 'det är här som jag äter' (CleftAdv + som)?
  AdvComp : Comp -> Adv -> Comp ;

  dethaer_NP : NP ;
  detdaer_NP : NP ;
  dedaer8utr_NP : NP ;
  dedaer8neut_NP : NP ;
  denhaer_NP : NP ;
  dendaer_NP : NP ;

}
