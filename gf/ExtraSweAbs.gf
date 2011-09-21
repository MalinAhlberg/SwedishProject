-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs ** {


fun
  CompoundNomN : N -> N -> N ;  -- fot+boll
  CompoundGenN : N -> N -> N ;  -- yrkes+musiker
  CompoundAdjN : A -> N -> N ;  -- vit+vin
 
  it8utr_Pron   : Pron ;
  this8denna_Quant : Quant ;
  ReflGenVP : VPSlash -> CN -> VP ; --han tog sin bok 

  DropAttVV : VV -> VV ;      -- började att äta --> började äta. 
  SupCl  : NP -> VP -> Pol -> S ; -- när jag sovit
  
  
  PassV2   : V2 -> VP ;  -- äts 
  PassV2Be : V2 -> VP ;  -- bli äten
  RelNP'   : NP -> VP -> Temp -> Pol -> NP ;

  

  dethaer_NP : NP ;
  detdaer_NP : NP ;
  dedaer8utr_NP : NP ;
  dedaer8neut_NP : NP ;
  denhaer_NP : NP ;
  dendaer_NP : NP ;
}
