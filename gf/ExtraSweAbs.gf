--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs -[FocAdv,FocAP] ** {
    
cat --ReflNP ; 
    PronAD ; -- relational pronouns which can act like adjectives and determiners. 'fler'
             -- can be compared as both: de flesta katter, katterna är flest
             --                             Predet?
             -- få sådana katter. not parsable, since få determiner.
    PronAQ ; -- relational pronouns which can act like adjectives and quantifiers. 'sådan'
  
    AdvFoc ; -- foucsing adverbs 'bara'. acts as predeterminers, normal adverbs or before finite verb
    
    RelVSCl ; 

    N2P ;
    N2' ;
    SimpleVP ;
    --- objectives
    Obj ;
    [Obj]{2} ;
--    BaseObj ;    --to prevent normal NPs to be conjuncted as Objs, but doesn't allow 'han ser sin katt och huset'
--    [BaseObj]{2} ;
 
  -- ExistNP :: CN or RelCNNP and add Adv AdV to verb!
  -- ImpersonCl :: sin katt åt man??
fun

 
   DetNP : Det -> NP ;
-------------------------------------------------------------------------------
-- For objects          
-------------------------------------------------------------------------------
-- put in VerbSwe and unimport there
     Slash2V3 : V3 -> Obj -> VPSlash ;
     Slash3V3 : V3  -> Obj -> VPSlash ;
     ComplSlash : VPSlash -> Obj -> VP ;
     CompNP : Obj -> Comp ;
     SlashV2VNP : V2V -> Obj -> VPSlash -> VPSlash ;
     Coercion : NP -> Obj ;
    ReflCN : CN -> Num -> Obj ;
    --ReflCN : NP -> Obj ;
    ReflIdNP : Obj ;
    --UseObj : BaseObj -> Obj ;
    --ConjBaseObj : Conj -> ListBaseObj -> BaseObj ;
    ConjObj : Conj -> ListObj -> Obj ;

  -- alla sina syskon? sin brors bok?
 -- ReflCN : Num -> CN -> ReflNP ;
 -- ReflSlash : VPSlash -> ReflNP -> VP ;
  --ReflCN : CN -> Num -> NP ;  --xs bil
 -- IdRefl : NP ;               --sig
 -- IdReflSelf: NP ;            --sig själv
  -- ReflVP   : VPSlash -> VP ;


-------------------------------------------------------------------------------


  --tests
  SuperlA : A -> AP ;

-------------------------------------------------------------------------------
-- Formal subjects
-------------------------------------------------------------------------------
  FormalSub : SimpleVP -> Det -> CN -> Cl ; -- det sitter en katt där

  SimpleV      : V -> SimpleVP ;            -- sitter
  Pass2VSimple : V2 -> SimpleVP ;     -- skrivs 
  AdvSimpleVP : SimpleVP -> Adv -> SimpleVP ;
  AdVSimpleVP : SimpleVP -> AdV -> SimpleVP ;

-------------------------------------------------------------------------------
-- Test for 'antalet'
-------------------------------------------------------------------------------
  --ApposNP : NP -> NP -> NP ;  -- ett mycket stort antal katter (add '(hennes katt) johan'?)
  ComplN2P : Det -> N2P -> CN -> NP ;
  AdjN2 : AP -> N2P -> N2P ;
  UseN2P : N2' -> N2P ;
  N2N    : N2' -> N ; 
  ---

-------------------------------------------------------------------------------
-- Varandra
-------------------------------------------------------------------------------
  VarandraVP : VPSlash -> VP ;
  SlashV3Varandra : V3 -> VPSlash ;
   
-------------------------------------------------------------------------------
-- tests, doesn't work for Foc anyway
-------------------------------------------------------------------------------
  VS_it : VS -> VP ; -- hon vet det
  VV_it : VV -> VP ; -- hon vill det
  

-------------------------------------------------------------------------------
-- tests, 'själv'. No good types.
-------------------------------------------------------------------------------
  SelfNP  : NP -> NP ;  --kungen själv
  SelfAdV : AdV ;  -- han såg själv att ..

-------------------------------------------------------------------------------
-- tests, genetive
-------------------------------------------------------------------------------
  GenCN : NP -> Num -> CN -> NP ;
  PredGen : NP -> NP -> Cl ;      -- den är min, not needed atm, but maybe good if we try to avoid PossPron later

-------------------------------------------------------------------------------
-- Relatives
-------------------------------------------------------------------------------
  RelVS : S -> RelVSCl -> S ; -- hon sover, vilket vi vet
  RelSlashVS : Temp -> Pol -> NP -> VS -> RelVSCl ;  -- vilket vi vet
  RelCNNP : Num -> CN -> RS -> NP ;  -- de äpplen du äter

-------------------------------------------------------------------------------
-- Focusing adverbs
-------------------------------------------------------------------------------
  AdvFocVP : AdvFoc -> VP -> VP ; -- (han) bara log
  PredetAdvF : AdvFoc -> Predet ; -- bara (barn), inte ens (katten)
  AdvFocAdV : AdvFoc -> AdV     ;  -- (hon sover) bara
  FocAP : Comp -> NP -> Foc ; -- changed from AP -> NP -> Foc
                              -- to allow 'sådan är han'
                              -- also allows 'här är han' , 'katt är han'
                              -- which might actually be good
                              -- can remove FocAdv

-------------------------------------------------------------------------------
-- For determiners and quantifiers
-------------------------------------------------------------------------------
-- overgenerating, but useful
  DetNP_utr : Det -> NP ; -- den här

  DetPronAD : PronAD -> Det ;
  QuantPronAQ : PronAQ -> Quant ;
  CompPronAQ : PronAQ -> Comp ;
  CompPronAD : PronAD -> Comp ;
  -- de blev sådana
  ComplVAPronAQ : VA -> PronAQ -> VP ;
 -- de blev fler
  ComplVAPronAD : VA -> PronAD -> VP ;

  it8utr_Pron   : Pron ;
  this8denna_Quant : Quant ;


---
  CompoundNomN : N -> N -> N ;  -- fot+boll
  CompoundGenN : N -> N -> N ;  -- yrkes+musiker
  CompoundAdjN : A -> N -> N ;  -- vit+vin
 
  
-------------------------------------------------------------------------------
-- Various functions
-------------------------------------------------------------------------------
  ComplBareVV : VV -> VP -> VP ;      -- började att äta --> började äta. 
  SupCl  : NP -> VP -> Pol -> S ; -- när jag sovit
  -- needs to allow RelNP' to use it!
  
  
 
  UseComparA  : A -> AP ;
  PassV2   : V2 -> VP ;  -- äts 
  PassV3   : V3 -> VPSlash ;  --ges till henne
  PassV2Be : V2 -> VP ;  -- bli äten
  
 

  {- here it would be nice with VPSlash -> AP
       'han är äten', 'han är given till henne', 
       'han är ombedd att gå' 'den är tänkt att sitta där'
     but VPSlash does not hold information about this verbform
     You can also say 'de gågna åren','de tänkta bostäderna',
     so maybe there should be a function V -> AP too. -}
  -- den nyligen funna.
  -- be callled PPartAP?
  PPartAP : V2 -> AP ; --VPSlash -> AP ;
   
  -- jag äter redan äpplet 
  -- does not work for Focused. 'det gör redan kvinnan' 632
  -- is redan more a AdV anyway?
  AdvVPSlash : VPSlash -> Adv -> VPSlash ;

  -- jag är redan här
  -- add possibility of saying 'det är här som jag äter' (CleftAdv + som)?
  -- is redan more a AdV anyway?
  AdvComp : Comp -> Adv -> Comp ;


-------------------------------------------------------------------------------
-- Predeterminers,Quantifiers,Determiners
-------------------------------------------------------------------------------

   bara_AdvFoc : AdvFoc ;

  sadana_PronAQ : PronAQ ;
  fler_PronAD : PronAD ;
  -- overgenerating: alla hela katter. should not be ok.
  -- predets should be able to decide definites?
  hela_Predet : Predet ;  --hela horder/hela katten  -- both
  sjaelva_Quant : Quant ; -- själva kungen/själva öronen -- def
  samma_Predet : Predet ; -- samma katter/samma öra 
  varenda_Det : Det ;
  vardera_Det : Det ;
  ena_Det : Det ;
  baegge_Det : Det ;
  baada_Det : Det ;
  varannan_Det : Det ;
  somliga_Det : Det ;
  dylika_Det : Det ;
  oovriga_Det : Det ;
  aatskilliga_Det : Det ;
  samtliga_Det : Det ;

  noll_Det : Det ;
  annan_Quant : Quant ;

  numberOf : N2' ;

  likna_V2 : V2 ;
  akta_V3 : V3 ;
  flicka_N : N ;
  komma_V : V ;
  frysa_V : V ;
  -- LeaveOutObj : VPSlash -> VP ;
}
