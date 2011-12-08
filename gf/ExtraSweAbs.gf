--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs -[FocAdv,FocAP] ** {
    
cat
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
fun

 
   DetNP : (a : Boolean) -> Det a -> NP a ;
-------------------------------------------------------------------------------
-- For objects          
-------------------------------------------------------------------------------
     ComplSlash : NPObject -> VPSlash -> VP ;
    ReflIdNP    : NP Object ;
    ReflIdGen : Quant Object ;


-------------------------------------------------------------------------------
  SuperlA : (a : Boolean) -> A -> AP a ;

-------------------------------------------------------------------------------
-- Formal subjects
-------------------------------------------------------------------------------
  FormalSub : SimpleVP -> Det Subject -> CN -> Cl ; -- det sitter en katt där

  SimpleV      : V -> SimpleVP ;            -- sitter
  Pass2VSimple : V2 -> SimpleVP ;     -- skrivs 
  AdvSimpleVP : SimpleVP -> AdvObject -> SimpleVP ;
  AdVSimpleVP : SimpleVP -> AdV -> SimpleVP ;

-------------------------------------------------------------------------------
-- Varandra
-------------------------------------------------------------------------------
  varandra : NPObject ; 
   
-------------------------------------------------------------------------------
-- Relatives
-------------------------------------------------------------------------------
  RelVS : S -> RelVSCl -> S ; -- hon sover, vilket vi vet
  RelSlashVS : Temp -> Pol -> NPSubject -> VS -> RelVSCl ;  -- vilket vi vet
  RelCNNP : (a : Boolean) -> Num -> CN -> RS -> NP a  ;  -- de äpplen du äter

-------------------------------------------------------------------------------
-- Focusing adverbs
-------------------------------------------------------------------------------
  AdvFocVP : AdvFoc -> VP -> VP ; -- (han) bara log
  PredetAdvF : AdvFoc -> Predet ; -- bara (barn), inte ens (katten)
  AdvFocAdV : AdvFoc -> AdV     ;  -- (hon sover) bara
  FocAP : Comp -> NPSubject -> Foc ; -- changed from AP -> NP -> Foc
                              -- to allow 'sådan är han'
                              -- also allows 'här är han' , 'katt är han'
                              -- which might actually be good
                              -- can remove FocAdv

-------------------------------------------------------------------------------
-- For determiners and quantifiers
-------------------------------------------------------------------------------
  DetNP_utr : (a : Boolean) -> Det a -> NP a ; -- den här

  DetPronAD : PronAD -> Det Object ;
  QuantPronAQ : PronAQ -> Quant Object ;
  CompPronAQ : PronAQ -> Comp ;
  CompPronAD : PronAD -> Comp ;
  -- de blev sådana
  ComplVAPronAQ : VA -> PronAQ -> VP ;
 -- de blev fler
  ComplVAPronAD : VA -> PronAD -> VP ;

  it8utr_Pron   : Pron ;
  this8denna_Quant : Quant Object ;


---
  CompoundNomN : N -> N -> N ;  -- fot+boll
  CompoundGenN : N -> N -> N ;  -- yrkes+musiker
  CompoundAdjN : A -> N -> N ;  -- vit+vin
 
  
-------------------------------------------------------------------------------
-- Various functions
-------------------------------------------------------------------------------
  ComplBareVV : VV -> VP -> VP ;      -- började att äta --> började äta. 
  SupCl  : NPSubject -> VP -> Pol -> S ; -- när jag sovit
  
  
  UseComparA  : (a : Boolean) -> A -> AP a ;
  PassV2   : V2 -> VP ;  -- äts 
  PassV3   : V3 -> VPSlash ;  --ges till henne
  PassV2Be : V2 -> VP ;  -- bli äten
  
 

   PPartAP : (a : Boolean) -> V2 -> AP a ; --VPSlash -> AP ;
   
 
-------------------------------------------------------------------------------
-- Predeterminers,Quantifiers,Determiners
-------------------------------------------------------------------------------

   bara_AdvFoc : AdvFoc ;

  sadana_PronAQ : PronAQ ;
  fler_PronAD : PronAD ;
  hela_Predet : Predet ;  --hela horder/hela katten 
  sjaelva_Quant : Quant Object ; -- själva kungen/själva öronen
  samma_Predet : Predet ; -- samma katter/samma öra 
  varenda_Det : Det Object ;
  vardera_Det : Det Object ;
  ena_Det : Det Object ;
  baegge_Det : Det Object ;
  baada_Det : Det Object ;
  varannan_Det : Det Object ;
  somliga_Det : Det Object ;
  dylika_Det : Det Object ;
  oovriga_Det : Det Object ;
  aatskilliga_Det : Det Object ;
  samtliga_Det : Det Object ;

  noll_Det : Det Object ;
  annan_Quant : Quant Object ;

  numberOf : N2' ;

  likna_V2 : V2 ;
  akta_V3 : V3 ;
  flicka_N : N ;
  komma_V : V ;
  frysa_V : V ;
}
