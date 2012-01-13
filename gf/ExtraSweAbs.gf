--# -path=./gf:.:swedish:prelude:alltenses:abstract:scandinavian:common
-- Structures special for Swedish. These are not implemented in other
-- Scandinavian languages.

abstract ExtraSweAbs = ExtraScandAbs -[TopAdv,TopAP] ** {
    
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
    VPX ;

 
 -- fun
   --TensedVPI : Temp -> Pol -> [VPI] -> VP ;
 --  MkVPX : Pol -> VP -> VPX ;
 --  ConjVPX : Conj -> [VPX] -> VPX ;
 --  PredVPX : XTense -> Order -> NP -> VPX -> S ;
 --  ComplFut : Pol -> VPX -> VPS ;
 --  ComplFutKommer : Pol -> VPX -> VPS ;
 --  ComplSup : Pol -> VPX -> VPS ;
----  ConjVP : Conj -> [VP] -> VP ;
 --  cat [VPX]{2} ;

 fun
   DetNP : (a : NPType) -> DetTyped a -> NPTyped a ;
-------------------------------------------------------------------------------
-- For objects          
-------------------------------------------------------------------------------

    ComplSlash :VPSlash ->  NPTyped Object -> VP ;
    ReflIdPron : PronTyped Object ;  -- sig, sin 


-------------------------------------------------------------------------------
  SuperlA : A -> AP ;
  ComparAP : A -> AP ;  -- en större katt

-------------------------------------------------------------------------------
-- Formal subjects
-------------------------------------------------------------------------------
  FormalSub : SimpleVP -> DetTyped Subject -> CN -> Cl ; -- det sitter en katt där

  SimpleV      : V -> SimpleVP ;            -- sitter
  Pass2VSimple : V2 -> SimpleVP ;     -- skrivs 
  AdvSimpleVP : SimpleVP -> AdvTyped Object -> SimpleVP ;
  AdVSimpleVP : SimpleVP -> AdV -> SimpleVP ;

-------------------------------------------------------------------------------
-- Varandra
-------------------------------------------------------------------------------
  varandra : NPTyped Object ; 
   
-------------------------------------------------------------------------------
-- Relatives
-------------------------------------------------------------------------------
  RelVS : S -> RelVSCl -> S ; -- hon sover, vilket vi vet
  RelSlashVS : Temp -> Pol -> NPTyped Subject -> VS -> RelVSCl ;  -- vilket vi vet
  RelCNNP : (a : NPType) -> Num -> CN -> RS -> NPTyped a  ;  -- de äpplen du äter

-------------------------------------------------------------------------------
-- Focusing adverbs
-------------------------------------------------------------------------------
  AdvFocVP : AdvFoc -> VP -> VP ; -- (han) bara log
  PredetAdvF : AdvFoc -> Predet ; -- bara (barn), inte ens (katten)
  AdvFocAdV : AdvFoc -> AdV     ;  -- (hon sover) bara
  TopAP : Comp -> NPTyped Subject -> Top ; -- changed from AP -> NP -> Top
                              -- to allow 'sådan är han'
                              -- also allows 'här är han' , 'katt är han'
                              -- which might actually be good
                              -- can remove TopAdv

-------------------------------------------------------------------------------
-- For determiners and quantifiers
-------------------------------------------------------------------------------
  DetNP_utr : (a : NPType) -> DetTyped a -> NPTyped a ; -- den här

  DetPronAD : (a : NPType) -> PronAD -> DetTyped a ;
  -- not implented??
  QuantPronAQ : (a : NPType) -> PronAQ -> QuantTyped a ;
  CompPronAQ : PronAQ -> Comp ;
  CompPronAD : PronAD -> Comp ;
  -- de blev sådana
  ComplVAPronAQ : VA -> PronAQ -> VP ;
 -- de blev fler
  ComplVAPronAD : VA -> PronAD -> VP ;

  it8utr_Pron   : Pron  ;
  this8denna_Quant : Quant ;


---
  CompoundNomN : N -> N -> N ;  -- fot+boll
  CompoundGenN : N -> N -> N ;  -- yrkes+musiker
  CompoundAdjN : A -> N -> N ;  -- vit+vin
 
  
-------------------------------------------------------------------------------
-- Various functions
-------------------------------------------------------------------------------
  ComplBareVV : VV -> VP -> VP ;      -- började att äta --> började äta. 
  SupCl  : NPTyped Subject -> VP -> Pol -> S ; -- när jag sovit
  
  
  UseComparA  : A -> AP ;
  PassV2 : V2 -> VP ;  -- bli äten --shouldn't need to be here, but get irrefutable pattern
  PassVP : VPSlash -> VP ; -- ätas
  
 

   PPartAP : V2 -> AP ; --VPSlash -> AP ;
   
 
-------------------------------------------------------------------------------
-- Predeterminers,Quantifiers,Determiners
-------------------------------------------------------------------------------

   bara_AdvFoc : AdvFoc ;
   tillochmed_AdvFoc : AdvFoc ;

  sadana_PronAQ : PronAQ ;
  fler_PronAD : PronAD ;
  hela_Predet : Predet ;  --hela horder/hela katten 
  sjaelva_Quant : Quant  ; -- själva kungen/själva öronen
  samma_Predet : Predet ; -- samma katter/samma öra 
  varenda_Det : Det  ;
  vardera_Det : Det  ;
  ena_Det : Det  ;
  baegge_Det : Det ;
  baada_Det : Det  ;
  varannan_Det : Det ;
  somliga_Det : Det  ;
  dylika_Det : Det  ;
  oovriga_Det : Det  ;
  aatskilliga_Det : Det ;
  samtliga_Det : Det ;

  noll_Det : Det  ;
  annan_Quant : Quant  ;

  numberOf : N2' ;
  boerja_med_VV : VV ; 
  ge_V3' : V3 ; 

  likna_V2 : V2 ;
  akta_V3 : V3 ;
  flicka_N : N ;
  komma_V : V ;
  frysa_V : V ;
}
