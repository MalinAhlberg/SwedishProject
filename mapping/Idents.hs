module Idents where

import PGF

cidASimul      = mkCId "ASimul"
cidAAnter      = mkCId "AAnter"
cidPositAdvAdj = mkCId "PositAdvAdj"
cidPositAdVAdj = mkCId "PositAdVAdj"
cidUseCl       = mkCId "UseCl"
cidUseQCl      = mkCId "UseQCl"
cidPredVP      = mkCId "PredVP"
cidAdjCN       = mkCId "AdjCN"
cidUseN        = mkCId "UseN"
cidDetQuant    = mkCId "DetQuant"
cidNumSg       = mkCId "NumSg"
cidNumPl       = mkCId "NumPl"
cidDetCN       = mkCId "DetCN"
cidIndefArt    = mkCId "IndefArt"
cidDefArt      = mkCId "DefArt"
cidUsePN       = mkCId "UsePN"
cidUseQuantPN  = mkCId "UseQuantPN"
cidSymbPN      = mkCId "SymbPN"
cidMkSymb      = mkCId "MkSymb"
cidUsePron     = mkCId "UsePron"
cidConjNP      = mkCId "ConjNP"
cidBaseNP      = mkCId "BaseNP"
cidConsNP      = mkCId "ConsNP"
cidConjCN      = mkCId "ConjCN"
cidBaseCN      = mkCId "BaseCN"
cidConsCN      = mkCId "ConsCN"
cidMassNP      = mkCId "MassNP"
cidAdvNP       = mkCId "AdvNP"
cidTPres       = mkCId "TPres"
cidTPast       = mkCId "TPast"
cidTFut        = mkCId "TFut"
cidTCond       = mkCId "TCond"
cidTTAnt       = mkCId "TTAnt"
cidPPos        = mkCId "PPos"
cidPNeg        = mkCId "PNeg"
cidComplSlash  = mkCId "ComplSlash"
cidSlashV2a    = mkCId "SlashV2a"
cidComplVS     = mkCId "ComplVS"
cidUseV        = mkCId "UseV"
cidAdVVP       = mkCId "AdVVP"
cidAdvVP       = mkCId "AdvVP"
cidAdvVPSlash  = mkCId "AdvVPSlash"
cidPrepNP      = mkCId "PrepNP"
cidto_Prep     = mkCId "to_Prep"
cidsuch_as_Prep= mkCId "such_as_Prep"
cidPastPartAP  = mkCId "PastPartAP"
cidPassV2      = mkCId "PassV2"
cidAdvS        = mkCId "AdvS"
cidPositA      = mkCId "PositA"
cidIDig        = mkCId "IDig"
cidIIDig       = mkCId "IIDig"
cidNumCard     = mkCId "NumCard" 
cidNumDigits   = mkCId "NumDigits"
cidNumNumeral  = mkCId "NumNumeral"
cidnum         = mkCId "num"
cidpot2as3     = mkCId "pot2as3"
cidpot1as2     = mkCId "pot1as2"
cidpot0as1     = mkCId "pot0as1"
cidpot01       = mkCId "pot01"
cidpot0        = mkCId "pot0"
cidn7          = mkCId "n7"
cidPossPron    = mkCId "PossPron"
cidCompAP      = mkCId "CompAP"
cidCompNP      = mkCId "CompNP"
cidCompAdv     = mkCId "CompAdv"
cidUseComp     = mkCId "UseComp"
cidCompoundCN  = mkCId "CompoundCN"
cidDashCN      = mkCId "DashCN"
cidProgrVP     = mkCId "ProgrVP"
cidGerundN     = mkCId "GerundN"
cidGenNP       = mkCId "GenNP"
cidPredetNP    = mkCId "PredetNP"
cidDetNP       = mkCId "DetNP"
cidAdAP        = mkCId "AdAP"
cidPositAdAAdj = mkCId "PositAdAAdj"

--cidUseA2       = mkCId "UseA2"
--cidUseA        = mkCId "UseA"
cidBaseAP      = mkCId "BaseAP"
cidConjAP      = mkCId "ConjAP"
cidAndConj     = mkCId "and_Conj"
cidOrConj      = mkCId "or_Conj"
cidConsAP      = mkCId "ConsAP"
cidQuestVP     = mkCId "QuestVP"
cidComplVV     = mkCId "ComplVV"
cidComplVA     = mkCId "ComplVA"
cidUseCopula   = mkCId "UseCopula"
cidPhrUtt      = mkCId "PhrUtt"
cidNoPConj     = mkCId "NoPConj"
cidNoVoc       = mkCId "NoVoc"
cidUttS        = mkCId "UttS"
cidUttQS       = mkCId "UttQS"
cidUseComparA  = mkCId "UseComparA"
cidOrdSuperl   = mkCId "OrdSuperl"
cidUttImpPol   = mkCId "UttImpPol"
cidImpVP       = mkCId "ImpVP"
cidPConjConj   = mkCId "PConjConj"
cidUttNP       = mkCId "UttNP"
cidGenericCl   = mkCId "GenericCl"
cidAdAdv       = mkCId "AdAdv"
cidConsAdv     = mkCId "ConsAdv"
cidBaseAdv     = mkCId "BaseAdv"
cidConjAdv     = mkCId "ConjAdv"
cidConsVPS     = mkCId "ConsVPS"
cidBaseVPS     = mkCId "BaseVPS"
cidConjVPS     = mkCId "ConjVPS"
cidConsS       = mkCId "ConsS"
cidBaseS       = mkCId "BaseS"
cidConjS       = mkCId "ConjS"
cidSubjS       = mkCId "SubjS"
cidUttAdv      = mkCId "UttAdv"
cidApposCN     = mkCId "ApposCN"
cidUseRCl      = mkCId "UseRCl"
cidImpersCl    = mkCId "ImpersCl"
cidReflVP      = mkCId "ReflVP"
-- added to Extra
cidDropAttVV   = mkCId "DropAttVV"
cidRelNP'      = mkCId "RelNP'"
cidPassV2'     = mkCId "PassV2'"

-- to implemented yet
cidSSubjS      = mkCId "SSubjS"
cidCNNumNP     = mkCId "CNNumNP"

-- words
cidBy8agent_Prep = mkCId "by8agent_Prep"
cidD_1         = mkCId "D_1"
cidName        = mkCId "john_PN"
cidCan_VV      = mkCId "can_VV"  --ambig! can be can8know 
cidMust_VV     = mkCId "must_VV"
cidWant_VV     = mkCId "want_VV"
cidHave_V2     = mkCId "have_V2"
cidGet_V2      = mkCId "faa_V2"   -- doesn't exist
cidGet_VV      = mkCId "faa_VV"   -- doesn't exist
cidDo_V2       = mkCId "do_V2"
cidBecome_V2   = mkCId "become_V2" -- doesn't exist
cidBecome_VA   = mkCId "become_VA" 
