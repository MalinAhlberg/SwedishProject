module Types where
import PGF
import Data.Maybe

text   = fromJust $ readType "Text"
phr    = fromJust $ readType "Phr"   
utt    = fromJust $ readType "Utt"
sent   = fromJust $ readType "S"
cl     = fromJust $ readType "Cl"
vp     = fromJust $ readType "VP"
vpx    = fromJust $ readType "VPParse"
np     = fromJust $ readType "NP"
npsub  = fromJust $ readType "NPTyped Subject" 
npobj  = fromJust $ readType "NPTyped Object" 
adv    = fromJust $ readType "AdvTyped Object" --TODO should be subject too
adV    = fromJust $ readType "AdV"
iadv   = fromJust $ readType "IAdv"
--advsub = fromJust $ readType "AdvTyped Subject"  
--advobj = fromJust $ readType "AdvTyped Object"  
cadv   = fromJust $ readType "CAdv"  
v      = fromJust $ readType "V"
vs     = fromJust $ readType "VS"
vv     = fromJust $ readType "VV"
v2     = fromJust $ readType "V2"
v3     = fromJust $ readType "V3"
va     = fromJust $ readType "VA"
vq     = fromJust $ readType "VQ"
v2s    = fromJust $ readType "V2S"
v2q    = fromJust $ readType "V2Q"
v2a    = fromJust $ readType "V2A"
cn     = fromJust $ readType "CN"
n      = fromJust $ readType "N"
n2     = fromJust $ readType "N2"
det    = fromJust $ readType "Det"
--detsub = fromJust $ readType "DetTyped Subject"
--detobj = fromJust $ readType "DetTyped Object"
predet = fromJust $ readType "Predet"
ap     = fromJust $ readType "AP"
--apsub  = fromJust $ readType "APTyped Subject"
--apobj  = fromJust $ readType "APTyped Object"
rcl    = fromJust $ readType "RCl"
idet   = fromJust $ readType "IDet"
ip     = fromJust $ readType "IP"
card   = fromJust $ readType "Card"
art    = fromJust $ readType "Art"
rp     = fromJust $ readType "RP"
ada    = fromJust $ readType "AdA"
subj   = fromJust $ readType "Subj"
conj   = fromJust $ readType "Conj"
pconj  = fromJust $ readType "PConj"
voc    = fromJust $ readType "Voc"
icomp  = fromJust $ readType "IComp"
comp   = fromJust $ readType "Comp"
prep   = fromJust $ readType "Prep"
rs     = fromJust $ readType "RS"
iquant = fromJust $ readType "IQuant"
quant  = fromJust $ readType "Quant"
--quantsub = fromJust $ readType "QuantTyped Subject"
--quantobj = fromJust $ readType "QuantTyped Object"
pron   = fromJust $ readType "Pron"  -- obs! ej reflexive
relvp  = fromJust $ readType "RelVP"
advs   = [adv,adV,iadv]
--pol    = fromJust $ readType "Pol"  --no, this must be some special, just string "inte"
--advs = [advsub,advobj]
nps  = [npsub,npobj,np]
--npsubs = [npsub,np]
verbs   = [v,v2] --,v3,vs,vq,va,v2a,v2q,vv,v2v,v2s] 
phrText = fromJust $ readType "PhrText"
emsent = fromJust $ readType "SubjS" --obs!! 
--aps  = [apsub,apobj]
toGFStr :: [Type] -> Maybe String
toGFStr []                 = return ""
toGFStr xs  | xs == advs   = return "?advs"
            | xs == nps    = return "?nps"
            | xs == verbs  = return "?v"
toGFStr [x] | x  == cn     = return "?cn"
            | x  == np     = return "?np"
            | x  == npsub  = return "?npsub"
            | x  == npobj  = return "?npobj"
            | x  == ap     = return "?ap"
            | x  == adv    = return "?adv"
            | x  == iadv   = return "?iadv"
            | x  == adV    = return "?adV"
            | x  == v      = return "?v"
            | x  == conj   = return "?conj"
            | x  == comp   = return "?comp"
            | x  == icomp  = return "?icomp"
            | x  == sent   = return "?s"
            | x  == phrText= return "?phrText"
            | x  == emsent = return "?conj ?s"
toGFStr  x                = Nothing
           


