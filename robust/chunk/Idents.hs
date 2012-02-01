module Idents where
import PGF
import Data.Maybe

text   = fromJust $ readType "Text"
phr    = fromJust $ readType "Phr"   
utt    = fromJust $ readType "Utt"
s      = fromJust $ readType "S"
cl     = fromJust $ readType "Cl"
vp     = fromJust $ readType "VP"
npsub  = fromJust $ readType "NPTyped Subject" -- NO PROBLEM! problem? solve by another parse category? NPCast
npobj  = fromJust $ readType "NPTyped Object" 
advsub = fromJust $ readType "AdvTyped Subject"  
advobj = fromJust $ readType "AdvTyped Object"  
v      = fromJust $ readType "V"
cn     = fromJust $ readType "CN"
detsub = fromJust $ readType "DetTyped Subject"
detobj = fromJust $ readType "DeTyped Objectt"
predet = fromJust $ readType "Predet"
apsub  = fromJust $ readType "APTyped Subject"
apobj  = fromJust $ readType "APTyped Object"
rcl    = fromJust $ readType "RCl"
idet   = fromJust $ readType "IDet"
card   = fromJust $ readType "Card"
art    = fromJust $ readType "Art"
rp     = fromJust $ readType "RP"
ada    = fromJust $ readType "AdA"
subj   = fromJust $ readType "Subj"
conj   = fromJust $ readType "Conj"
voc    = fromJust $ readType "Voc"
pol    = fromJust $ readType "Pol"  --no, this must be some special, just string "inte"

