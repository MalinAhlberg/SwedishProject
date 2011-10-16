module Rules where

import General  -- language-independent functional morphology library
import Types    -- morphological parameters and word classes

-- Missing: Species.

-- Noun: Weak Declension

-- Maskulinum
species_masc :: Species -> (Number,Case) -> [String] -> Str
species_masc Indef _ xs = strings xs
species_masc Def (n,c) xs = strings $ 
   case (c,n) of 
    (Gen,Sg) -> [x ++ "ns"  | x <- xs]
    (Dat,Sg) -> [x ++ "num" | x <- xs]
    (_,Sg)   -> [x ++ "n"   | x <- xs]
    (Nom,Pl) -> [x ++  y    | x <- xs, y <- ["ni", "nir"]]
    (Gen,Pl) -> [x ++ "nna" | x <- xs]
    (Dat,Pl) -> [x ++ "in"  | x <- xs]
    (Ack,Pl) -> [x ++ "na"  | x <- xs]

biti_rule :: String -> NN
biti_rule biti = i_rule (tk 1 biti)

bryti_rule :: String -> NN
bryti_rule bryti = except (i_rule bryti) [(NN_C Sg Nom Indef,bryti)]

landbo_rule :: String -> NN
landbo_rule landbo (NN_C n c d) = species_masc d (n,c) $  
    case (n,c) of
     (Sg,Nom) -> [landbo, landbo++"i", landbo++"e"]
     (Pl,Nom) -> [landbo ++ suff | suff <- ["ar","r","","ær"]]
     (Pl,Dat) -> [landbo++"um", landbo++"om", landbo++"m"]
     _        -> [landbo, landbo++"a", landbo++"æ"]
      
i_rule :: String -> NN
i_rule bit (NN_C n c d) = species_masc d (n,c) $
    case (n,c) of
     (Sg,Nom) -> [bit++"i", bit++"e"]
     (Pl,Nom) -> bita_bitae ++ [bit++"ar",bit++"ær"]
     (Pl,Dat) -> [bit++"um", bit++"om"]
     _        -> bita_bitae
  where bita_bitae = [bit++"a", bit++"æ"]
        
-- Femininum
species_fem :: Species -> (Number,Case) -> [String] -> Str
species_fem Indef _ xs = strings xs
species_fem Def (n,c) xs = strings $ 
   case (c,n) of 
    (Nom,Sg) -> [x ++ "n"   | x <- xs]
    (Gen,Sg) -> [x ++ y     | x <- xs, y <- ["nna", "nnar"]]
    (Dat,Sg) -> [x ++ "nni" | x <- xs]
    (Ack,Sg) -> [x ++ "na"  | x <- xs]
    (Nom,Pl) -> [x ++  y    | x <- xs, y <- ["na", "nar"]] -- ?
    (Gen,Pl) -> [x ++ "nna" | x <- xs]
    (Dat,Pl) -> [x ++ "in"  | x <- xs]
    (Ack,Pl) -> [x ++ y     | x <- xs, y <- ["na", "nar"]]

gata_rule :: String -> NN
gata_rule gata (NN_C n c d) = species_fem d (n,c) $
    case (n,c) of
     (Sg,Nom) -> [gat++"a",gat++"æ"]
     (Sg,_)   -> [gat++"u",gat++"o"]
     (Pl,Gen) -> [gat++suff | suff <- ["na", "næ", "u","o"]]
     (Pl,Dat) -> [gat++"um", gat++"om"]
     (Pl,_)   -> [gat++suff | suff <- ["u", "ur", "o","or"]]
 where gat = tk 1 gata

kyrkia_rule :: String -> NN
kyrkia_rule kyrkia = excepts (gata_rule kyrkia) 
                     [(NN_C Pl Gen Indef, 
                       strings [kyrk++"na", kyrk++"næ", kyrk++"iu", kyrk++"io"])]
 where kyrk = tk 2 kyrkia

fru_rule :: String -> NN
fru_rule fru (NN_C n c d) = species_fem d (n,c) $ 
   case (n,c) of 
    (Sg,Nom) -> [fru,fru++"a", fru++"æ"]
    (Sg,_)   -> [fru,fru++"u",fru++"o"]
    (Pl,Gen) -> [fru]
    (Pl,Dat) -> [fru++suff | suff <- ["om","m","um"]]
    (Pl,_)   -> [fru++suff | suff <- ["o","or","ur","r",""]]

glaethi_rule :: String -> NN
glaethi_rule glaethi (NN_C n c d) =
   case (n,c) of
    (Sg,Nom) -> strings [glaethi++"n", glaeth++"en"]
    (Sg,Gen) -> strings [glaethi++"nna",glaethi++"nnar"]
    (Sg,Dat) -> strings [glaethi++"nni", glaeth++"enni"]
    (Sg,Ack) -> strings [glaethi++"na", glaeth++"ena"]
    _      -> nonExist
  where glaeth = tk 1 glaethi

-- Neutrum
species_neu :: Species -> (Number,Case) -> [String] -> Str
species_neu Indef _ xs = strings xs
species_neu Def (n,c) xs = strings $ 
   case (c,n) of 
    (Gen,Sg) -> [x ++ "ns"  | x <- xs]
    (Dat,Sg) -> [x ++ "nu" | x <- xs]
    (_,Sg)   -> [x ++ "t"  | x <- xs]
    (Gen,Pl) -> [x ++ "nna" | x <- xs]
    (_,Pl) -> [x ++ "in"  | x <- xs]

ogha_rule :: String -> NN
ogha_rule ogha (NN_C n c d) = 
   species_neu d (n,c) $
     case (n,c) of
     (Sg,_)   -> [ogh++"a"  ,ogh++"æ"]      
     (Pl,Gen) -> [ogh++"na" ,ogh++"næ"]
     (Pl,Dat) -> [ogh++"um" ,ogh++"om"]
     _        -> [ogh++"un" ,ogh++"on"]
  where ogh = tk 1 ogha

-- Noun: Strong Declension

-- Masc

fisker_rule  :: String -> NN
fisker_rule  fisker (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [fisker]
     (Sg,Gen,Indef) -> [fisk++"s"]
     (Sg,Dat,Indef) -> [fisk ++ x | x <- ["i","e",""]]
     (Sg,Ack,Indef) -> [fisk]
     (Pl,Nom,Indef) -> [fisk ++ x | x <- ["a","ar","æ","ær"]]
     (Pl,Dat,Indef) -> [fisk ++ x | x <- ["um","om"]]
     (Pl,_,Indef)   -> [fisk ++ x | x <- ["a","æ"]]
     (Sg,Nom,Def)   -> [fisk++"rin"]
     (Sg,Gen,Def)   -> [fisk++"sins"]
     (Sg,Dat,Def)   -> [fisk ++ x | x <- ["inum","enum","num"]]
     (Sg,Ack,Def)   -> [fisk++"in"]
     (Pl,Nom,Def)   -> [fisk ++ x | x <- ["ani","anir","æni","ænir"]]
     (Pl,Dat,Def)   -> [fisk ++ x | x <- ["umin","omin"]]
     (Pl,_,Def) -> [fisk ++ x | x <- ["ana","æna"]]
 where fisk = tk 2 fisker

aengil_rule  :: String -> NN
aengil_rule  aengil (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [aengil++"s"]
     (Sg,Dat,Indef) -> [aengl ++ x | x <- ["i","e"]]
     (Sg,_,Indef)   -> [aengil]
     (Pl,Nom,Indef) -> [aengl ++ x | x <- ["a","ar","æ","ær"]]
     (Pl,Dat,Indef) -> [aengl ++ x | x <- ["um","om"]]
     (Pl,_,Indef)   -> [aengl ++ x | x <- ["a","æ"]]
     (Sg,Gen,Def)   -> [aeng++"ilsins"]
     (Sg,Dat,Def)   -> [aengl ++ x | x <- ["inum","enum"]]
     (Sg,_,Def)     -> [aeng ++ x | x <- ["lin","ilin"]]
     (Pl,Nom,Def)   -> [aengl ++ x | x <- ["ani","anir","æni","ænir"]]
     (Pl,Dat,Def)   -> [aengl ++ x | x <- ["umin","omin"]]
     (Pl,_,Def)     -> [aengl ++ x | x <- ["ana","æna"]]
 where aeng = tk 2 aengil
       aengl = aeng++"l"

sko_rule     :: String -> NN
sko_rule  sko (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [sko++"s"]
     (Sg,Nom,Indef) -> [sko, sko++ "r"]
     (Sg,_,Indef)   -> [sko]
     (Pl,Nom,Indef) -> [sko ++ x | x <- ["a","ar","ar","r"]]
     (Pl,Dat,Indef) -> [sko ++ x | x <- ["um","om","m"]]
     (Pl,_,Indef)   -> [sko,sko++"a"]
     (Sg,Nom,Def)   -> [sko ++ x | x <- ["n","in","rin","rn"]]
     (Sg,Gen,Def)   -> [sko++"sins"]
     (Sg,Dat,Def)   -> [sko ++ "num"]
     (Sg,Ack,Def)   -> [sko++ x | x <- ["in","n"]]
     (Pl,Nom,Def)   -> [sko ++ x | x <- ["ani","anir","ni","nir"]]
     (Pl,Dat,Def)   -> [sko ++ x | x <- ["umin","omin","min"]]
     (Pl,_,Def)     -> [sko ++ x | x <- ["anna","nna"]]
 
aptan_rule  :: String -> NN
aptan_rule  aptan (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef)   -> [aptan ++ "s"]
     (Sg,Dat,Indef)   -> [apt   ++ x | x <- ["ni","ne"]]
     (Sg,_,Indef)     -> [aptan]
     (Pl,Nom,Indef)   -> [apt ++ x | x <- ["na","nar","næ","nær"]]
     (Pl,Dat,Indef)   -> [apt ++ x | x <- ["num","nom"]]
     (Pl,_,Indef)     -> [apt ++ x | x <- ["na","næ"]]
     (Sg,Gen,Def)     -> [apt ++ "ansins"]
     (Sg,Dat,Def)     -> [apt ++ x | x <- ["ninum","nenum"]]
     (Sg,_,Def)       -> [apt ++ x | x <- ["nin","anin"]]
     (Pl,Nom,Def)     -> [apt ++ x | x <- ["nani","nanir","næni","nænir"]]
     (Pl,Dat,Def)     -> [apt ++ x | x <- ["numin","nomin"]]
     (Pl,Gen,Def)     -> [apt ++ x | x <- ["nanna","nænna"]]
     (Pl,Ack,Def)     -> [apt ++ x | x <- ["nana","næna"]]
 where apt = tk 2 aptan

vaever_rule  :: String -> NN
vaever_rule  vaever (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [vaever]
     (Sg,Gen,Indef) -> [vaef ++ "s"]
     (Sg,_,Indef)   -> [vaef]
     (Pl,Nom,Indef) -> [vaef ++ x | x <- ["ia","iar","iæ","iær"]]
     (Pl,Dat,Indef) -> [vaef ++ x | x <- ["ium","iom"]]
     (Pl,_,Indef)   -> [vaef ++ x | x <- ["ia","iæ"]]
     (Sg,Nom,Def)   -> [vae  ++ "vrin"]
     (Sg,Gen,Def)   -> [vaef ++ "sins"]
     (Sg,Dat,Def)   -> [vaef ++"num"]
     (Sg,Ack,Def)   -> [vaef ++ "in"]
     (Pl,Nom,Def)   -> [vaef ++ x | x <- ["iani","ianir","iæni","iænir"]]
     (Pl,Dat,Def)   -> [vaef ++ x | x <- ["iumin","iomin"]]
     (Pl,Gen,Def)   -> [vaef ++ x | x <- ["ianna","iænna"]]
     (Pl,Ack,Def)   -> [vaef ++ x | x <- ["iana","iæna"]]
 where vae = tk 3 vaever
       vaef = vae++"f"

laegger_rule  :: String -> NN
laegger_rule  laegger (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [laegger]
     (Sg,Gen,Indef) -> [laegg ++ x | x <- ["ia","iar","iæ","iær"]]
     (Sg,Dat,Indef) -> [laeg  ++ x | x <- ["gi","ge",""]]
     (Sg,Ack,Indef) -> [laeg]
     (Pl,Nom,Indef) -> [laegg ++ x | x <- ["ia","iar","iæ","iær"]]
     (Pl,Dat,Indef) -> [laegg ++ x | x <- ["ium","iom"]]
     (Pl,_,Indef)   -> [laegg ++ x | x <- ["ia","iæ"]]
     (Sg,Nom,Def)   -> [laegg ++ "rin"]
     (Sg,Gen,Def)   -> [laegg ++ "sins"]
     (Sg,Dat,Def)   -> (laeg++"num"):[laegg ++ x | x <- ["inum","enum"]]
     (Sg,Ack,Def)   -> [laeg ++ "in"]
     (Pl,Nom,Def)   -> [laegg ++ x | x <- ["iani","ianir","iæni","iænir"]]
     (Pl,Dat,Def)   -> [laegg ++ x | x <- ["iumin","iomin"]]
     (Pl,_,Def)     -> [laegg ++ x | x <- ["ianna","iænna"]]
 where laeg  = tk 3 laegger
       laegg = tk 2 laegger

oeri_rule  :: String -> NN
oeri_rule oeri (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [oeri,oeri++"r"]
     (Sg,Gen,Indef) -> [oeri++"s"]
     (Sg,_,Indef)   -> [oeri,oer++"e"]
     (Pl,Nom,Indef) -> [oer ++ x | x <- ["a","ar","æ","ær"]]
     (Pl,Dat,Indef) -> [oer ++ x | x <- ["um","om"]]
     (Pl,_,Indef)   -> [oer ++ x | x <- ["a","æ"]]
     (Sg,Nom,Def)   -> [oeri ++ x | x <- ["n","rn"]]
     (Sg,Gen,Def)   -> [oeri ++ "sins"]
     (Sg,Dat,Def)   -> [oer ++ x | x <- ["inum","enum"]]
     (Sg,Ack,Def)   -> [oer ++ x | x <- ["in","en"]]
     (Pl,Nom,Def)   -> [oer ++ x | x <- ["ani","anir","æni","ænir"]]
     (Pl,Dat,Def)   -> [oer ++ x | x <- ["umin","omin"]]
     (Pl,Gen,Def)   -> [oer ++ x | x <- ["anna","ænna"]]
     (Pl,Ack,Def)   -> [oer ++ x | x <- ["ana","æna"]]
 where oer = tk 1 oeri

raetter_rule  :: String -> NN
raetter_rule raetter (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [raetter]
     (Sg,Gen,Indef) -> (rae++"z"):[raett ++ x | x <- ["a","ar","æ","ær"]]
     (Sg,Dat,Indef) -> [raet ++ x | x <- ["ti","te",""]]
     (Sg,Ack,Indef) -> [raet]
     (Pl,Nom,Indef) -> [raett ++ x | x <- ["i","ir","e","er"]]
     (Pl,Dat,Indef) -> [raett ++ x | x <- ["um","om"]]
     (Pl,_,Indef)   -> [raett ++ x | x <- ["a","æ"]]
     (Sg,Nom,Def)   -> [raett ++ "rin"]
     (Sg,Gen,Def)   -> [raett ++ "sins", rae++"zins"]
     (Sg,Dat,Def)   -> (raet++"num"):[raett ++ x | x <- ["inum","enum"]]
     (Sg,Ack,Def)   -> [raett ++ "in"]
     (Pl,Nom,Def)   -> [raett ++ x | x <- ["ini","inir"]]
     (Pl,Dat,Def)   -> [raett ++ x | x <- ["umin","omin"]]
     (Pl,Gen,Def)   -> [raett ++ x | x <- ["anna","ænna"]]
     (Pl,Ack,Def)   -> [raett ++ x | x <- ["ina","ena"]]
 where raet  = tk 3 raetter
       raett = tk 2 raetter
       rae   = tk 4 raetter

bondi_rule   :: String -> NN
bondi_rule bondi (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [bondi, bond++"e"]
     (Sg,_,Indef)   -> [bond++"a", bond++"æ"]
     (Pl,Gen,Indef) -> [bond++"a", bond++"æ"]
     (Pl,Dat,Indef) -> [bond++"um", bond++"om"]
     (Pl,_,Indef)   -> [umlaut(bond)++"er"]
     (Sg,Nom,Def)   -> [bond++"in", bond++"en"]
     (Sg,Gen,Def)   -> [bond++"sins"]
     (Sg,Dat,Def)   -> [bond++"anum",bond++"ænum"]
     (Sg,Ack,Def)   -> [bond++"in"]
     (Pl,Nom,Def)   -> [umlaut(bond)++suf | suf <- ["rini","rinir"]]
     (Pl,Dat,Def)   -> [bond ++ suf | suf <- ["umin","omin"]]
     (Pl,Gen,Def)   -> [bond ++ suf | suf <- ["anna","ænna"]]
     (Pl,Ack,Def)   -> [umlaut(bond)++"rina"]
 where bond  = tk 1 bondi


son_rule   :: String -> NN
son_rule son (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [son, s++"un"]
     (Sg,Gen,Indef) -> [son ++ "ar", son++"ær", sun++"ar", sun++"ær"]
     (Sg,Dat,Indef) -> [syn++"i", syn++"e",sun]
     (Sg,Ack,Indef) -> [syn, son,sun]
     (Pl,Nom,Indef) -> [syn++ suff | suff <- ["i","ir","e","er"]]
     (Pl,Gen,Indef) -> [son++"a", son++"æ", sun++"a", sun++"æ"]
     (Pl,Dat,Indef) -> [son++"um", sun++"um"]
     (Pl,Ack,Indef) -> [syn++"i",sun++"u"]
     (Sg,Nom,Def)   -> [son++"in", sun++"in"]
     (Sg,Gen,Def)   -> [son++"sins", sun++"sins"]
     (Sg,Dat,Def)   -> [syn++"inum",syn++"enum",sun++"um"]
     (Sg,Ack,Def)   -> [syn++"in", son++"in", sun++"in"]
     (Pl,Nom,Def)   -> [syn++"ini",syn++"inir", syn++"eni",syn++"enir"]
     (Pl,Gen,Def)   -> [son++"anna", son++"ænna",sun++"anna", sun++"ænna"]
     (Pl,Dat,Def)   -> [son++"umin",sun++"umin"]
     (Pl,Ack,Def)   -> [syn++"ina", sun++"una"]
 where s = tk 2 son
       sun = s ++ "un"
       syn = s ++ "yn"

mather_rule  :: String -> NN
mather_rule mather (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [mather, man]
     (Sg,Gen,Indef) -> [man++"z"]
     (Sg,Dat,Indef) -> [man++"ni", man++"ne"]
     (Sg,Ack,Indef) -> [man]
     (Pl,Gen,Indef) -> [man++"na", man++"næ"]
     (Pl,Dat,Indef) -> [man++"num",man++"nom"]
     (Pl,_,Indef)   -> [maen]
     (Sg,Nom,Def)   -> [math++"irin",math++"rin",man++"nin"]
     (Sg,Gen,Def)   -> [man++"zins"]
     (Sg,Dat,Def)   -> [man++"ninum",man++"nenum"]
     (Sg,Ack,Def)   -> [man++"nin"]
     (Pl,Nom,Def)   -> [maen++"ni",maen++"nir"]
     (Pl,Gen,Def)   -> [man++"nanna", man++"nænna"]
     (Pl,Dat,Def)   -> [man++"numin",man++"nomin"]
     (Pl,Ack,Def)   -> [maen++"na"]
 where math = tk 2 mather
       man  = tk 3 mather ++ "n"
       maen = tk 3 mather ++ "æn"

foter_rule   :: String -> NN
foter_rule foter (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [foter]
     (Sg,Gen,Indef) -> [fot++suff | suff <- ["a","ar","æ","ær"]]
     (Sg,Dat,Indef) -> [foet++"i",foet++"e",fot++"i",fot++"e"]
     (Sg,Ack,Indef) -> [fot]
     (Pl,Gen,Indef) -> [fot++"a",fot++"æ"]
     (Pl,Dat,Indef) -> [fot++"um",fot++"om"]
     (Pl,_,Indef)   -> [foet++"er"]
     (Sg,Nom,Def)   -> [fot++"rin"]
     (Sg,Gen,Def)   -> [fot++"sins"]
     (Sg,Dat,Def)   -> [foet++"inum",foet++"enum",fot++"inum",fot++"enum"]
     (Sg,Ack,Def)   -> [fot++"in"]
     (Pl,Nom,Def)   -> [foet++"rini",foet++"rinir"]
     (Pl,Gen,Def)   -> [fot++"anna", fot++"ænna"]
     (Pl,Dat,Def)   -> [fot++"umin", fot++"omin"]
     (Pl,Ack,Def)   -> [foet++"rina"]
 where fot  = tk 2 foter
       foet = umlaut fot
 
fathir_rule  :: String -> NN
fathir_rule fathir (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [fathir]
     (Sg,Gen,Indef) -> [fath++"ur",fath++"urs",fath++"or",fath++"ors"]
     (Sg,Dat,Indef) -> [fath++"ur",fath++"or",faeth++"er"]
     (Sg,Ack,Indef) -> [fath++"ur",fath++"or"]
     (Pl,Gen,Indef) -> [faeth++"ra",faeth++"ræ"]
     (Pl,Dat,Indef) -> [faeth++"rum",faeth++"rom"]
     (Pl,_,Indef)   -> [faeth++"er"]
     (Sg,Nom,Def)   -> [fath++"irin",fath++"rin"]
     (Sg,Gen,Def)   -> [fath++"ursins",fath++"urins",fath++"orsins",fath++"orins"]
     (Sg,Dat,Def)   -> [fath++"urnum",fath++"ornum",faeth++"ernum"]
     (Sg,Ack,Def)   -> [fath++"urin",fath++"rin"]
     (Pl,Nom,Def)   -> [faeth++"rini",faeth++"rinir"]
     (Pl,Gen,Def)   -> [faeth++"ranna",faeth++"rænna"]
     (Pl,Dat,Def)   -> [faeth++"rumin",faeth++"romin"]
     (Pl,Ack,Def)   -> [faeth++"rina"]
 where fath  = tk 2 fathir
       faeth = umlaut fath

eghandi_rule :: String -> NN
eghandi_rule eghandi (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [eghand++"i",eghand++"e"]
     (Sg,_,Indef)   -> [eghand++"a",eghand++"æ"]
     (Pl,Gen,Indef) -> [eghand++"a",eghand++"æ"]
     (Pl,Dat,Indef) -> [eghand++"um",eghand++"om"]
     (Pl,_,Indef)   -> [eghand++"er",eghand++"ar"]
     (Sg,Nom,Def)   -> [eghand++"in",eghand++"en"]
     (Sg,Gen,Def)   -> [eghand++"sins"]
     (Sg,Dat,Def)   -> [eghand++"anum",eghand++"ænum"]
     (Sg,Ack,Def)   -> [eghand++"in"]
     (Pl,Nom,Def)   -> [eghand++"inir",eghand++"ini"]
     (Pl,Gen,Def)   -> [eghand++"anna",eghand++"ænna"]
     (Pl,Dat,Def)   -> [eghand++"umin",eghand++"omin"]
     (Pl,Ack,Def)   -> [eghand++"ina"]
 where eghand = tk 1 eghandi

-- Fem.

agn_rule :: String -> NN
agn_rule agn (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [agn ++ x | x <- ["a","ar","æ","ær"]]
     (Sg,_,Indef)   -> [agn]
     (Pl,Dat,Indef) -> [agn ++ x | x <- ["um","om"]]
     (Pl,Gen,Indef) -> [agn ++ x | x <- ["a","æ"]]
     (Pl,_,Indef)   -> [agn ++ x | x <- ["a","ar","æ","ær"]]
     (Sg,Nom,Def)   -> [agn ++ "in"]
     (Sg,Gen,Def)   -> [agn ++ x | x <- ["inna","innar"]]
     (Sg,Dat,Def)   -> [agn ++ "inni"]
     (Sg,Ack,Def)   -> [agn ++ "ina"]
     (Pl,Gen,Def)   -> [agn ++ x | x <- ["anna","ænna"]]
     (Pl,Dat,Def)   -> [agn ++ x | x <- ["umin","omin"]]
     (Pl,_,Def)   -> [agn ++ x | x <- ["ana","anar","æna","ænar"]]

bro_rule :: String -> NN
bro_rule bro (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [bro ++ x | x <- ["a","ar","æ","ær","r",""]]
     (Sg,_,Indef)   -> [bro]
     (Pl,Dat,Indef) -> [bro ++ x | x <- ["um","om","m"]]
     (Pl,Gen,Indef) -> [bro ++ x | x <- ["a","æ",""]]
     (Pl,_,Indef)   -> [bro ++ x | x <- ["a","ar","æ","ær","r",""]]
     (Sg,Nom,Def)   -> [bro ++ x | x <- ["in", "n"]]
     (Sg,Gen,Def)   -> [bro ++ x | x <- ["inna","nna","innar","nnar"]]
     (Sg,Dat,Def)   -> [bro ++ x | x <- ["inni","nni"]]
     (Sg,Ack,Def)   -> [bro ++ x | x <- ["ina","na"]]
     (Pl,Gen,Def)   -> [bro ++ x | x <- ["anna","ænna","nna"]]
     (Pl,Dat,Def)   -> [bro ++ x | x <- ["umin","omin","min"]]
     (Pl,_,Def)     -> [bro ++ x | x <- ["ana","anar","æna","ænar","nar","rna","rnar","na","nar"]]

aeg_rule :: String -> NN
aeg_rule  aeg (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [aeg ++ suf | suf <- ["gia","giar","giæ","giær"]]
     (Sg,_,Indef)   -> [aeg]
     (Pl,Dat,Indef) -> [aeg ++ suf | suf <- ["gium","giom"]]
     (Pl,Gen,Indef) -> [aeg ++ suf | suf <- ["gia","giæ"]]
     (Pl,_,Indef)   -> [aeg ++ suf | suf <- ["gia","giar","giæ","giær"]]
     (Sg,Nom,Def)   -> [aeg ++ suf | suf <- ["gin"]]
     (Sg,Gen,Def)   -> [aeg ++ suf | suf <- ["inna","innar"]]
     (Sg,Dat,Def)   -> [aeg ++ suf | suf <- ["ginni"]]
     (Sg,Ack,Def)   -> [aeg ++ suf | suf <- ["gina"]]
     (Pl,Gen,Def)   -> [aeg ++ suf | suf <- ["gianna","giænna"]]
     (Pl,Dat,Def)   -> [aeg ++ suf | suf <- ["giumin","giomin"]]
     (Pl,_,Def)     -> [aeg ++ suf | suf <- ["iana","ianar","iæna","iænar"]]

heth_rule :: String -> NN
heth_rule  heth (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [heth]
     (Sg,Gen,Indef) -> [heth ++ suf | suf <- ["a","ar","æ","ær"]]
     (Sg,_,Indef)   -> [heth ++ suf | suf <- ["i","e",""]]
     (Pl,Dat,Indef) -> [heth ++ suf | suf <- ["um","om"]]
     (Pl,Gen,Indef) -> [heth ++ suf | suf <- ["a","æ"]]
     (Pl,_,Indef)   -> [heth ++ suf | suf <- ["a","ar","æ","ær"]]
     (Sg,Nom,Def)   -> [heth ++ suf | suf <- ["in"]]
     (Sg,Gen,Def)   -> [heth ++ suf | suf <- ["inna","innar"]]
     (Sg,Dat,Def)   -> [heth ++ suf | suf <- ["inni"]]
     (Sg,Ack,Def)   -> [heth ++ suf | suf <- ["ina"]]
     (Pl,Gen,Def)   -> [heth ++ suf | suf <- ["anna","ænna"]]
     (Pl,Dat,Def)   -> [heth ++ suf | suf <- ["umin","omin"]]
     (Pl,_,Def)     -> [heth ++ suf | suf <- ["ana","anar","æna","ænar"]]

faerth_rule :: String -> NN
faerth_rule  faerth (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [faerth ++ suf | suf <- ["a","ar","æ","ær"]]
     (Sg,_,Indef)   -> [faerth]
     (Pl,Dat,Indef) -> [faerth ++ suf | suf <- ["um","om"]]
     (Pl,Gen,Indef) -> [faerth ++ suf | suf <- ["a","æ"]]
     (Pl,_,Indef)   -> [faerth ++ suf | suf <- ["i","ir","e","er"]]
     (Sg,Nom,Def)   -> [faerth ++ "in"]
     (Sg,Gen,Def)   -> [faerth ++ suf | suf <- ["inna","innar"]]
     (Sg,Dat,Def)   -> [faerth ++ suf | suf <- ["inni"]]
     (Sg,Ack,Def)   -> [faerth ++ suf | suf <- ["ina"]]
     (Pl,Gen,Def)   -> [faerth ++ suf | suf <- ["anna","ænna"]]
     (Pl,Dat,Def)   -> [faerth ++ suf | suf <- ["umin","omin"]]
     (Pl,_,Def)     -> [faerth ++ suf | suf <- ["ina","inar","ena","enar"]]

gas_rule :: String -> NN
gas_rule  gas (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [gas ++ suf | suf <- ["a","ar"]]
     (Sg,_,Indef)   -> [gas]
     (Pl,Dat,Indef) -> [gas ++ suf | suf <- ["um"]]
     (Pl,Gen,Indef) -> [gas ++ suf | suf <- ["a"]]
     (Pl,_,Indef)   -> [umlaut(gas)]
     (Sg,Nom,Def)   -> [gas ++ "in"]
     (Sg,Gen,Def)   -> [gas ++ suf | suf <- ["inna","innar"]]
     (Sg,Dat,Def)   -> [gas ++ "inni"]
     (Sg,Ack,Def)   -> [gas ++ "ina"]
     (Pl,Gen,Def)   -> [gas ++ "anna"]
     (Pl,Dat,Def)   -> [gas ++ "umin"]
     (Pl,_,Def)     -> [umlaut(gas) ++ suf | suf <- ["ina","inar"]]

bok_rule :: String -> NN
bok_rule bok (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [bok++"a",bok++"ar"]
     (Sg,_,Indef)   -> [bok]
     (Pl,Gen,Indef) -> [bok++"a"]
     (Pl,Dat,Indef) -> [bok++"um",bok++"om"]
     (Pl,_,Indef)   -> [boek++"er"]
     (Sg,Nom,Def)   -> [bok++"in"]
     (Sg,Gen,Def)   -> [bok++"inna",bok++"innar"]
     (Sg,Dat,Def)   -> [bok++"inni"]
     (Sg,Ack,Def)   -> [bok++"ina"]
     (Pl,Nom,Def)   -> [boek++"enar",boek++"ena"]
     (Pl,Gen,Def)   -> [bok++"anna"]
     (Pl,Dat,Def)   -> [bok ++ suf | suf <- ["umin","omin"]]
     (Pl,Ack,Def)   -> [boek++"enar"]
 where boek = umlaut bok

ko_rule :: String -> NN
ko_rule ko (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [ko++"a",ko++"ar",ko++"r"]
     (Sg,_,Indef)   -> [ko]
     (Pl,Gen,Indef) -> [ko,ko++"a",ko++"æ"]
     (Pl,Dat,Indef) -> [ko++"um",ko++"m",ko++"om"]
     (Pl,_,Indef)   -> [koe,koe++"r",ko,ko++"r"]
     (Sg,Nom,Def)   -> [ko++"in",ko++"n"]
     (Sg,Gen,Def)   -> [ko ++ suf | suf <- ["innar","nnar", "nna", "inna"]]
     (Sg,Dat,Def)   -> [ko++"inni",ko++"nni"]
     (Sg,Ack,Def)   -> [ko++"ina", ko++"na"]
     (Pl,Gen,Def)   -> [ko++"anna"]
     (Pl,Dat,Def)   -> [ko++"umin", ko++"omin", ko++"min"]
     (Pl,_,Def)     -> [koe++"na",koe++"nar",ko++"na", ko++"nar"]
 where koe = umlaut ko

mothir_rule  :: String -> NN
mothir_rule mothir (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Nom,Indef) -> [mothir]
     (Sg,Gen,Indef) -> [moth++"ur",moth++"urs",moth++"or",moth++"ors"]
     (Sg,Dat,Indef) -> [moth++"ur",moth++"or",myth++"r"]
     (Sg,Ack,Indef) -> [moth++"ur", moth++"or"]
     (Pl,Gen,Indef) -> [moeth++"ra", moeth++"ræ"]
     (Pl,Dat,Indef) -> [moeth++"rum",moeth++"rom"]
     (Pl,_,Indef)   -> [moeth++"er"]
     (Sg,Nom,Def)   -> [moth++"irin"]
     (Sg,Gen,Def)   -> [moth ++ suf | suf <- ["urinnar","urinna","orinnar","orinna"]]
     (Sg,Dat,Def)   -> [moth++"urinni",moth++"orinni", myth ++ "rinni"]
     (Sg,Ack,Def)   -> [moth++suff | suff <- ["urina","urna", "orina", "orna"]]
     (Pl,Gen,Def)   -> [moeth++"ranna",moeth++"rænna"]
     (Pl,Dat,Def)   -> [moeth++"rumin",moeth++"romin"]
     (Pl,_,Def)     -> [moeth++"rinar",moeth++"rina"]
 where moth  = tk 2 mothir
       myth  = tk 4 mothir ++ "yþ"
       moeth = umlaut moth

-- Neut.

skip_rule  :: String -> NN
skip_rule skip (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [skip++"s"]
     (Sg,Dat,Indef) -> [skip ++ x | x <- ["i","e",""]]
     (Sg,_,Indef)   -> [skip]
     (Pl,Gen,Indef) -> [skip ++ x | x <- ["a","æ"]]
     (Pl,Dat,Indef) -> [skip ++ x | x <- ["um","om"]]
     (Pl,_,Indef)   -> [skip]
     (Sg,Gen,Def)   -> [skip++"sins"]
     (Sg,Dat,Def)   -> [skip ++ x | x <- ["inu","enu","nu"]]
     (Sg,_,Def)     -> [skip ++ "it"]
     (Pl,Dat,Def)   -> [skip ++ x | x <- ["umin","omin"]]
     (Pl,Gen,Def)   -> [skip ++ x | x <- ["anna","ænna"]]
     (Pl,_,Def)     -> [skip ++ "in"]

bo_rule  :: String -> NN
bo_rule bo (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [bo++"s"]
     (Sg,_,Indef)   -> [bo]
     (Pl,Gen,Indef) -> [bo ++ x | x <- ["a",""]]
     (Pl,Dat,Indef) -> [bo ++ "m"]
     (Pl,_,Indef)   -> [bo]
     (Sg,Gen,Def)   -> [bo++"sins"]
     (Sg,Dat,Def)   -> [bo ++ "no"]
     (Sg,_,Def)     -> [bo ++ x | x <- ["t","it"]]
     (Pl,Dat,Def)   -> [bo ++ "min"]
     (Pl,Gen,Def)   -> [bo ++ x | x <- ["anna","nna"]]
     (Pl,_,Def)     -> [bo ++ x | x <- ["in","n"]]


hovuth_rule  :: String -> NN
hovuth_rule hovuth (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [hov++"uz", huv++"uz"]
     (Sg,Dat,Indef) -> [hof++"þe", hoef++"þi", hof++"þi"]
     (Sg,_,Indef)   -> [hov++"uþ",huv++"uþ"]
     (Pl,Gen,Indef) -> [hof++"þa",hoef++"þa"]
     (Pl,Dat,Indef) -> [hof++"þom",hof++"þum",hoef++"þum"]
     (Pl,_,Indef)   -> [hov++"uþ",huv++"uþ"]
     (Sg,Gen,Def)   -> [hov++"uzins",huv++"uzins"]
     (Sg,Dat,Def)   -> [hof++"þenu",hoef++"inu",hof++"þinu"]
     (Sg,_,Def)     -> [hov++"uþit",huv++"uþit"]
     (Pl,Gen,Def)   -> [hof++"þanna",hoef++"þanna"]
     (Pl,Dat,Def)   -> [hof++"þomin",hof++"þumin",hoef++"þumin"]
     (Pl,_,Def)     -> [hov++"uþin",huv++"uþin"]
  where hov  = tk 2 hovuth
        huv  = tk 2 hov ++ "uv"
        hof  = tk 1 hov ++ "f"
        hoef = umlaut hof

trae_rule :: String -> NN
trae_rule trae (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [trae++"s"]
     (Sg,_,Indef)   -> [trae]
     (Pl,Gen,Indef) -> [trae++"a",trae]
     (Pl,Dat,Indef) -> [trae++"um",trae++"m"]
     (Pl,_,Indef)   -> [trae]
     (Sg,Gen,Def)   -> [trae++"sins"]
     (Sg,Dat,Def)   -> [trae++"no"]
     (Sg,_,Def)     -> [trae ++ x | x <- ["t","it"]]
     (Pl,Gen,Def)   -> [trae ++ x | x <- ["anna","nna"]]
     (Pl,Dat,Def)   -> [trae++"umin",trae++"min"] 
     (Pl,_,Def)     -> [trae ++ x | x <- ["in","n"]]

skaer_rule   :: String -> NN
skaer_rule skaer (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [skaer++"s"]
     (Sg,Dat,Indef) -> [skaer++"i"]
     (Sg,_,Indef)   -> [skaer]
     (Pl,Gen,Indef) -> [skaer++"ia"]
     (Pl,Dat,Indef) -> [skaer++"iom",skaer++"ium"]
     (Pl,_,Indef)   -> [skaer]
     (Sg,Gen,Def)   -> [skaer++"ins"]
     (Sg,Dat,Def)   -> [skaer++"inu"]
     (Sg,_,Def)     -> [skaer++"it"]
     (Pl,Gen,Def)   -> [skaer++"ianna"]
     (Pl,Dat,Def)   -> [skaer++"iomin",skaer++"iumin"]
     (Pl,_,Def)     -> [skaer++"in"]

minne_rule   :: String -> NN
minne_rule minne (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [minn++"is"]
     (Sg,_,Indef)   -> [minne]
     (Pl,Gen,Indef) -> [minn++"a"]
     (Pl,Dat,Indef) -> [minn++"om"]
     (Pl,_,Indef)   -> [minne]
     (Sg,Gen,Def)   -> [minn++"isins"]
     (Sg,Dat,Def)   -> [minne++"no"]
     (Sg,_,Def)     -> [minne++"t"]
     (Pl,Gen,Def)   -> [minn++"anna"]
     (Pl,Dat,Def)   -> [minn++"omin"]
     (Pl,_,Def)     -> [minne++"n"]
  where minn = tk 1 minne

aepli_rule   :: String -> NN
aepli_rule aepli (NN_C n c d) = 
   strings $
    case (n,c,d) of
     (Sg,Gen,Indef) -> [aepli++"s"]
     (Sg,_,Indef)   -> [aepli]
     (Pl,Gen,Indef) -> [aepl++"a"]
     (Pl,Dat,Indef) -> [aepl++"om"]
     (Pl,_,Indef)   -> [aepli]
     (Sg,Gen,Def)   -> [aepl++"isins"]
     (Sg,Dat,Def)   -> [aepl++"inu"]
     (Sg,_,Def)     -> [aepl++"it"]
     (Pl,Nom,Def)   -> [aepl++"in"]
     (Pl,Gen,Def)   -> [aepl++"anna"]
     (Pl,Dat,Def)   -> [aepl++"umin"]
     (Pl,_,Def)     -> [aepl++"it"]
  where aepl = tk 1 aepli

-- Adjective
-- strong
{-
 helagher_rule :: String -> AV
 mither_rule   :: String -> AV 
 gamal_rule    :: String -> AV
 kristin_rule  :: String -> AV
-}

comparative :: (Gender,Number,Case) -> String -> [String]
comparative (g,n,c) blar = 
  case (g,n,c) of
   (Fem,Sg,_)    -> [blar++"e",blar++"i"]
   (Masc,Sg,Nom) -> [blar++"e",blar++"i"]
   (_,Sg,_)      -> [blar++"e",blar++"i",blar++"a"]
   (_,Pl,Gen)    -> [blar++"a"]
   (_,Pl,Dat)      -> [blar++"e",blar++"i",blar++"om",blar++"um"]
   (_,Pl,_)      -> [blar++"e",blar++"i"]

blar_rule     :: String -> AV
blar_rule blar para =
 strings $ 
  case para of
   (StrongPos g n c) -> blar_strong_rule blar (g,n,c)
   (WeakPos g n c)   -> blar_weak_rule  blar (g,n,c)
   (Comp g n c)      -> comparative (g,n,c) blar
   (Super)           -> [blar++"er"]

langer_rule     :: String -> AV
langer_rule langer para =
 strings $ 
  case para of
   (StrongPos g n c) -> langer_strong_rule langer (g,n,c)
   (WeakPos g n c)   -> langer_weak_rule  langer (g,n,c)
   (Comp g n c)      -> comparative (g,n,c) (laeng ++ "r")
   (Super)           -> [laeng++"ster"]
 where lang  = tk 2 langer
       laeng = changeVowel "æ" lang

helagher_rule :: String -> AV
helagher_rule helagher para =
 strings $ 
  case para of
   (StrongPos g n c) -> helagher_strong_rule helagher (g,n,c)
   (WeakPos g n c)   -> langer_weak_rule  helagher (g,n,c)
   (Comp g n c)      -> comparative (g,n,c) (helagh ++ "r")
   (Super)           -> [helagh++"ster"]
 where helagh  = tk 2 helagher

blar_strong_rule :: String -> (Gender,Number,Case) -> [String]
blar_strong_rule blar para =
 case para of
  (Masc,Sg,Nom) -> [bla++"r",bla]
  (Masc,Sg,Gen) -> [bla++"s"]
  (Masc,Sg,Dat) -> [bla++suff | suff <- ["om","m","r","ur"]]
  (Masc,Sg,Ack) -> [bla++"n"]
  (Masc,Pl,Nom)   -> [bla++suff | suff <- ["","ir","r"]]
  (Fem,Sg,Gen)  -> [bla++"r",bla]
  (Fem,Sg,Dat)  -> [bla++"e",bla]
  (Fem,Pl,Nom)    -> [bla++"r",bla]
  (Fem,Pl,Ack)    -> [bla++"r",bla]
  (Neut,Sg,Gen) -> [bla++"s"]
  (Neut,Sg,Dat) -> [bla ++ suff | suff <- ["","o","u"]]
  (Neut,Sg,_)   -> [bla++"t", bla++"tt"]
  (_,Pl,Dat)      -> [bla++suff | suff <- ["om","m","um"]]
  _               ->  [bla]
 where bla = tk 1 blar

blar_weak_rule :: String -> (Gender,Number,Case) -> [String]
blar_weak_rule blar para = 
 case para of
  (Masc,Sg,Nom) -> [bla++suff | suff <- ["","e","i"]]
  (Masc,Sg,_)     -> [bla]
  (Masc,Pl,_)     -> blao_blau
  (Fem,Sg,Nom)    -> [bla]
  (Fem,Sg,_)      -> blao_blau
  (Fem,Pl,_)      -> blao_blau
  (Neut,Sg,_)     -> [bla]
  (Neut,Pl,_)     -> blao_blau
 where blao_blau = [bla++suff | suff <- ["","o","u"]]
       bla       = tk 1 blar

langer_weak_rule :: String -> (Gender,Number,Case) -> [String]
langer_weak_rule langer para =
    case para of
     (Fem,Sg,Nom)    -> langa
     (Fem,_,_)       -> lango_langu
     (Masc,Sg,Nom)   -> [lang++x | x <- ["i","e"]]
     (Masc,Sg,_)     -> langa
     (Masc,Pl,_)     -> lango_langu
     (Neut,Sg,_)     -> langa
     (Neut,Pl,_)     -> lango_langu
  where lang        = tk 2 langer
        lango_langu = [lang++ x | x <- ["o","u"]]
        langa       = [lang++"a"]

langer_strong_rule :: String -> (Gender,Number,Case) -> [String]
langer_strong_rule langer para =
 case para of
  (Masc,Sg,Nom)   -> [langer]
  (Masc,Sg,Gen)   -> [lang++"s"]
  (Masc,Sg,Dat)   -> [lang++"om",lang++"um"]
  (Masc,Sg,Ack)   -> [lang++"an", lang++"æn"]
  (Masc,Pl,Nom)   -> [lang++suff | suff <- ["i","e","ir","er"]]
  (Masc,Pl,Ack)   -> [lang++"a",lang++"æ"]
  (Fem,Sg,Nom)    -> [lang]
  (Fem,Sg,Gen)    -> [lang++suff | suff <- ["a","ra","ar","rar",                                            "æ","ræ","ær","rær"]]
  (Fem,Sg,Dat)    -> [lang++suff | suff <- ["i","ri","e","re"]]
  (Fem,Sg,Ack)    -> [lang++"a", lang++"æ"]
  (Fem,Pl,Nom)    -> [lang++suff | suff <- ["a","ar","æ","ær"]]
  (Fem,Pl,Ack)    -> [lang++suff | suff <- ["a","ar","æ","ær"]]
  (Neut,Sg,Gen)   -> [lang++"s"]
  (Neut,Sg,Dat)   -> [lang++"o",lang++"u"]
  (Neut,Sg,_)     -> [lang++"t"]
  (Neut,Pl,Nom)   -> [lang]
  (Neut,Pl,Ack)   -> [lang]
  (_,Pl,Gen)      -> [lang++suff | suff <- ["a","ra","æ","ræ"]]
  (_,Pl,Dat)      -> [lang++suff | suff <- ["om","m","um"]]
 where lang = tk 2 langer

helagher_strong_rule :: String -> (Gender,Number,Case) -> [String]
helagher_strong_rule helagher para =
 case para of
  (Masc,Sg,Nom)   -> [helagher]
  (Masc,Sg,Gen)   -> [hela++"x"]
  (Masc,Sg,Dat)   -> [haelgh++"om",haelgh++"um"]
  (Masc,Sg,Ack)   -> [haelgh++"an", haelgh++"æn"]
  (Masc,Pl,Nom)   -> [haelgh++suff | suff <- ["i","e","ir","er"]]
  (Masc,Pl,Ack)   -> [haelgh++"a",haelgh++"æ"]
  (Fem,Sg,Nom)    -> [helagh]
  (Fem,Sg,Gen)    -> [haelgh++suff | suff <- ["a","ra","ar","rar",
                                              "æ","ræ","ær","rær"]]
  (Fem,Sg,Dat)    -> [haelgh++suff | suff <- ["i","ri","e","re"]]
  (Fem,Sg,Ack)    -> [haelgh++"a", haelgh++"æ"]
  (Fem,Pl,Nom)    -> [haelgh++suff | suff <- ["a","ar","æ","ær"]]
  (Fem,Pl,Ack)    -> [haelgh++suff | suff <- ["a","ar","æ","ær"]]
  (Neut,Sg,Gen)   -> [hela++"x"]
  (Neut,Sg,Dat)   -> [haelgh++"o",haelgh++"u"]
  (Neut,Sg,_)     -> [hela++"kt"]
  (Neut,Pl,Nom)   -> [helagh]
  (Neut,Pl,Ack)   -> [helagh]
  (_,Pl,Gen)      -> [haelgh++suff | suff <- ["a","ra","æ","ræ"]]
  (_,Pl,Dat)      -> [haelgh++suff | suff <- ["om","m","um"]]
 where helagh = tk 2 helagher
       hela   = tk 2 helagh
       haelgh = changeVowel "æ" (tk 1 hela) ++ "gh"

-------------------------------------------------
-- Adverb

opta_rule :: String -> Adverb
opta_rule opta (AdverbForm g) = 
    mkStr $ case g of
              Posit  -> opta  
              Compar -> opta++"r"
              Superl -> opta++"st"    


laenger_rule :: String -> Adverb
laenger_rule laenge (AdverbForm g) = 
    mkStr $ case g of
              Posit  -> laenge  
              Compar -> laenge++"r"
              Superl -> (tk 1 laenge)++"st"    

yter_rule :: String -> Adverb
yter_rule yter (AdverbForm g) = 
    mkStr $ case g of
              Posit  -> yter  
              Compar -> yter++"mer"
              Superl -> yter++"st"    

-------------------------------------------------

ord_fyrsti_rule :: String -> Ordinal
ord_fyrsti_rule  fyrsti (OrdF g n c) =
  strings $
    case (g,n,c) of
     (Masc,Sg,Nom)    -> [fyrsti]
     (Masc,Sg,_)      -> [fyrsti++"a"]
     (Fem,Sg,Nom)     -> [fyrsti++"a",fyrsti]
     (Neut,Sg,_)      -> [fyrsti++"a"]
     _                -> [fyrsti++"u"]

ord_annar_rule :: Ordinal
ord_annar_rule (OrdF g n c) =
  strings $
    case (g,n,c) of
     (Masc,Sg,Nom)    -> ["annar","annan"]
     (Masc,Sg,Gen)    -> ["annars"]
     (Masc,Sg,Ack)    -> ["annan"]
     (Masc,Sg,Dat)    -> ["aþrum","aþrom","andrum","androm"]
     (Masc,Pl,Nom)    -> ["aþri","aþrir","andri","andrir"]
     (Masc,Pl,Ack)    -> ["aþra","andra"]
     (Fem,Sg,Nom)   -> ["annur","annor"]
     (Fem,Sg,Gen)   -> ["annara","annarra", "annarar","annarrar"]
     (Fem,Sg,Dat)   -> ["annari","annarri"]
     (Fem,Sg,Ack)   -> ["aþra","andra"]
     (Fem,Pl,Nom)     -> ["aþra","andra","aþrar","andrar"]
     (Fem,Pl,Ack)     -> ["aþrar","andra","andrar"]
     (Neut,Sg,Nom)    -> ["annat"]
     (Neut,Sg,Gen)    -> ["annars"]
     (Neut,Sg,Dat)    -> ["aþru","aþro","andru","andro"]
     (Neut,Sg,Ack)    -> ["annat"]
     (Neut,Pl,Nom)    -> ["annur","annor"]
     (Neut,Pl,Ack)    -> ["annur","annor"]
     (_,Pl,Gen)       -> ["annara","annarra","andra"]
     (_,Pl,Dat)       -> ["aþrum","aþrom","andrum","androm"]

-- Pronomen

sin_rule :: String -> Pronoun
sin_rule sin (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> [sin++"na"]
  (_,Pl,Dat)    -> [sin++"um",sin++"om"]
  (Masc,Sg,Gen) -> [sin++"s"]
  (Masc,Sg,Dat) -> [sin++"um",sin++"om"]
  (Masc,Sg,_)   -> [sin]
  (Masc,Pl,Nom) -> [sin++"i",sin++"ir"]
  (Masc,Pl,Ack) -> [sin++"a"]
  (Fem,Sg,Nom)  -> [sin]
  (Fem,Sg,Gen)  -> [sin++"na",sin++"nar"]
  (Fem,Sg,Dat)  -> [sin++"ni",sin++"ne"]
  (Fem,Sg,Ack)  -> [sin++"a"]
  (Fem,Pl,_)    -> [sin++"a",sin++"ar"]
  (Neut,Sg,Gen) -> [sin++"s"]
  (Neut,Sg,Dat) -> [sin++"u",sin++"o"]
  (Neut,Sg,_)   -> [si ++ "t"]
  (Neut,Pl,_)   -> [sin]
 where si = tk 1 sin

var_rule :: Pronoun
var_rule (PNF g n c) = 
 case (g,n,c) of
  (Masc,Sg,Ack) -> strings ["van", "varn"]
  (_,Pl,Gen)    -> strings ["varra"]
  (Fem,Sg,Gen)  -> strings ["varra","varrar"]
  (Neut,Sg,Nom) -> strings ["vart"]
  (Neut,Sg,Ack) -> strings ["vart"]
  _             -> sin_rule "var" (PNF g n c)

ithar_rule :: Pronoun
ithar_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> ["iþra"]
  (_,Pl,Dat)    -> ["iþrum","iþrom"]
  (Masc,Sg,Nom) -> ["iþar"]
  (Masc,Sg,Gen) -> ["iþars"]
  (Masc,Sg,Dat) -> ["iþrum","iþrom"]
  (Masc,Sg,Ack) -> ["iþan"]
  (Masc,Pl,Nom) -> ["iþri", "iþrir"]
  (Masc,Pl,Ack) -> ["iþra"]
  (Fem,Sg,Nom)  -> ["iþur"]
  (Fem,Sg,Gen)  -> ["iþra","iþrar"]
  (Fem,Sg,Dat)  -> ["iþre"]
  (Fem,Sg,Ack)  -> ["iþra"]
  (Fem,Pl,_)    -> ["iþra","iþrar"]
  (Neut,Sg,Nom) -> ["iþart"]
  (Neut,Sg,Gen) -> ["iþars"]
  (Neut,Sg,Dat) -> ["iþru","iþro"]
  (Neut,Sg,Ack) -> ["iþart"]
  (Neut,Pl,_)   -> ["iþur"]

thaen_rule :: Pronoun
thaen_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> ["þera", "þerra", "þæra", "þærra"]
  (_,Pl,Dat)    -> ["þem","þom","þøm"]
  (Masc,Sg,Nom) -> ["þæn"]
  (Masc,Sg,Gen) -> ["þæs"]
  (Masc,Sg,Dat) -> ["þem","þæm","þøm"]
  (Masc,Sg,Ack) -> ["þæn","þen"]
  (Masc,Pl,Nom) -> ["þe", "þer"]
  (Masc,Pl,Ack) -> ["þa","þe"]
  (Fem,Sg,Nom)  -> ["þe","þøn"]
  (Fem,Sg,Gen)  -> ["þera","þærra"]
  (Fem,Sg,Dat)  -> ["þeri","þere","þerre"]
  (Fem,Sg,Ack)  -> ["þa"]
  (Fem,Pl,_)    -> ["þa","þar","þæ","þær","þe","þer"]
  (Neut,Sg,Nom) -> ["þæt"]
  (Neut,Sg,Gen) -> ["þæs"]
  (Neut,Sg,Dat) -> ["þy","þi","þe"]
  (Neut,Sg,Ack) -> ["þæt"]
  (Neut,Pl,_)   -> ["þøn","þe"]

thaeni_rule :: Pronoun
thaeni_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> ["þæssa"]
  (_,Pl,Dat)    -> ["þæssum","þæmma","þænna"]
  (Masc,Sg,Nom) -> ["þænni","þænne"]
  (Masc,Sg,Gen) -> ["þæssa"]
  (Masc,Sg,Dat) -> ["þæssum","þæssom","þæmma","þænna"]
  (Masc,Sg,Ack) -> ["þænna"]
  (Masc,Pl,Nom) -> ["þæssi", "þæssir"]
  (Masc,Pl,Ack) -> ["þæssa"]
  (Fem,Sg,Nom)  -> ["þæssi","þæsse"]
  (Fem,Sg,Gen)  -> ["þæssa"]
  (Fem,Sg,Dat)  -> ["þæssi","þæsse"]
  (Fem,Sg,Ack)  -> ["þæssa","þænna"]
  (Fem,Pl,_)    -> ["þæssa","þæssar"]
  (Neut,Sg,Nom) -> ["þætta"]
  (Neut,Sg,Gen) -> ["þæssa"]
  (Neut,Sg,Dat) -> ["þæssu","þæsso"]
  (Neut,Sg,Ack) -> ["þætta"]
  (Neut,Pl,_)   -> ["þæssi","þæssin","þænne","þæsson","þæssa","þæssom"]


hvar_rule :: Pronoun
hvar_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
   (_,Pl,_)      -> []
   (Fem,Sg,_)    -> []
   (Masc,Sg,Nom) -> ["hva","hvar","ha","har"]
   (Masc,Sg,Gen) -> ["hvas","hvæs","hves","hvess"]
   (Masc,Sg,Dat) -> ["hvem"]
   (Masc,Sg,Ack) -> ["hvan","hvem","hven"]
   (Neut,Sg,Nom) -> ["hvat","hvadh"]
   (Neut,Sg,Gen) -> ["hvas","hvæs"]
   (Neut,Sg,Dat) -> ["hvi"]
   (Neut,Sg,Ack) -> ["hvat","hvadh"]
 
hvilkin_rule :: Pronoun
hvilkin_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> ["hvilika","hvilikra"]
  (_,Pl,Dat)    -> ["hvilikum","hvilikom"]
  (Masc,Sg,Nom) -> ["hvilikin"]
  (Masc,Sg,Gen) -> ["hvilikins"]
  (Masc,Sg,Dat) -> ["hvilikom","hvilikum"]
  (Masc,Sg,Ack) -> ["hvilikin"]
  (Masc,Pl,Nom) -> ["hviliki","hvilike"]
  (Masc,Pl,Ack) -> ["hvilika"]
  (Fem,Sg,Nom)  -> ["hvilikin"]
  (Fem,Sg,Gen)  -> ["hvilika","hvilikra"]
  (Fem,Sg,Dat)  -> ["hviliki","hvilikri","hvilike"]
  (Fem,Sg,Ack)  -> ["hvilika"]
  (Fem,Pl,_)    -> ["hvilika"]
  (Neut,Sg,Nom) -> ["hvilikit"]
  (Neut,Sg,Gen) -> ["hvilikins"]
  (Neut,Sg,Dat) -> ["hviliku","hviliko"]
  (Neut,Sg,Ack) -> ["hvilikit"]
  (Neut,Pl,_)   -> ["hvilikin"]

nokor_rule :: Pronoun
nokor_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Dat)     -> ["nakrom","nakvarom","nokrum","nokorum"]
  (Masc,Sg,Nom)  -> ["nakvar","nokor"]
  (Masc,Sg,Gen)  -> ["nakvars","nokors"]
  (Masc,Sg,Dat)  -> ["nakrom","nakvarom","nokrum","nokorum"]
  (Masc,Sg,Ack)  -> ["nakvan","nakvarn","nakon"]
  (Masc,Pl,Gen)  -> ["nakra","nakwara","nokra","nokora"]
  (Masc,Pl,Nom)  -> [x ++ y | x <- ["nakri","nakvari","nokri","nokori"], y <- ["","r"]]
  (Masc,Pl,Ack)  -> ["nakra","nakvara","nokra","nokora"]
  (Fem,Sg,Nom)   -> [x ++ y | x <- ["nakra","nakvara","nokra","nokora"], y <- ["","r"]]
  (Fem,Sg,Gen)   -> ["nakra","nakvara","nokra","nokora"]
  (Fem,Sg,Dat)   -> ["nakre","nakvare","nokri","nokori"]
  (Fem,Sg,Ack)   -> ["nakra","nakvara","nokra","nokora"]
  (Fem,Pl,Gen)     -> ["nakra","nakwara","nokra","nokora"]
  (Fem,Pl,_)     -> [x ++ y | x <- ["nakra","nakvara","nokra","nokora"], y <- ["","r"]]
  (Neut,Sg,Nom)  -> ["nakvat","nokot"]
  (Neut,Sg,Gen)  -> ["nakvars","nokors"]
  (Neut,Sg,Dat)  -> ["nakro","nakvaro","nokru","nokoru"]
  (Neut,Sg,Ack)  -> ["nakvat","nokot"]
  (Neut,Pl,Gen) -> ["nakra","nakwara","nakrar","nakwarar","nokra","nokora"]
  (Neut,Pl,_)    -> ["nakor","nokor"]

aengin_rule :: Pronoun
aengin_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
  (_,Pl,Gen)    -> ["ænga","ængra"]
  (_,Pl,Dat)    -> ["ængum","ængom"]
  (Masc,Sg,Nom) -> ["ængin"]
  (Masc,Sg,Gen) -> ["ængsins"]
  (Masc,Sg,Dat) -> ["ængum","ængom"]
  (Masc,Sg,Ack) -> ["ængin"]
  (Masc,Pl,Nom) -> ["ængi","ænge"]
  (Masc,Pl,Ack) -> ["ænga"]
  (Fem,Sg,Nom)  -> ["ængin","ængon"]
  (Fem,Sg,Gen)  -> ["ænga","ænginna"]
  (Fem,Sg,Dat)  -> ["ængi","ænginni","ænge","ænginne"]
  (Fem,Sg,Ack)  -> ["ænga"]
  (Fem,Pl,_)    -> ["ænga","ængar"]
  (Neut,Sg,Nom) -> ["ænkti","ænkte","ængte","ænti","ækki"]
  (Neut,Sg,Gen) -> ["ængsings"]
  (Neut,Sg,Dat) -> ["ængu","ængo"]
  (Neut,Sg,Ack) -> ["ænkti","ænkte","ængte","ænti","ækki"]
  (Neut,Pl,_)   -> ["ængin","ængon"]


hvaer_rule :: Pronoun
hvaer_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
   (_,Pl,_)      -> []
   (Masc,Sg,Nom) -> ["hvar"]
   (Masc,Sg,Gen) -> ["hvars"]
   (Masc,Sg,Dat) -> ["hvarium","hvariom"]
   (Masc,Sg,Ack) -> ["hvarn"]
   (Fem,Sg,Nom) -> ["hvar","hvarium","hvarion"]
   (Fem,Sg,Gen) -> ["hvarra","hvaria"]
   (Fem,Sg,Dat) -> ["hvare","hvarre", "hvarie","hvario"]
   (Fem,Sg,Ack) -> ["hvaria"]
   (Neut,Sg,Nom) -> ["hvart"]
   (Neut,Sg,Gen) -> ["hvars"]
   (Neut,Sg,Dat) -> ["hvariu","hvario"]
   (Neut,Sg,Ack) -> ["hvart"]

hvarghin_rule :: Pronoun
hvarghin_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
   (_,Pl,_)      -> []
   (Fem,Sg,Ack)  -> ["hvargha"]
   (Fem,_,_)     -> []
   (Masc,Sg,Nom) -> ["hvarghin"]
   (Masc,Sg,Gen) -> ["hvarghins"]
   (Masc,Sg,Dat) -> ["hvarghum","hvarghom"]
   (Masc,Sg,Ack) -> ["hvarghin"]
   (Neut,Sg,Dat) -> ["hvarghu","hvargho"]
   (Neut,Sg,Gen) -> []
   (Neut,Sg,_)   -> ["hvarki","hvarti","hvarte","hvarghit"]


person_rule :: PPronoun
person_rule t = strings $ case t of
   (First Sing Nom)  -> ["iak","iæk","iagh"]
   (First Sing Gen)  -> ["min"]
   (First Sing Dat)  -> ["mæ","mær","mik","migh"]
   (First Sing Ack)  -> ["mik","migh"]
   (First Dual Nom)  -> ["vit"]
   (First Dual Gen)  -> ["okar","okkar"]
   (First Dual _)  -> ["oker"]
   (First Plu Nom)   -> ["vi","vir"]
   (First Plu Gen)   -> ["var","vara","varra"]
   (First Plu _)     -> ["os"]
   (Second Sing Nom) -> ["þu"]
   (Second Sing Gen) -> ["þin"]
   (Second Sing Dat) -> ["þä","þär","þik","digh"]
   (Second Sing Ack) -> ["þik","thigh","digh"]
   (Second Dual Nom) -> ["it"]
   (Second Dual Gen) -> ["ikar","ikkar"]
   (Second Dual _) -> ["iker"]
   (Second Plu Nom)  -> ["i","ir"]
   (Second Plu Gen)  -> ["iþar","iþra"]
   (Second Plu _)    -> ["iþer"]
   (Refl Sing Nom)   -> []
   (Refl Sing Gen)   -> ["sin"]
   (Refl Sing Dat)   -> ["sæ","sær","sik","sigh"]
   (Refl Sing Ack)   -> ["sik","sigh"]
   (Refl Dual Nom)   -> []
   (Refl Dual Gen)   -> ["sin"]
   (Refl Dual  _)   -> ["sæ","sær","sik"]
   (Refl Plu Nom)    -> []
   (Refl Plu Gen)    -> ["sin"]
   (Refl Plu Dat)    -> ["sæ","sær","sik"]
   (Refl Plu Ack)    -> ["sæ","sær","sik"]


hanhon_rule :: Pronoun
hanhon_rule (PNF g n c) = 
 strings $
 case (g,n,c) of
   (_,Pl,_)      -> []
   (Neut,Sg,_)    -> []
   (Masc,Sg,Nom) -> ["han"]
   (Masc,Sg,Gen) -> ["hans"]
   (Masc,Sg,Dat) -> ["hanum","honum","honom"]
   (Masc,Sg,Ack) -> ["han"]
   (Fem,Sg,Nom) -> ["hon"]
   (Fem,Sg,Gen) -> ["hænna","hænnar","hænnas","hænnes"]
   (Fem,Sg,Dat) -> ["hænni","hænne"]
   (Fem,Sg,Ack) -> ["hana","hona"]

-- Verb

passivum :: Vox -> [String] -> Str
passivum p xs = 
 strings $ 
  case p of
   Act  -> xs
   Pass -> map (++"s") xs

indicative_pl :: (Person,Modus,Vox) -> String -> Str
indicative_pl (p,m,v) stem = 
 passivum v $
  case (m,p) of
   (_,P1)    -> [stem ++ "um", stem ++ "om"]
   (_,P2)    -> [stem ++ "in"]
   (Ind, P3) -> [stem ++ "a"]
   (Conj,P3) -> [stem ++ suf | suf <- ["i","in","e","en"]]

imperative_pl :: Person12 -> String -> Str
imperative_pl p stem =
 strings $
  case p of 
   Pers1 -> [stem ++ "um", stem ++ "om"]
   Pers2 -> [stem ++ "in"]

preteritum_ind_pl :: (Person,Vox) -> String -> Str
preteritum_ind_pl (p,v) stem =
 passivum v $
  case p of
   P1  -> [stem++"þum",stem++"þom"] 
   P2  -> [stem++"þin"]
   P3  -> [stem++"þu", stem++"þo"]

preteritum_conj_pl :: (Person,Vox) -> String -> Str
preteritum_conj_pl (p,v) stem =
 passivum v $
  case p of
   P1  -> [stem++"þum",stem++"þom"] 
   P2  -> [stem++"þin"]
   P3  -> [stem++"þi", stem++"þin"]

v_to_f :: String -> String
v_to_f stem
 | last stem == 'v' = init stem ++ "f"
 | otherwise        = stem


-- svaga verb

aelska_rule :: String -> Verb
aelska_rule aelska p = 
  case p of
   PresSg Ind Act   -> strings [aelska++"r",aelska]
   (PresSg Ind Pass)  -> strings [aelska ++"s"]
   (Inf v)            -> passivum v [aelska]
   (ImperSg)          -> strings [aelska]
   (ImperPl per)      -> imperative_pl      per       aelsk
   (PresPl per m v)   -> indicative_pl      (per,m,v) aelsk
   (PretInd Pl per v) -> preteritum_ind_pl  (per,v)   aelsk
   (PretConjPl per v) -> preteritum_conj_pl (per,v)  (aelsk++"a")
   (PresSg Conj v)    -> passivum v [aelsk++"i",aelsk++"e"]
   (PretInd Sg _ v)   -> passivum v [aelska++"þi"] 
   (PretConjSg v)     -> passivum v [aelska++"þi", aelska++"þe"]
 where aelsk  = tk 1 aelska

kraevia_rule :: String -> Verb
kraevia_rule kraevia p =
 case p of
   (PresSg Ind Act)     -> strings    [kraev++"er"]
   (PresSg Ind Pass)    -> strings    [kraef++"s"]
   (Inf v)              -> passivum v [kraevia]
   (ImperSg)            -> strings    [kraef]
   (ImperPl Pers1)      -> strings    [kraef++"ium",kraef++"iom"]
   (ImperPl Pers2)      -> strings    [kraev++"i"]
   (PresPl P1   _ v)    -> passivum v [kraef++"ium",kraef++"iom"]
   (PresPl P2 Ind v)    -> passivum v [kraev++"i"]
   (PresPl P3 Ind v)    -> passivum v [kraef++"ia"]
   (PretInd Sg _ v)     -> passivum v [kraf++"þi"] 
   (PretConjSg v)       -> passivum v [kraf++"þi", kraf++"þe"]
   (PretInd Pl per v)   -> preteritum_ind_pl (per,v) kraf
   (PretConjPl per v)   -> preteritum_conj_pl (per,v) kraf
   _                    -> aelska_rule (kraev++"a") p
 where kraev = tk 2 kraevia
       kraef = v_to_f kraev
       kraf  = case findStemVowel kraef of
                 (pre,_,suf) -> pre ++ "a" ++ suf
         
foera_rule :: String -> Verb
foera_rule foera p = 
 case p of
   (PresSg Ind Act)     -> strings [foer++"ir", foer++"i"]
   (PresSg Ind Pass)    -> strings [foer++"s"]
   (Inf v)              -> passivum v [foera]
   (ImperSg)            -> strings [foer]
   (PretInd Pl per v)   -> preteritum_ind_pl  (per,v) foer
   (PretConjPl per v)   -> preteritum_conj_pl (per,v) foer
   (PretInd Sg _ v)     -> passivum v [foer++"þi"]
   (PretConjSg v)       -> passivum v [foer++"þi", foer++"þe"]
   _                    -> aelska_rule foera p
 where foer = tk 1 foera

is_pret :: VerbForm -> Bool
is_pret v = elem v $
            [PretInd  n p v | v <- values, n <- values, p <- values] ++
            [PretConjSg   v | v <- values]                           ++
            [PretConjPl p v | v <- values, p <- values] 

is_pres :: VerbForm -> Bool
is_pres v = elem v $ 
           [PresSg     m v | v <- values, m <- values] ++
           [PresPl   p m v | v <- values, m <- values, p <- values] 
  
liva_rule :: String -> Verb
liva_rule liva p = 
 case p of
   (PresSg Ind Act)  -> strings [liv++"er", liv++"ir", liv++"i"]
   (PresSg Ind Pass) -> strings [lif++"s"]
   ImperSg           -> strings [lif]
   p | is_pret p     -> foera_rule (lif++"a") p
   _                 -> foera_rule liva p
 where liv = tk 1 liva
       lif = v_to_f liv

-- starka verb

vika_rule :: String -> Verb
vika_rule vika p = 
  case p of
   (PresSg Ind Act)   -> strings [vik++"er"]
   (PresSg Ind Pass)  -> strings [vik ++"s"]
   (Inf v)            -> passivum v [vika]
   (ImperSg)          -> strings [vik]
   (ImperPl per)      -> imperative_pl      per       vik
   (PresPl per m v)   -> indicative_pl      (per,m,v) vik
   (PretInd Pl P1 v)  -> passivum v  [vik++"um",vik++"om"]
   (PretInd Pl P2 v)  -> passivum v  [vik++"in"]
   (PretInd Pl P3 v)  -> passivum v  [vik++"u",vik++"o"]
   (PretConjPl P1 v)  -> passivum v  [vik++"um",vik++"om"]
   (PretConjPl P2 v)  -> passivum v  [vik++"in"]
   (PretConjPl P3 v)  -> passivum v  [vik++"i",vik++"in"]
   (PresSg Conj v)    -> passivum v [vik++"i",vik++"e"]
   (PretInd Sg P2 v)  -> passivum v [vek++"t"]
   (PretInd Sg _ v)   -> passivum v [vek]
   (PretConjSg v)     -> passivum v [vik++"i", vik++"e"]
 where vik  = tk 1 vika
       vek  = changeVowel "e" vik

laesa_rule :: String -> Verb
laesa_rule laesa p = 
  case p of
   (PresSg Ind Act)   -> strings [laes]
   (PresSg Ind Pass)  -> strings [laes]
   (Inf v)            -> passivum v [laesa]
   (ImperSg)          -> strings [laes]
   (ImperPl per)      -> imperative_pl      per       laes
   (PresPl per m v)   -> indicative_pl      (per,m,v) laes
   (PretInd Pl P1 v)  -> passivum v  [las++"um",las++"om"]
   (PretInd Pl P2 v)  -> passivum v  [las++"in"]
   (PretInd Pl P3 v)  -> passivum v  [las++"u",las++"o"]
   (PretConjPl P1 v)  -> passivum v  [las++"um",las++"om"]
   (PretConjPl P2 v)  -> passivum v  [las++"in"]
   (PretConjPl P3 v)  -> passivum v  [las++"i",las++"in"]
   (PresSg Conj v)    -> passivum v [laes++"i",laes++"e"]
   (PretInd Sg P2 v)  -> passivum v [las++"ts"]
   (PretInd Sg _ v)   -> passivum v [las]
   (PretConjSg v)     -> passivum v [las++"i", las++"e"]
 where  laes  = tk 1 laesa
        las   = changeVowel "a" (tk 1 laesa)

-- Auxilary functions
-- Handles the umlaut phenomena.

isVowel :: Char -> Bool
isVowel c = elem c "aeiouyæø"


changeVowel :: String -> String -> String
changeVowel vowel s = case findStemVowel s of
                         (spr,i,ck) -> spr ++ vowel ++ ck

-- Assumes that the stem vowel is the last vowel or diphtong in 
-- the input stem.
findStemVowel :: String -> (String, String, String)
findStemVowel sprick = (reverse rps, reverse i, reverse kc) where
  (kc, irps) = break isVowel $ reverse sprick
  (i,   rps) = span  isVowel $ irps

umlaut :: String -> String
umlaut man = m ++ mkUm a ++ n where
  (m,a,n) = findStemVowel man
  mkUm v = case v of
    "a" -> "æ"
    "o" -> "ø"
    _   -> v
