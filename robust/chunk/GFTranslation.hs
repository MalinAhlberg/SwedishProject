module GFTranslation where
import Types
import PGF

toGF :: String -> [Type]
toGF str = case str of
     "ROOT"  ->  [text]
     "S"     ->  [s,relvp ]
     "AP"    ->  aps
     "CAP"   ->  aps
     "NP"    ->  nps
     "PP"    ->  advs 
     "VP"    ->  [vp] -- or just V?
     "CNP"   ->  nps
     "CPP"   ->  advs
     "CVP"   ->  [vp]
     "CS"    ->  [s]
     "++"    ->  [pconj, conj]
     "+A"    ->  [predet]
     "+F"    ->  [s]  -- or remove?
     "AA"    ->  [ada] --What to do here? pAA
     "AG"    ->  advs
     "AT"    ->  aps  --used to be VPSlashAP??]
     "CA"    ->  [predet]
     "DT"    ->  ([iquant, predet, n2]++quant)
     "EF"    ->  [rs]
     "EO"    ->  [utt] --"att ...": "att det är deras ansvar" or more of a VP:
                     --"att göra så", but cannot be parsed as VP, danger! rather use UttVP
     "ES"    ->  [utt] -- same type as EO
     "ET"    ->  advs--what is this?
     "FO"    ->  [pron]
     "FS"    ->  [pron] -- impersonligt eller generiskt pron
     "FV"    ->  vtypes ----catV must be V,V2,V3 ...
     -- punctuation: I?,"IC","ID","IG","IK","IM", "IO", "IP", "IQ", "IR", "IS", "IT", "IU",
     -- punctuation: , "JC", "JG", "JR", "JT",
     "IV"    ->  vtypes 
     "MA"    ->  (advs ++ nps)
     "MD"    ->  (cadv: nps)
     "MS"    ->  [s]
     "OA"    ->  (vp : advs)
     "OO"    ->  ([s, vp,npobj] ++ aps)
     "PR"    ->  [prep]
     "PT"    ->  advs
     "RA"    ->  (advs ++ nps)
     "SP"    ->  [comp]
     "SS"    ->  [npsub] 
     "TA"    ->  (advs ++ nps)
     "TT"    ->  [voc]
     "VA"    ->   (advs ++ nps)
     "VO"    ->  [utt]  --same as EO]
     "VS"    ->  [utt]  --same as EO
     "XA"    ->  advs -- sa att saga
     _       ->  []


vtypes = [v,v2,v3,v2a,va,vv,vs]
nps    = [npobj,npsub]
advs   = [advobj,advsub]
aps    = [apobj,apsub]
quant   = [quantobj,quantsub]
{-
     "AVP"   ->  [meta]
     "CAVP"  ->  [meta] -- should try to find conjunction etc 
     "XX"    ->  [meta]
     "XP"    ->  [meta]
     "CONJP" ->  [meta] -- should try to find conjunction etc 
     "CXP"   ->  [meta] -- should try to find conjunction etc 
     "NAC"   ->  [meta]
     "AN"    ->  [meta]
     "DB"    ->  [meta]
     "XF"    ->  [meta]
     "XT"    ->  [meta]
     "XX"    ->  [meta]-}
 
--     "YY"    -> [utt ]--ja_Utt
--     "UK"    ->  [adv] -- mostly "än", should be skipped ]

--     "PL"    -> -- particle -> skip 
--     "ST"    -> --1.,* skip ]
--     "KA"    -> [cAdv ++ NP]
--     "NA"    -> [inte
     -- deep trees
--     "BS"    -> cat"S" -- NP 'den är som katten'
--     "CJ"    -> cat"S" `mplus` cat "PP" `mplus` cat "VP"   --first conjunct
--                        `mplus` pAdj     `mplus` pflatNP
--     "C+"    -> cat "S" `mplus` cat "PP" `mplus` cat "VP"   --second conjuct
--                        `mplus` pAdj     `mplus` pflatNP
--     "CC"    -> cat "S" `mplus` cat "PP" `mplus` cat "VP"   --sister conjuct
--                        `mplus` pAdj     `mplus` pflatNP
--     "HD"    -> (fst3 <$> pCN) `mplus` pAdj `mplus` pIAdv `mplus` (fst <$> pNP)
--     "IF"    -> do (tpm,s,pol,v) <- pVP "IV"  -- for deep trees
--                   return v
--     "PA"    -> pflatNP `mplus` (fst <$> pNP)  -- for deep trees
--                        `mplus` cat "VP" `mplus` cat "S" 
--                        `mplus` cat "NP" `mplus` cat "CNP"
--      
--    ,"VG"    -> cat "VP"  -- for deep trees

  {-
translatePos pos = case take 2 pos of
                        "++" -> [catConj,catPConj]
                        "AB" -> [Adv,IAdv,A,AdA]  --should be wrapped by functions...
                        "AJ" -> [A,A2]
                        "AN" -> catNouns
                        "AVIP">The
                        "AVIV">The
                        "AVPK">The
                        "AVPS">The
                        "AVPT">The
                        "AVSN">The
                        "BVIV">The
                        "BVPK">The
                        "BVPS">The
                        "BVPT">The
                        "BVSN">The
                        "EH  PU">H
                        "EH">Hesit
                        "EHNJPU">H
                        "EHRJ">Hes
                        "EHRJPU">H
                        "EHUJ">Hes
                        "EHUJPU">H
                        "EN  HH">I
                        "EN  HHGG"
                        "EN">Indef
                        "FV">The v
                        "FVIV">The
                        "FVPK">The
                        "FVPS  PA"
                        "FVPS">The
                        "FVPT">The
                        "FVSN">The
                        "GVIP">The
                        "GVIV  PA"
                        "GVIV">The
                        "GVPS  PA"
                        "GVPS">The
                        "GVPT  PA"
                        "GVPT">The
                        "GVSN  PA"
                        "GVSN">The
                        "HV">The v
                        "HVIP">The
                        "HVIV">The
                        "HVPS">The
                        "HVPT">The
                        "HVSN">The
                        "I?">Quest
                        "IC">Quota
                        "ID    AA"
                        "ID">Part 
                        "IG">Other
                        "IG++  KR"
                        "IG++EL">O
                        "IG++ELKR"
                        "IGPR">Oth
                        "IK    KR"
                        "IK">Comma
                        "IK++  KR"
                        "IK++">Com
                        "IK++EL">C
                        "IK++ELKR"
                        "IK++OC">C
                        "IM">Infin
                        "IP">Perio
                        "IPPR">Per
                        "IPXX">Per
                        "IQ">Colon
                        "IR">Paren
                        "IS">Semic
                        "IS++">Sem
                        "IS++ELKR"
                        "IT">Dash<
                        "IT++  KR"
                        "IT++">Das
                        "IT++EL">D
                        "IT++ELKR"
                        "IT++OC">D
                        "ITPR">Das
                        "IU">Excla
                        "KV">The v
                        "KVIV">The
                        "KVPS">The
                        "KVPT">The
                        "KVSN">The
                        "MN    GG"
                        "MN  HH">M
                        "MN  SS">M
                        "MN">Meta-
                        "MNDD">Met
                        "MNDDSS">M
                        "MVPS">The
                        "MVPT">The
                        "NJ">Falli
                        "NJPU">Fal
                        "NN    GG"
                        "NN  HH">O
                        "NN  HHGG"
                        "NN  HS">O
                        "NN  HSGG"
                        "NN  SS">O
                        "NN  SSGG"
                        "NN">Other
                        "NNDD  GG"
                        "NNDD">Oth
                        "NNDDHH">O
                        "NNDDHHGG"
                        "NNDDHS">O
                        "NNDDHSGG"
                        "NNDDSS">O
                        "NNDDSSGG"
                        "NNDDTR">O
                        "PN    GG"
                        "PN  HH">P
                        "PN  HHGG"
                        "PN  SS">P
                        "PN  SSGG"
                        "PN">Prope
                        "PNDD  GG"
                        "PNDD">Pro
                        "PNDDHH">P
                        "PNDDSS">P
                        "PNDDSSGG"
                        "PO">Prono
                        "POCP">Pro
                        "POCPHH">P
                        "POCPHHAA"
                        "POCPHHGG"
                        "PODP  AA"
                        "PODP  GG"
                        "PODP">Pro
                        "PODPHH">P
                        "PODPHHAA"
                        "PODPHHGG"
                        "POFP">Pro
                        "POFPHH">P
                        "POFPHHAA"
                        "POFPHHGG"
                        "POKP  GG"
                        "POKP">Pro
                        "POKPHH">P
                        "POKPHHGG"
                        "PONP">Pro
                        "PONPHH">P
                        "POOP">Pro
                        "POOPHH">P
                        "POPP  AA"
                        "POPP">Pro
                        "POPPHH">P
                        "POPPHHAA"
                        "POPPHHGG"
                        "PORP  GG"
                        "PORP">Pro
                        "PORPHH">P
                        "PORPHHAA"
                        "PORPHHGG"
                        "POSU  GG"
                        "POSU">Pro
                        "POSUHH">P
                        "POSUHHGG"
                        "POTP  GG"
                        "POTP">Pro
                        "POTPHH">P
                        "POTPHHAA"
                        "POTPHHGG"
                        "POXP  AA"
                        "POXP  GG"
                        "POXP">Pro
                        "POXPHH">P
                        "POXPHHAA"
                        "POXPHHGG"
                        "POXX">Pro
                        "POZP">Pro
                        "POZPHH">P
                        "POZPHHAA"
                        "POZPHHGG"
                        "PR">Prepo
                        "PRKP">Pre
                        "PRSU">Pre
                        "PU">List 
                        "QQ">?</va
                        "QV">The v
                        "QVIV">The
                        "QVPS">The
                        "QVPT">The
                        "QVSN">The
                        "RJ">Level
                        "RJPU">Lev
                        "RO  HH">N
                        "RO">Numer
                        "ROOT">Num
                        "ROOTHH">N
                        "SP  HH">P
                        "SP  HHGG"
                        "SP  HM">P
                        "SP  HMGG"
                        "SP  SM">P
                        "SP">Prese
                        "SV">The v
                        "SVIV">The
                        "SVPS">The
                        "SVPT">The
                        "TP    PA"
                        "TP  HH">P
                        "TP  HHGG"
                        "TP  HHPA"
                        "TP  HM">P
                        "TP  HMGG"
                        "TP  HMPA"
                        "TP  SM">P
                        "TP  SMPA"
                        "TP">Perfe
                        "UJ">Risin
                        "UJPU">Ris
                        "UK">Subor
                        "UKAT">Sub
                        "UKFI">Sub
                        "UKKC">Sub
                        "UKKD">Sub
                        "UKKM">Sub
                        "UKKS">Sub
                        "UKKU">Sub
                        "UKOM">Sub
                        "UKTE">Sub
                        "UU">Excla
                        "VN    GG"
                        "VN  HH">V
                        "VN  HS">V
                        "VN  SS">V
                        "VN">Verba
                        "VNDD  GG"
                        "VNDD">Ver
                        "VNDDHH">V
                        "VNDDHS">V
                        "VNDDSS">V
                        "VNDDSSGG"
                        "VV    PA"
                        "VV  SM">O
                        "VV  SMPA"
                        "VV">Other
                        "VVIP">Oth
                        "VVIPSM">O
                        "VVIV  PA"
                        "VVIV">Oth
                        "VVIVSM">O
                        "VVIVSMPA"
                        "VVOP">Oth
                        "VVPK">Oth
                        "VVPS  PA"
                        "VVPS">Oth
                        "VVPSSM">O
                        "VVPSSMPA"
                        "VVPT  PA"
                        "VVPT">Oth
                        "VVPTSM">O
                        "VVPTSMPA"
                        "VVSN  PA"
                        "VVSN">Oth
                        "VVSNSM">O
                        "VVSNSMPA"
                        "WVIV">The
                        "WVPK">The
                        "WVPS">The
                        "WVPT">The
                        "WVSN">The
                        "XX">Uncla
                        "YY">Inter
                        -}
