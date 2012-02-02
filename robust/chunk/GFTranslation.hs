toGF :: String -> Maybe Type
toGF str = case str of
     "ROOT"  -> catText
     "S"     -> catS 
     "AP"    -> catAP
     "AVP"   -> catMeta
     "CAVP"  -> catMeta -- should try to find conjunction etc 
     "CAP"   -> catAP
     "NP"    -> catNP
     "PP"    -> catAdv
     "VP"    -> catVP -- or just V?
     "XX"    -> catMeta
     "XP"    -> catMeta
     "CNP"   -> catNP
     "CPP"   -> catAdv
     "CONJP" -> catMeta -- should try to find conjunction etc 
     "CVP"   -> catVP
     "CS"    -> catS
     "CXP"   -> catMeta -- should try to find conjunction etc 
     "NAC"   -> catMeta
     "++"    -> catPConj, catConj
     "+A"    -> catPredet
     "+F"    -> catS  -- or remove?
     "AA"    -> -- what to do here? pAA
     "AG"    -> catAdv
     "AN"    -> catMeta 
     "AT"    -> catAP  --used to be VPSlashAP??
     "CA"    -> catPredet
     "DB"    -> catMeta
     "DT"    -> catQuant, catIQuant, catPredet, catN2
     "EF"    -> catRS
     "EO"    -> catUtt --"att ...": "att det är deras ansvar" or more of a VP:
                       --"att göra så", but cannot be parsed as VP, danger! rather use UttVP
     "ES"    -> catUtt -- same type as EO
     "ET"    -> catAdv  --what is this?
     "FO"    -> catPron 
     "FS"    -> catPron -- impersonligt eller generiskt pron
     "FV"    -> catV ----catV must be V,V2,V3 ...
     -- punctuation: I?,"IC","ID","IG","IK","IM", "IO", "IP", "IQ", "IR", "IS", "IT", "IU",
     -- punctuation: , "JC", "JG", "JR", "JT",
     "IV"    -> catV
     "KA"    -> catCAdv ++ catNP
     "MA"    -> catAdv, catNP
     "MD"    -> catNP, catAdv
     "MS"    -> catS 
     "NA"    -> inte
     "OA"    -> catAdv, catVP
     "OO"    -> catS, catVP, catAP,catNP
     "PL"    -> -- particle -> skip 
     "PR"    -> catPrep
     "PT"    -> catAdv
     "RA"    -> catAdv, catNP
     "SP"    -> catComp
     "SS"    -> catNP -- subject
     "ST"    -> --1.,* skip 
     "TA"    -> catAdv, catNP
     "TT"    -> catVoc
     "UK"    -> catAdv -- mostly "än", should be skipped 
     "VA"    -> catAdv, catNP
     "VO"    -> catUtt  --same as EO
     "VS"    -> catUtt  --same as EO
     "XA"    -> catAdv -- sa att saga
     "XF"    -> catMeta
     "XT"    -> catMeta
     "XX"    -> catMeta
     "YY"    -> catUtt --ja_Utt
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
   ] 


