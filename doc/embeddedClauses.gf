finns:

 EmbedVP
 EmbedQS    -> SC
 EmbedS
 
 PredSCVP : SC -> VP -> Cl
 SentCN   : CN -> SC -> CN
 
 frågan var katterna sover någonstans
 PhrUtt NoPConj (UttNP (DetCN (DetQuant DefArt NumSg) (SentCN (UseN question_N) (EmbedQS (UseQCl (TTAnt TPres ASimul) PPos (QuestIAdv where_IAdv (PredVP (DetCN (DetQuant DefArt NumPl) (UseN cat_N)) (AdvVP (UseV sleep_V) somewhere_Adv)))))))) NoVoc
 
 att hon äter äpplen är dåligt
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredSCVP (EmbedS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron she_Pron) (ComplSlash (SlashV2a eat_V2) (DetCN (DetQuant IndefArt NumPl) (UseN apple_N)))))) (UseComp (CompAP (PositA bad_A)))))) NoVoc

 att dö är dåligt
 PredSCVP (EmbedVP (UseV die_V)) (UseComp (CompAP (PositA bad_A))) | l

-------
StrandRelSlash   : RP -> ClSlash -> RCl ;   -- that he lives in
 huset som hon tittar på är stort
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (RelCN (UseN house_N) (UseRCl (TTAnt TPres ASimul) PPos (StrandRelSlash IdRP (SlashVP (UsePron she_Pron) (SlashV2a watch_V2)))))) (UseComp (CompAP (PositA big_A)))))) NoVoc

EmptyRelSlash    : ClSlash -> RCl ;   -- he lives in
 huset hon tittar på är stort
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (RelCN (UseN house_N) (UseRCl (TTAnt TPres ASimul) PPos (EmptyRelSlash (SlashVP (UsePron she_Pron) (SlashV2a watch_V2)))))) (UseComp (CompAP (PositA big_A)))))) NoVoc

RelNP   : NP -> RS  -> NP ;    -- Paris, which is here
 katten , som är gul (tänker)
  NP       RS         (VP)
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (RelNP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseComp (CompAP (PositA yellow_A)))))) (UseV think_V)))) NoVoc

RelCN   : CN -> RS  -> CN ;   -- house that John bought
 (jag ser) katten som är gul
           CN      RS
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplSlash (SlashV2a see_V2) (DetCN (DetQuant DefArt NumSg) (RelCN (UseN cat_N) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseComp (CompAP (PositA yellow_A))))))))))) NoVoc

RelCl    : Cl -> RCl ;            -- such that John loves her
 (jag vill ha en katt) sådan att den leker
                          Cl
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron he_Pron) (ComplVV want_VV (ComplSlash (SlashV2a have_V2) (DetCN (DetQuant IndefArt NumSg) (RelCN (UseN cat_N) (UseRCl (TTAnt TPres ASimul) PPos (RelCl (PredVP (UsePron it8utr_Pron) (UseV play_V))))))))))) NoVoc

RelVP    : RP -> VP -> RCl ;      -- who loves John
 (jag såg katten) som tycker om honom
                    RP
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (UsePron i_Pron) (ComplSlash (SlashV2a see_V2) (DetCN (DetQuant DefArt NumSg) (RelCN (UseN cat_N) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (ComplSlash (SlashV2a like_V2) (UsePron he_Pron)))))))))) NoVoc

-- obs om ej prep  Can also use more fancy RP!!
RelSlash : RP -> ClSlash -> RCl ; -- whom John loves
 jag såg katten om vilken han tycker
                               VP             
 PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (UsePron i_Pron) (ComplSlash (SlashV2a see_V2) (DetCN (DetQuant DefArt NumSg) (RelCN (UseN cat_N) (UseRCl (TTAnt TPres ASimul) PPos (RelSlash IdRP (SlashVP (UsePron he_Pron) (SlashV2a like_V2)))))))))) NoVoc


-- obs dåligt
RelS     : S -> RS -> S ;              -- she sleeps, which is good
 hon sover , som  är dåligt
 PhrUtt NoPConj (UttS (RelS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron she_Pron) (UseV sleep_V))) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (AdvVP UseCopula (PositAdvAdj bad_A)))))) NoVoc

ny RelS:
 hon sover, vilket är dåligt
 PhrUtt NoPConj (UttS (RelS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron she_Pron) (UseV sleep_V))) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseComp (CompAP (PositA bad_A))))))) NoVo

  skulle vilja ha: 'hon sover, vilket vi vet'

