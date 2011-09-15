module Test where


-- räknar fel???
compareRes :: [String] -> String
compareRes xs | and ans = "Hurra!!"
              | otherwise = let errs = map snd $ filter (not . fst) $ zip ans [0..] 
                                h    = if take 1 errs== [0] then [show (length res),show (length xs)] else [] in
                   unlines $ ":(" : h ++ [show (xs !! (x-1))++" /= "++ show (res !! (x-1)) | x <- errs, x>0]
  where ans = (length res==length xs) : zipWith (==) res xs 
  --16
res =
   -- den har katt
   ["? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N)))))) NoVoc))"
   -- det är mycket gult
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompAP (AdAP very_AdA (PositA yellow_A))))))) NoVoc))"
  -- även bil har katt
  ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (PredetNP ? (MassNP (UseN car_N))) (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N)))))) NoVoc))"
  -- katten är inte gul
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (UseComp (CompAP (PositA yellow_A)))))) NoVoc))"
   -- det är inte gult
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (PositA yellow_A)))))) NoVoc))"
   -- det är inte gult och stort
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (ConjAP and_Conj (BaseAP (PositA yellow_A) (PositA big_A)))))))) NoVoc))"
   -- det är inte gult litet och stort
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (ConjAP and_Conj (ConsAP (PositA yellow_A) (BaseAP (PositA small_A) (PositA big_A))))))))) NoVoc))"
   -- vilka får vara med
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (QuestVP whoPl_IP (ComplVV faa_VV (? UseCopula with_Prep))))) NoVoc))"
   -- här ges några katter
   ,"? (? (PhrUtt NoPConj (UttS (AdvS here_Adv (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN somePl_Det (UseN cat_N)) (UseV ?))))) NoVoc))"
   -- katten sitts efter bil
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (AdvVP (UseV ?) (PrepNP after_Prep (MassNP (UseN car_N))))))) NoVoc))"
   -- denna katt jagar bil
  ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN this8denna_Quant (UseN cat_N)) (ComplSlash (SlashV2a hunt_V2) (MassNP (UseN car_N)))))) NoVoc))"
   -- våra katter hade suttit
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres AAnter) PPos (PredVP (DetCN (DetQuant (PossPron we_Pron) NumPl) (UseN cat_N)) (UseV sit_V)))) NoVoc))"
   -- men han blir gulare
   ,"? (? (PhrUtt but_PConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron he_Pron) (ComplVA become_VA (UseComparA yellow_A))))) NoVoc))"
   -- de har dålig bil
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it8utr_Pron) (ComplSlash (SlashV2a have_V2) (MassNP (AdjCN (PositA bad_A) (UseN car_N))))))) NoVoc))"
   -- den dåliga bilen tänker
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (AdjCN (PositA bad_A) (UseN car_N))) (UseV think_V)))) NoVoc))"
   -- de vill äta bil
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (ComplVV want_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN car_N))))))) NoVoc))"
   -- de måste äta bil
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (ComplVV must_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN car_N))))))) NoVoc))"
   -- fler katter tänker gult
   ,"? (? (? (? (? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (AdvVP (UseV think_V) (PositAdvAdj yellow_A))))) NoVoc)))) (? ?)))"
   -- eller katten på bilen
   ,"? (? (? (? (? (? (? (? (PhrUtt (PConjConj or_Conj) (UttNP (AdvNP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (PrepNP on_Prep (DetCN (DetQuant DefArt NumSg) (UseN car_N))))) NoVoc)))))) (? ?)))"
   -- gå bakom en katt
   ,"? (? (PhrUtt NoPConj (UttImpPol PPos (ImpVP (AdvVP (UseV walk_V) (PrepNP behind_Prep (DetCN (DetQuant IndefArt NumSg) (UseN cat_N)))))) NoVoc))"
   -- katterna måste tänka igen
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN cat_N)) (ComplVV must_VV (? think_V ?))))) NoVoc))"
   -- katten börjar att tänka 
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (ComplVV ? (UseV think_V))))) NoVoc))"
   -- dessutom tänker antalet arbetande
   ,"? (? (PhrUtt NoPConj (UttS (AdvS ? (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (UseV think_V))))) NoVoc))"
   -- man äter redan äpplet
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplSlash (? (SlashV2a eat_V2) already_Adv) (DetCN (DetQuant DefArt NumSg) (UseN apple_N)))))) NoVoc))"
   -- katterna är dåligt ätna
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN cat_N)) (UseComp (CompAP (AdAP (PositAdAAdj bad_A) (? eat_V2))))))) NoVoc))"
   -- flertalet katter är någonstans
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant (? ?) NumSg) (UseN cat_N)) (UseComp (CompAdv somewhere_Adv))))) NoVoc))"
   -- katt för gällande bil
   ,"? (? (? (? (MassNP (UseN cat_N))) (PrepNP for_Prep (MassNP (AdjCN (PositA (? ?)) (UseN car_N))))))"
   -- alla äter på varandra 
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP everybody_NP (ComplSlash (SlashV2a eat_V2) (? (PrepNP on_Prep ?)))))) NoVoc))"
   -- det är inte lätt
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (PositA ?)))))) NoVoc))"
   -- vi har frågan gul 
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron we_Pron) (ComplSlash (SlashV2a have_V2) (? (DetCN (DetQuant DefArt NumSg) (UseN question_N)) yellow_A))))) NoVoc))"
   -- prästen kan kunna tänka
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN priest_N)) (ComplVV can_VV (ComplVV (DropAttVV can_VV) (UseV think_V)))))) NoVoc))"
   -- det är tjockt långt
   ,"? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompAP (AdAP (PositAdAAdj thick_A) (PositA long_A))))))) NoVoc))"





   ]

