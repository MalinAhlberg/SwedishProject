module Test where


compareRes :: [String] -> String
compareRes xs | and ans = color green "Hurra!! All is good"
              | otherwise = let errs = map snd $ filter (not . fst) $ zip ans [0..] 
                                h    = if take 1 errs== [0] 
                                          then [show (length res),show (length xs)]
                                          else []                                in
                   color red $ unlines $ ":(" : h ++ [show (xs !! (x-1))
                                          ++" /= "++ show (res !! (x-1)) | x <- errs, x>0]
  where ans = (length res==length xs) : zipWith (==) res xs 



res =
   -- den har katt
   ["s452\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N)))))) NoVoc"
   -- det är mycket gult
   ,"s541\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompAP (AdAP very_AdA (PositA yellow_A))))))) NoVoc"
  -- även bil har katt
  ,"s452\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N)))))) NoVoc"
  -- better but needs lexicon,"PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (PredetNP ? (MassNP (UseN car_N))) (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N)))))) NoVoc"
  -- katten är inte gul
   ,"s620\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (UseComp (CompAP (PositA yellow_A)))))) NoVoc"
   -- det är inte gult
   ,"s1739\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (PositA yellow_A)))))) NoVoc"
   -- det är inte gult och stort
   ,"s1739\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (ConjAP and_Conj (BaseAP (PositA yellow_A) (PositA big_A)))))))) NoVoc"
   -- det är inte gult litet och stort
   ,"s1739\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (ConjAP and_Conj (ConsAP (PositA yellow_A) (BaseAP (PositA small_A) (PositA big_A))))))))) NoVoc"
   -- vilka får tänka till
   ,"s542\tPhrUtt NoPConj (UttQS (UseQCl (TTAnt TPres ASimul) PPos (QuestVP whoPl_IP (ComplVV faa_VV (UseV tankaTill_V))))) NoVoc"
   -- här ges några katter
   ,"s694\tPhrUtt NoPConj (UttS (AdvS here_Adv (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN somePl_Det (UseN cat_N)) (PassV2 eat_V2))))) NoVoc"
   -- katten sitts efter bil
   ,"s802\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (AdvVP (PassV2 eat_V2) (PrepNP after_Prep (MassNP (UseN car_N))))))) NoVoc"
   -- denna katt jagar bil
  ,"s898\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN this8denna_Quant (UseN cat_N)) (ComplSlash (SlashV2a hunt_V2) (MassNP (UseN car_N)))))) NoVoc"
   -- våra katter har suttit
   ,"s945\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres AAnter) PPos (PredVP (DetCN (DetQuant (PossPron we_Pron) NumPl) (UseN cat_N)) (UseV sit_V)))) NoVoc"
   -- men han blir gulare
   ,"s1001\tPhrUtt but_PConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron he_Pron) (ComplVA become_VA (UseComparA yellow_A))))) NoVoc"
   -- den har dålig bil
   ,"s1037\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it8utr_Pron) (ComplSlash (SlashV2a have_V2) (MassNP (AdjCN (PositA bad_A) (UseN car_N))))))) NoVoc"
   -- den dåliga bilen tänker
   ,"s1106\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (AdjCN (PositA bad_A) (UseN car_N))) (UseV think_V)))) NoVoc"
   -- de vill äta bil
   ,"s1103\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (ComplVV want_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN car_N))))))) NoVoc"
   -- de måste äta bil
   ,"s1107\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (ComplVV must_VV (ComplSlash (SlashV2a eat_V2) (MassNP (UseN car_N))))))) NoVoc"
   -- fler resor tänker gult
   ,"s1129\t? (? (? (? (PhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (AdvVP (UseV think_V) (PositAdvAdj yellow_A))))) NoVoc)))) (? ?)"
   --,"PhrUtt NoPConj (? (? (? (? (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (AdvVP (UseV think_V) (PositAdvAdj yellow_A)))))))) (? ?)) NoVoc"
   -- eller katten på bilen
   ,"s1150\t? (? (? (? (? (? (PhrUtt (PConjConj or_Conj) (UttNP (AdvNP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (PrepNP on_Prep (DetCN (DetQuant DefArt NumSg) (UseN car_N))))) NoVoc)))))) (? ?)"
   -- gå bakom en katt
   ,"s1244\tPhrUtt NoPConj (UttImpPol PPos (ImpVP (AdvVP (UseV walk_V) (PrepNP behind_Prep (DetCN (DetQuant IndefArt NumSg) (UseN cat_N)))))) NoVoc"
   -- katterna måste tänka till
   ,"s1246\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN cat_N)) (ComplVV must_VV (UseV tankaTill_V))))) NoVoc"
   -- katten börjar att tänka 
   ,"s1295\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (ComplVV (DropAttVV begin_VV) (UseV think_V))))) NoVoc"
   -- dessutom tänker antalet arbetande
   ,"s1321\tPhrUtt NoPConj (UttS (AdvS ? (UseCl (TTAnt TPres ASimul) PPos (PredVP (? (? ?) (? ?)) (UseV think_V))))) NoVoc"
   -- man äter redan äpplet
   ,"s1458\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplSlash (AdvVPSlash (SlashV2a eat_V2) already_Adv) (DetCN (DetQuant DefArt NumSg) (UseN apple_N)))))) NoVoc"
   -- katterna är dåligt ätna
   ,"s1506\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN cat_N)) (UseComp (CompAP (AdAP (PositAdAAdj bad_A) (? eat_V2))))))) NoVoc"
   -- antalet katter är någonstans
   ,"s1606\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (ComplN2 numberOf_N2 (DetCN (DetQuant IndefArt NumPl) (UseN cat_N)))) (UseComp (CompAdv somewhere_Adv))))) NoVoc"
   -- katt för gällande bil
   ,"s1617\t? (? (MassNP (UseN cat_N))) (PrepNP for_Prep (MassNP (AdjCN (PositA (? ?)) (UseN car_N))))"
   --,"PhrUtt NoPConj (? (? (MassNP (UseN cat_N))) (PrepNP for_Prep (MassNP (AdjCN (PositA (? ?)) (UseN car_N))))) NoVoc"
   -- alla äter på varandra 
   ,"s1682\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP everybody_NP (ComplSlash (SlashV2a eat_V2) (? (PrepNP on_Prep ?)))))) NoVoc"
   -- det är inte lätt
   ,"s1739\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseComp (CompAP (PositA ?)))))) NoVoc"
   -- vi målar frågan gul 
   ,"s1756\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron we_Pron) (ComplSlash (SlashV2A paint_V2A (PositA yellow_A)) (DetCN (DetQuant DefArt NumSg) (UseN question_N)))))) NoVoc"
   -- prästen kan kunna tänka
   ,"s1840\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN priest_N)) (ComplVV can_VV (ComplVV (DropAttVV can_VV) (UseV think_V)))))) NoVoc"
   -- det är tjockt långt
   ,"s1871\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompAP (AdAP (PositAdAAdj thick_A) (PositA long_A))))))) NoVoc"
   -- nu kunde katten flyga
   ,"s1955\tPhrUtt NoPConj (UttS (AdvS now_Adv (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (ComplVV (DropAttVV can_VV) (UseV fly_V)))))) NoVoc"
   -- han är katt idag
   ,"s1975\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron he_Pron) (AdvVP (UseComp (CompNP (MassNP (UseN cat_N)))) today_Adv)))) NoVoc"
   -- det är rena gummit
   ,"s1989\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompNP (DetCN (DetQuant DefArt NumSg) (AdjCN (PositA clean_A) (UseN rubber_N)))))))) NoVoc"
   -- vi måste ha katt
   ,"s2024\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron we_Pron) (ComplVV must_VV (ComplSlash (SlashV2a have_V2) (MassNP (UseN cat_N))))))) NoVoc"
   -- det går redan här
   ,"s2241\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (AdvVP (AdvVP (UseV walk_V) already_Adv) here_Adv)))) NoVoc"
   -- nu ska katten flyga
   ,"s1955\tPhrUtt NoPConj (UttS (AdvS now_Adv (UseCl (TTAnt TFut ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (UseV fly_V))))) NoVoc"
   -- nu skulle katten flyga
   ,"s1955\tPhrUtt NoPConj (UttS (AdvS now_Adv (UseCl (TTAnt TCond ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN cat_N)) (UseV fly_V))))) NoVoc"
   -- två katter kan tänka
   ,"s5482\tPhrUtt NoPConj (UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant IndefArt (NumCard (NumDigits (IDig D_1)))) (UseN cat_N)) (ComplVV can_VV (UseV think_V))))) NoVoc"
   -- den kommer att tänka
   ,"s3\tPhrUtt NoPConj (UttS (UseCl (TTAnt TFut ASimul) PPos (PredVP (UsePron it8utr_Pron) (? think_V)))) NoVoc"
   -- nu kan barnen rinna om de sväller
   ,"s5726\tPhrUtt NoPConj (UttS (AdvS now_Adv (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumPl) (UseN child_N)) (AdvVP (ComplVV can_VV (UseV flow_V)) (SubjS if_Subj (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (UseV swell_V))))))))) NoVoc"
   ]
type C = Int
color :: C -> String -> String
color c s = fgcol c ++ s ++ normal

normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)


fgcol :: Int -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"


red,green :: C
red = 1
green = 2 

