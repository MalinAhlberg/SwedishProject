module Test where
import translate 

testIt = do res <- main' "test2.xml"
            if isOK res then putStrLn "YEY" else putStrLn ":("

isOK res = "? (? (UseCl (TTAnt TPres ASimul) PPos (PredVP (MassNP (UseN ?)) (ComplSlash (SlashV2a have_V2) (MassNP (UseN ?)))))) 76.47058823529412\n"++
"? (? (UseCl (TTAnt TPres ASimul) PPos (PredVP (MassNP (UseN ?)) (UseComp (CompAP (AdAP very_AdA (PositA ?))))))) 76.47058823529412
? (? (UseCl (TTAnt TPres ASimul) PPos (PredVP (PredetNP ? (MassNP (UseN car_N))) (ComplSlash (SlashV2a have_V2) (MassNP (UseN ?))))))
77.35849056603774
? (? (UseCl (TTAnt TPres ASimul) PNeg (PredVP (MassNP (UseN ?)) (UseComp (CompAP (PositA yellow_A))))))
77.94117647058823
? (? (UseCl (TTAnt TPres ASimul) PNeg (PredVP (MassNP (UseN ?)) (UseComp (CompAP (ConjAP and_Conj (BaseAP (PositA yellow_A) (PositA big_A))))))))
79.54545454545455
? (? (UseCl (TTAnt TPres ASimul) PNeg (PredVP (MassNP (UseN ?)) (UseComp (CompAP (ConjAP and_Conj (ConsAP (PositA yellow_A) (BaseAP (PositA small_A) (PositA big_A)))))))))"

