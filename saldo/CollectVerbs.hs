import SaldoXML

main = do


 d <- parseDict x
 let ls = filter ((`elem` unknown) . fst) d
     vs = concatMap (map snd . table . snd) ls
