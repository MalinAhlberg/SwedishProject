

  pgf <- readPGF
  let morpho = ...
  lex <- getSaldo 
  map simpilfy (words str)

-- compound: kattpälsen  -> [("",katt_nn),("pälsen",päls_nn)]
-- ne      : Johan Ros   -> [("X1",johan_PN),("",ros_PN)]
simplify :: [String] -> [(String,Text)]
simplify w = do
 ok = lookupInMorpho w
 if not then ok = compound lex w 
 makeAnnotated es
 replaceNames in new unannotated
 [ | (str,tag) <-es, tg=="" ]  -- only let untagged be named, but cannot do one by one
 ns <- findName es
 zi

