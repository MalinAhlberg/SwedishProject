

assign :: [Tree String] -> Maybe (Verb,[Vals])
assign tree = do
  Just v  <- getMainVerb ts
  vs <- tryValencies (prune ts)
  return (v,vs)


getMainVerb ts = findNode "FV" ts 
                 `mplus`
                 findNode "IV" ts = v
               
findNode :: [Tree String] -> String -> Maybe String
findNode []  = Nothing
findNode (Node x [Node t [Node w []]]:xs) v 
          | x==v = Just w
          | otherwise = findNode xs v
tryValencies = 
prune :: [Tree String] -> [Tree String]
prune = filter isObj  
  where isObj = (Node x _) = x `elem` ["SP","OO","OA","FO","EO","IO"]

-- remove FV or IV and SS. Maybe even all that is not: OO,SP,OA,FO,EO,IO
-- use Translate, change magiclookup to never fail.
--try parse (grammar (mkApp meta) ("" :-> pCompl V3)) dummyPGF dummyMorpho
--and return which vForms we do not fail for.
