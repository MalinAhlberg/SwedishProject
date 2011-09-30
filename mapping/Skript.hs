import Data.List


maini f = do
 inp <- readFile f
 let metas  = map (length . filter (=='?')) (lines inp)
 return $ map (\x -> (head x,length x)) $ group $ sort metas

