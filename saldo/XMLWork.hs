import XMLHelp
import Text.XML.HXT.Core 
import Data.List

mainet src 
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO (\x -> return $ map idS x))

f = do 
    es <- mainet "../testSuites/TalbankenTestEasy.xml" 
    ms <- mainet "../testSuites/TalbankenTestMedium.xml" 
    hs <- mainet "../testSuites/TalbankenTestHard.xml" 
    let ns = sort $ concat $ ms++es++hs
    return $ g ns

g (x:y:xs) | x==y = (x:g xs)
           | x/=y = g xs
g xs              = [] 


