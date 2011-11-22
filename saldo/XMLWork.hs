import XMLHelp hiding (id)
import SaldoXML
import Text.XML.HXT.Core hiding (Tree,trace)
import Debug.Trace
import Data.List
import Data.Tree
import Data.Char
import Data.Function
import Data.ByteString.Char8 (pack)
 
-- For extrating certian combination of tags from Talbanken

mainet f src 
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO (return . f))


main = do
  putStrLn "Starting ..."
  putStrLn "Reading xml ..."
  xml <- mainet (map toNumberedTree) input
  putStrLn "Finding tags ..."
  let tags = sort $ nub $ concatMap (findWords cs ts) $ map snd $ concat xml
  putStrLn "Writing file ..."
  writeFile "advDT.tags" $ show tags
 where cs = ["DT"]
       ts = ["AB", "ABDA", "ABFA", "ABJA", "ABJU", "ABKP", "ABKS", "ABKU", "ABMN", "ABNA"
            ,"ABOC", "ABRA", "ABSA", "ABSU", "ABTA", "ABZA", "ABZK"]
-- for finding all (syntactic) tags given to a list of words
main1 = do
  mxs <- parseDict "importFiles/saldoPart2.xml"
  case mxs of
       Nothing -> putStrLn "could not parse importFiles/saldoPart2.xml"
       Just xs -> do 
           putStrLn "saldo read"
           let ws = [form | (E pos tab) <- map snd xs, pos == pack "pn",
                                   let form = map snd tab,
                                   head form `notElem` persPron ]
           putStrLn "Analyse words ..."
           findAllTags' ws  -- recursive to decrease memory usage
 where persPron = ["jag","du","han","hon","det","den","denna","detta","dessa"
                  ,"vi","ni","de","dom"]

input = "../Talbanken05_20060604/FPS/P.tiger.xml"
--input = "../mapping/test.xml"
sadan = ["sådan","sådant","sådana"]

findAllTags' [] = return () 
findAllTags' (w:ws) = do 
  tag <- findAllTags w 
  appendFile "pn.tags" $ show tag
  putStrLn $ show tag
  findAllTags' ws 

findAllTags :: [String] -> IO [(String,String)]
findAllTags ws = do 
  putStrLn $ "Analysing "++head ws
  xml <- mainet (map toNumberedTree) input 
  let tags = concatMap (findTags ws) (map snd $ concat xml)
  return $ sortBy (compare `on` snd) $ nub tags

findTags :: [String] -> Tree String -> [(String,String)]
findTags ws (Node cat [Node tag [Node w []]]) | not (null form) = [(head form,cat)]
                                              | otherwise       = []
     where form = [map toLower w] `intersect` ws 
findTags ws (Node tag ts) = concatMap (findTags ws) ts


findWords :: [String] -> [String] -> Tree String -> [(String,String,String)]
findWords cats tags (Node cat [Node tag [Node w []]]) 
         | not (null isCat) && not (null isTag) = [(w,head isCat, head isTag)]
         | otherwise       = []
     where isCat = [cat] `intersect` cats
           isTag = [tag] `intersect` tags
findWords cs tgs (Node tag ts) = concatMap (findWords cs tgs) ts



---- check if there are dublicates in the testsuites
f = do 
    es <-  search "../testSuites/TalbankenTestEasy.xml" 
    ms <-  search "../testSuites/TalbankenTestMedium.xml" 
    hs <-  search "../testSuites/TalbankenTestHard.xml" 
    let ns = sort $ concat $ ms++es++hs
    return $ g ns
  where search = mainet (map idS)

g (x:y:xs) | x==y = (x:g xs)
           | x/=y = g xs
g xs              = xs

