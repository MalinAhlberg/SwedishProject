import XMLHelp
import Data.Char
import Data.List
import Debug.Trace
import Data.Maybe

talbanken = "../Talbanken05_20060604/FPS/P.tiger.xml" 
test = "test.xml"

findIt :: [(Id,String)] -> String -> Maybe Id
findIt sents str = lookup (trim str) (map (\(id,s) -> (format s,id)) sents)

main = do
 sents <- mainF talbanken (return . map getSentence) 
 old   <- readFile "Best.txt"
 return $ map (findIt $ concat sents) (lines old)

format = trim . (\\dots) .  map toLower  
trim (x:xs) | isSpace x         = trim xs
trim xs     | isSpace (last xs) = trim $ init xs
trim xs                              = xs
dots = ".,:!?,"
swap (a,b) = (b,a)


--- 
extractSentences = do 
  ss <- mainF talbanken (return . map getSentence')
  return $ concat ss

isBad = any (`elem` ["*"]) . snd
prettyPrint (id,s) = id++"\t"++ Data.List.unwords s


makeTestSuite = do
  ss <- extractSentences
  let goods = filter (not . isBad) ss
      testS = f goods
  return $ unlines $ map prettyPrint testS
 where f =  map head .takeWhile (not . null) 
           . map (take 10) . iterate (drop 10)
          -- unfoldr (return . first head . splitAt 10)

g = map head .takeWhile (not . null) 
   . map (take 10) . iterate (drop 10)



best = [Just "s452",Just "s541",Just "s542",Just "s620",Just "s694",Just "s802",Just "s898"
       ,Just "s945",Just "s1001",Just "s1037",Just "s1103",Just "s1106",Just "s1107",Just "s1129"
       ,Just "s1150",Just "s1244",Just "s1246",Just "s1295",{-Just "s1296",-} Just "s1321"
       ,Just "s1458",Just "s1506",Just "s1606",Just "s1617",Just "s1682",Just "s1739"
       ,Just "s1756",Just "s1840",Just "s1871",Just "s1955",Just "s1975",Just "s1989"
       ,Just "s2024",Just "s2241"
       ,Just "s2250"
       ,Just "s2417",Just "s2446",Just "s2551",Just "s2734",Just "s2981",Just "s3022",Just "s3177"
       ,Just "s3219",Just "s3284",Just "s3301",Just "s3555",Just "s3561",Just "s3566",Just "s3598"
       ,Just "s3687",Just "s3747",Nothing,Just "s3901",Just "s4005",Just "s4093",Just "s4281"
       ,Just "s4659",Just "s4790",Just "s4954",Just "s5086",Just "s5090",Just "s3747",Just "s5499"
       ,Just "s5645",Just "s5654",Just "s5724",Just "s5753",Just "s5784",Just "s5803",Just "s5974"
       ,Just "s26",Just "s62",Just "s81",Just "s83",Just "s223",Just "s226",Just "s396",Just "s524"
       ,Just "s528",Just "s538",Just "s568",Just "s632",Just "s690",Just "s696",Just "s752"
       ,Just "s771",Just "s793",Just "s835",Just "s888",Just "s907",Just "s966",Just "s967"
       ,Just "s996",Just "s1006",Just "s1028",Just "s1056",Just "s1139",Just "s1143",Just "s1176"
       ,Just "s1192",Just "s1236",Just "s1249",Just "s1250",Just "s1290",Just "s1378",Just "s1496"
       ,Just "s1674",Just "s1719",Just "s1767",Just "s1819",Just "s1855",Just "s1858",Just "s1878"
       ,Just "s1923",Just "s1948",Just "s1965",Just "s1988",Just "s1991",Just "s1995",Just "s2055"
       ,Just "s2074",Nothing]

bestList = map fromJust $ filter isJust best
