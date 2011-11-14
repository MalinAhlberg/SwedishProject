import XMLHelp
import PGF
import Simplify
import Text.XML.HXT.Core 
import Data.Char
import Data.List
import Data.Ord
import Debug.Trace
import Data.Maybe
import Control.Monad
import qualified Debug.Trace as DT

talbanken = "../Talbanken05_20060604/FPS/P.tiger.xml" 
test = "test.xml"


---------------------------------------------------------------------
-- Extracting parts of talbanken
---------------------------------------------------------------------
main = mainEt talbanken
mainEt 	:: String -> IO ()
mainEt src
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO mappEvaluations --simplify --process  -- (return . take 100)  
	        >>>
            xpickleDocument xpSentences
                            [withIndent yes,
                             withInputEncoding utf8] "TalbankenTestSuite.xml")
      return ()

-- Function for extracting nice sentences
mappEvaluations x =
  do pgf <- readPGF "../gf/Big.pgf"
     let morpho  = buildMorpho pgf language
         Just language = readLanguage "BigSwe"
         nice  = filter (not . hasBadTag) x
--         short = sortBy (comparing ws) nice
--         good  = map (replaceWords morpho) short 
     return $ take 100 $ take10th  $ drop 257 nice--drop 400 $ take 500 good
 where replaceWords morpho s = 
          Sent (idS s) (rootS s) (map (g morpho) (XMLHelp.words s)) (info s) (ws s)
       g morpho w = let smallW = map toLower (word w) 
                        simpleW = if isKnown morpho smallW
                                     then smallW
                                     else fromMaybe smallW (lookup (pos w) simpleList)
              in W (XMLHelp.id w) simpleW (pos w) 
 
-- Collect trees by their id
process :: IOSArrow [Sentence] [Sentence]
process = arrIO (\x -> return $ getTrees x)

getTrees :: [Sentence] -> [Sentence]
getTrees = filter ((`elem` list) . idS) -- (concat trs)

-- Simplifies a sentence to a easier one. Is not totally correct, since
-- all information needed can't be extracted from Talbanken
simplify :: IOSArrow [Sentence] [Sentence]
simplify = arrIO (\x -> do putStrLn (show x)
                           return $ map f x)
 where f s  = Sent (idS s) (rootS s) (map g (XMLHelp.words s)) (info s) (ws s)
       g w  = let simpleW = fromMaybe (word w) (lookup (pos w) simpleList) in
                DT.trace (word w++pos w) $ W (XMLHelp.id w) simpleW (pos w) 

-- checks all tags in sentence for evil ones
hasBadTag :: Sentence -> Bool
hasBadTag x = any (`elem` badTags) (ts x++cs x++ps x)
  where ts = map fst . concatMap tags . info
        cs = map cat . info
        ps = map pos . XMLHelp.words 

-- a list of unnice tags
badTags = ["NAC","XP","AVP",
           "PU" ,"CAP","CAVP","CNP","CONJP","CPP","CS"
           ,"CVP","CXP"
           ,"CJ"
           ,"XX","XT","XF","XA","DB"
           ,"IC","IG","IQ","IR","IS","IT"
           ,"ID","ET","UK","++"
          ]

-- a list of translations
simpleList = Simplify.transl

take10th :: [a] -> [a]
take10th = unfoldr f
  where f [] = Nothing
        f x  = Just (head x,drop 10 x)

---------------------------------------------------------------------
-- Finds sentences in Talbanken
---------------------------------------------------------------------

mainText = do
 sents <- mainF talbanken (return . map getSentence) 
 old   <- readFile "Best.txt"
 return $ map (findIt $ concat sents) (lines old)

findIt :: [(Id,String)] -> String -> Maybe Id
findIt sents str = lookup (trim str) (map (\(id,s) -> (format s,id)) sents)

format = trim . (\\dots) .  map toLower  
trim (x:xs) | isSpace x         = trim xs
trim xs     | isSpace (last xs) = trim $ init xs
trim xs                              = xs
dots = ".,:!?,"
swap (a,b) = (b,a)


list = catMaybes best 

extractSentences = do 
  ss <- mainF talbanken (return . map getSentence')
  return $ concat ss

-- allows words to contain a star, but not stars alone as a word
isBad' = any (`elem` ["*"]) . snd
-- do not allow * inside of words
isBad = any (`elem` ['*']) . concat . snd
prettyPrint (id,s) = id++"\t"++ Data.List.unwords s

---bad testsuite maker!!
makeTestSuite = do
  ss <- extractSentences
  let goods = filter (not . isBad) ss
      testS = f goods
  return $ map fst testS
  --return $ unlines $ map prettyPrint testS
 where f =  map head .takeWhile (not . null) 
           . map (take 10) . iterate (drop 10)
          -- unfoldr (return . first head . splitAt 10)

g = map head .takeWhile (not . null) 
   . map (take 10) . iterate (drop 10)


isKnown morpho str = 
  not $ null [lemma | lemma <- lookupMorpho morpho (map toLower str)]

testa  str = do
  pgf <- readPGF "../gf/BigTest.pgf"
  let Just language = readLanguage "BigTestSwe"
      morpho        = buildMorpho pgf language
  return $ not $ null [lemma | lemma <- lookupMorpho morpho str]



-- generated list of my old testsuite
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
