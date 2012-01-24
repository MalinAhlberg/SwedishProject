module ParseLex 
  (play
  ,reparse
  ,processparse
  ,testa
  ,smallparse
  ,getNextWord
  ,ParseData
  ,UserId
  ,FileType) where
import qualified System.IO.Strict as SIO
import System.Timeout
import System.TimeIt
import System.FilePath
import System.Process
import System.Random
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Utils as LU
import Control.Arrow
import PGF
import MkLex
import qualified Data.Map as Map     
import Data.Function

import Log as L
-- stderr -> err.log

data Parsed = Pars {t :: Tree, s :: String, n :: Int}
  deriving Show

type FileType = String
type ParseData = (LexMap,PGF)
type Sent = (String,String)
-- 40 s, maybe too much
timeLimit =  30*10^6


play = do
  L.log "will make dictMap"
  (t,maps) <- timeItT $! mkLexMap
  L.log $ "will compile Dictpgf: "++show t
  (t',pgf) <- timeItT $ readPGF pgfDict
  L.log $ "after pgf: "++show t'
  return (maps,pgf)

reparse :: UserId -> String -> FileType ->  IO [(FilePath,FilePath)]
reparse id str svg = do
  pgf <- readPGF pgfBackUp -- $ inDir id reusedPGF for big dict
  let inp = map toLower $ snd $ fixPunctuation $ replaceNumbers ("",str)
  runCommand $ "rm "++id++"/*"
  pid <- runCommand $ "rm "++"images/"++id++"/*"
  waitForProcess pid
  logA id $ "try to reparse "++inp
  let trees = parse pgf langBackUp textType inp -- reusedlang for big dict
  logA id $ "got "++show (length trees)
  mapM (uncurry $ pipeIt2graphviz id pgf langBackUp svg)
                             (zip trees $ map show [1..]) --reusedlang for big dict

processparse :: UserId -> String -> PGF -> LexMap -> FileType 
                -> IO [(Sent,Int, Either [String] (FilePath,FilePath))]
processparse id str pgfDict maps svg = do
  L.log $ id ++ " Begins with "++str
  res <- mkDir id maps (prepare str) langDict pgfDict
  let (pgf',l') = fromMaybe (pgfBackUp,langBackUp) res
  logA id "reading new PGF"
  cleanFiles id
  realParse id pgf' l' svg (splitUp str)
 where prepare = words . map toLower

cleanFiles id = do
   L.log $ "clean "++inDir id "/*"
   runCommand $ "rm "++inDir id "/*"
   runCommand $ "rm "++"images/"++inDir id "/*"

-- the mighty function for splitting up indata
splitUp [] = []
splitUp (w:ws) | w `elem` "\n.?!" = splitUp ws
splitUp ws = let (x,xs) = break (`elem` "\n.?!") ws
             in x:splitUp (drop 1 xs)

realParse :: UserId -> FilePath -> Language -> FileType -> [String]  
             -> IO [(Sent,Int, Either [String] (FilePath,FilePath))]
realParse id pgf l svg str = do
  logA id "read new pgf"
  (t,pgfNew) <- timeItT $ readPGF pgf
  logA id $ "read new pgf, "++show t 
  let morpho = buildMorpho pgfNew l
      input  = zip (map show [1..] ) str
  logA id "parsing"
  (t',pars) <- timeItT $ mapM (parseNormal pgfNew morpho l) input
  logA id (show pars)
  logA id "creates png files"
  mapM (createFile pgfNew l) pars
 where
       createFile :: PGF -> Language -> (Sent,Int,Either [String] Tree) 
                     -> IO (Sent,Int,Either [String] (FilePath,FilePath)) 
       createFile _   _  (s,i,Left x)    = return (s,0,Left x)
       createFile pgf l (s,i,Right tree) = do
          png <- pipeIt2graphviz id pgf l svg tree (fst s)
          return (s,i,Right png)

smallparse :: UserId -> FileType -> String 
              -> IO [(Sent,Int, Either [String] (FilePath,FilePath))]
smallparse id svg = realParse id pgfBackUp langBackUp svg . splitUp
  

parseNormal :: PGF -> Morpho -> Language -> Sent -> IO (Sent,Int, Either [String] Tree)
parseNormal pgf morpho lang x@(i,s) = do
  let fix     = fixPunctuation $ replaceNumbers x
      unknown = badWords morpho (snd fix)
  logA "" $ "Unknown: " ++show unknown
  if null unknown then parseOkNormal fix else return (x,0,Left unknown)
 where parseOkNormal (i,s) = do
         L.log $ "Parsing "++s
         tree <- timeout timeLimit $ return $ parse pgf lang textType (map toLower s)
         case tree of
            Just xs@(x:_) -> return ((i,s),length xs,Right (getBest xs))
            _             -> return ((i,s),0,Left [])
       getBest :: [Tree] -> Tree
       getBest ts = fromMaybe (head ts) (rank ts)
       rank :: [Tree] -> Maybe Tree
       rank = listToMaybe . filter (not .(\x -> any (`isInfixOf` x) bads) . showExpr [])
       bads = ["NumCard","MassNP","Top","man_nn","faar_nn","en_nn","foer_nn"
              ,"mycken_nn","nyy_nn","vill_ab","titt_nn","leva_nn","haer_nn"
              ,"PrepCN"]

testa :: String -> UserId ->  IO ()
testa s id = do
  pgf <- readPGF pgfBackUp --"BigParse.pgf"
  let morpho = buildMorpho pgf langBackUp --(read "BigParseSwe")
  print $ lookupMorpho morpho $ map toLower s
  tree <- parseNormal pgf morpho langBackUp ("",s)
  ls <- reparse id s "pdf"
  print tree
  print ls

badWords :: Morpho -> String -> [String]
badWords morpho s = [w| w <- words s, null $ lookupMorpho morpho $ map toLower w]

replaceNumbers :: Sent -> Sent
replaceNumbers = second (unwords . map exchangeNum . words)
  where exchangeNum w | any isNumber w = "1"
                      | otherwise   = w
fixPunctuation :: Sent -> Sent
fixPunctuation (n,s) | isAlpha (last s) = (n,s++" .")
                     | otherwise        = (n,s)


textType = fromJust $ readType "Text"
uttType  = fromJust $ readType "Phr"
langOld  = fromJust $ readLanguage "BigSwe" 
--pgfFile =  "../gf/Big.pgf" 
--outFile = "testisNP.txt"
--
--reusedPGF = "BigParse.pgf"
--reusedLang :: Language
--reusedLang = read "BigParseSwe"

pgfBackUp = "../gf/BigTest.pgf"
langBackUp :: Language
langBackUp = read "BigTestSwe"

pgfDict = "DictSweAbs.pgf"
langDict :: Language
langDict = read "DictSwe"


pipeIt2graphviz :: UserId -> PGF -> Language -> FileType -> Tree 
                   -> String -> IO (FilePath,FilePath)
pipeIt2graphviz id pgf lang svg t i = do
    tag <- randomIO :: IO Int
    logA id $ "have made a random number! "++show tag
    let dotFileP = inDir id "tmptreep.dot"
        pngFileP = fileName "tmptreep" tag 
        dotFileA = inDir id "tmptreea.dot"
        pngFileA = fileName "tmptreea" tag
    SIO.run $ SIO.writeFile dotFileP $ graphvizParseTree pgf lang t
    readProcess "dot" ["-T",svg,dotFileP,"-o","images/"++pngFileP] []
    SIO.run $ SIO.writeFile dotFileA $ graphvizAbstractTree pgf (True,True) t
    readProcess "dot" ["-T",svg,dotFileA,"-o","images/"++pngFileA] []
    return (pngFileP,pngFileA)
  where fileName name tag = inDir id $ addExtension (name ++show tag) svg
 
----------------------

complete' :: PGF -> Language -> Type -> Maybe Int -> String
         -> [String]
complete' pgf from typ mlimit input =
  let (ws,prefix) = tokensAndPrefix input
      ps0 = PGF.initState pgf from typ
      (ps,ws') = loop ps0 ws
  in if not (null ws')
       then []
       else maybe id takeBest mlimit $ order $ Map.keys (PGF.getCompletions ps prefix)
  where
    order = delete nonExist . sortBy (compare `on` nicety)
    takeBest i xs | last input == ' ' = take i $ map head $ groupBy ((==) `on` (take 1)) xs
                  | otherwise         = take i xs

    tokensAndPrefix :: String -> ([String],String)
    tokensAndPrefix s | not (null s) && isSpace (last s) = (ws, "")
                      | null ws = ([],"")
                      | otherwise = (init ws, last ws)
        where ws = words s

    loop ps []     = (ps,[])
    loop ps (w:ws) = case PGF.nextState ps (PGF.simpleParseInput w) of
                       Left  es -> (ps,w:ws)
                       Right ps -> loop ps ws

    nonExist = "#\191@\167X?X&%/"
    nicety (i:xs) | not (isAlpha i)  = ('\247':i:xs)
                  | otherwise        = map toLower (i:xs)

getNextWord str i = do
  pgf <- readPGF pgfBackUp
  let x = complete' pgf langBackUp textType (Just i) str
  return x

tryComplete str = do
  w <- getNextWord str 10
  print w
