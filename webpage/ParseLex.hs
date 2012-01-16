module ParseLex 
  (play
  ,reparse
  ,processparse
  ,testa
  ,smallparse
  ,ParseData
  ,UserId) where
import System.Timeout
import System.TimeIt
import System.Process
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Utils
import Control.Arrow
import PGF
import XMLHelp hiding (words,parse,id)
import MkLex

-- use with tee -a outfile
data Parsed = Pars {t :: Tree, s :: String, n :: Int}
  deriving Show

type ParseData = (LexMap,PGF)
type Sent = (Id,String)
-- 40 s, maybe too much
timeLimit =  30*10^6


play = do
  putStrLn "will make dictMap"
  maps <- timeIt $! mkLexMap
  putStrLn "will compile Dictpgf"
  pgf <- timeIt $ readPGF pgfDict
  putStrLn "after pgf"
  return (maps,pgf)

reparse :: UserId -> String -> IO [(FilePath,FilePath)]
reparse id str = do
  pgf <- readPGF $ inDir id reusedPGF
  let inp = map toLower $ snd $ fixPunctuation $ replaceNumbers ("",str)
  putStrLn $ "try to reparse "++inp
  let trees = parse pgf reusedLang textType inp
  putStrLn $ "got "++show (length trees)
  mapM (uncurry $ pipeIt2graphviz id pgf reusedLang) (zip trees $ map show [1..])

-- id to find out which user, which map
processparse :: UserId -> String -> PGF -> LexMap -> IO [(Sent,Int, Either [String] (FilePath,FilePath))]
processparse id str pgfDict maps = do
  putStrLn $ "Begins with "++str
  res <- mkDir id maps (prepare str) langDict pgfDict
  let (pgf',l') = fromMaybe (pgfBackUp,langBackUp) res
  putStrLn "reading new PGF"
  realParse id pgf' l' (splitUp str)
 where prepare = words . map toLower

-- the mighty function for splitting up indata
splitUp [] = []
splitUp (w:ws) | w `elem` "\n.?!" = splitUp ws
splitUp ws = let (x,xs) = break (`elem` "\n.?!") ws
             in x:splitUp (drop 1 xs)

realParse :: UserId -> FilePath -> Language -> [String] -> IO [(Sent,Int, Either [String] (FilePath,FilePath))]
realParse id pgf l str = do
  putStrLn "read new pgf"
  pgfNew <- timeIt $ readPGF pgf
  let morpho = buildMorpho pgfNew l
      input  = zip (map show [1..] ) str
  putStrLn "parsing"
  pars <- timeIt $ mapM (parseNormal pgfNew morpho l) input
  print pars
  putStrLn "creates png files"
  timeIt $ mapM (createFile pgfNew l) pars
 where
       createFile :: PGF -> Language -> (Sent,Int,Either [String] Tree) -> IO (Sent,Int,Either [String] (FilePath,FilePath)) 
       createFile _   _  (s,i,Left x)    = return (s,0,Left x)
       createFile pgf l (s,i,Right tree) = do
          png <- pipeIt2graphviz id pgf l tree (fst s)
          return (s,i,Right png)

smallparse :: UserId -> String ->  IO [(Sent,Int, Either [String] (FilePath,FilePath))]
smallparse id = realParse id pgfBackUp langBackUp . splitUp
  

parseNormal :: PGF -> Morpho -> Language -> Sent -> IO (Sent,Int, Either [String] Tree)
parseNormal pgf morpho lang x@(i,s) = do
  let fix     = fixPunctuation $ replaceNumbers x
      unknown = badWords morpho (snd fix)
  putStrLn $ "Unknown: " ++show unknown
  if null unknown then parseOkNormal fix else return (x,0,Left unknown)
 where parseOkNormal (i,s) = do
         putStrLn $ "Parsing "++s
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
  pgf <- readPGF "BigParse.pgf"
  let morpho = buildMorpho pgf (read "BigParseSwe")
  print $ lookupMorpho morpho $ map toLower s
  tree <- parseNormal pgf morpho (read "BigParseSwe") ("",s)
  ls <- reparse id s
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
pgfFile =  "../gf/Big.pgf" 
outFile = "testisNP.txt"

reusedPGF = "BigParse.pgf"
reusedLang = read "BigParseSwe"

pgfBackUp = "../gf/Test.pgf"
langBackUp :: Language
langBackUp = read "BigTestSwe"

pgfDict = "DictSweAbs.pgf"
langDict :: Language
langDict = read "DictSwe"


pipeIt2graphviz :: UserId -> PGF -> Language -> Tree -> Id -> IO (FilePath,FilePath)
pipeIt2graphviz id pgf lang t i = do
    let dotFileP = inDir id "tmptreep.dot"
        pngFileP = inDir id "tmptreep"++i++".png"
        dotFileA = inDir id "tmptreea.dot"
        pngFileA = inDir id "tmptreea"++i++".png"
    writeFile dotFileP $ graphvizParseTree pgf lang t
    readProcess "dot" ["-Tpng",dotFileP,"-o","images/"++pngFileP] []
    writeFile dotFileA $ graphvizAbstractTree pgf (True,True) t
    readProcess "dot" ["-Tpng",dotFileA,"-o","images/"++pngFileA] []
    return (pngFileP,pngFileA)

