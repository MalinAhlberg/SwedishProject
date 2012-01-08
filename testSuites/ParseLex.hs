module ParseLex where
import System.Timeout
import System.Process
import System.Console.ANSI hiding (Color)
import System.IO
import System.Exit
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Map hiding (map,null,filter)
import Data.Ord
import Data.Char
import Data.List
import Data.Function
import Control.Concurrent
import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Exception
import PGF
import XMLHelp hiding (words,parse,id)
import MkLex

-- use with tee -a outfile
data Parsed = Pars {t :: Tree, s :: String, n :: Int}
  deriving Show

data Count = C {ok :: Int, reject :: Int, total :: Int, lexErr :: Int, time :: Int
               ,strict :: Bool , outputFile :: FilePath}
type CState = StateT Count IO
type Sent = (Id,String)
-- 40 s, maybe too much
timeLimit =  30*10^6

play :: IO ()
play = do
  (maps,pgf) <- play'
  loop maps pgf
 where loop maps pgf = do
            putStrLn "Give a sentence"
            str <- input
            tree <- processparse str pgf langOld maps
            case tree of
                 Nothing -> putStrLn "no parse"
                 Just t  -> putStrLn "hurray"
            loop maps pgf

play' = do
  putStrLn "before pgf"
  maps <- mkLexMap
  pgf <- readPGF pgfDict
  putStrLn "after pgf"
  return (maps,pgf)

processparse :: String -> PGF -> Language -> LexMap -> IO (Maybe ((FilePath,FilePath),Int))
processparse str pgfDict langDict maps = do
  res <- mkDir maps (prepare str) langDict pgfDict
  let (pgf',l') = maybe (pgfBackUp,langBackUp) id res
  putStrLn "reading new PGF"
  pgfNew <- readPGF pgf'
  let morpho = buildMorpho pgfNew l'
  pars <- parseNormal pgfNew morpho l' ("1",str)
  print pars
  createFile pgfNew l' pars
 where prepare    = words . map toLower
       createFile _   _  Nothing  = return Nothing
       createFile pgf l (Just (tree,s)) = do
          png <- pipeIt2graphviz pgf l tree
          return $ Just (png,s)

input :: IO String
input = input' ""
 where input' :: String -> IO String
       input' inputStr = do
            c <- getChar
            unless (c == '\t' || c == '\DEL') $ putChar c
            hSetEcho stdin False
            case c of
                '\n'   -> return inputStr
                '\DEL' -> do
                   setCursorColumn $ (length inputStr) - 1
                   clearFromCursorToLineEnd
                   input' (initSafe inputStr)
                _      -> input' (inputStr ++ [c])
       initSafe [] = []
       initSafe xs = init xs

parseNormal pgf morpho lang s = do
  let fix     = fixPunctuation $ replaceNumbers s
      unknown = badWords morpho (snd fix)
  putStrLn $ "Unknown: " ++show unknown
  if null unknown then parseOkNormal (snd fix) else return Nothing
 where parseOkNormal s = do
         putStrLn $ "Parsing "++s
         tree <- timeout timeLimit $ return $ parse pgf lang textType (map toLower s)
         case tree of
            Just xs@(x:_) -> return $ Just (getBest xs,length xs)
            _             -> return Nothing
       getBest :: [Tree] -> Tree
       getBest ts = fromMaybe (head ts) (rank ts)
       rank :: [Tree] -> Maybe Tree
       rank = listToMaybe . filter (not .(\x -> any (`isInfixOf` x) bads) . showExpr [])
       bads = ["NumCard","man_nn","faar_nn","en_nn","foer_nn"
              ,"mycken_nn","nyy_nn","vill_ab","titt_nn","leva_nn","haer_nn"
              ,"PrepCN"]


testa :: String -> IO ()
testa s = do
  pgf <- readPGF "BigParse.pgf"
  let morpho = buildMorpho pgf (read "BigParseSwe")
  print $ lookupMorpho morpho $ map toLower s
  tree <- parseNormal pgf morpho (read "BigParseSwe") ("",s)
  print tree
  --print [w| w <- words s, null $ lookupMorpho morpho $ map toLower w]

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
langOld  = fromJust $ readLanguage "BigSwe" --"BigSwe" --"BigNewSwe"
pgfFile =  "../gf/Big.pgf" -- "../gf/BigTest.pgf" -- "../gf/Big.pgf" -- "BigNew.pgf"
outFile = "testisNP.txt"

pgfBackUp = "../gf/Test.pgf"
langBackUp :: Language
langBackUp = read "BigTestSwe"

pgfDict = "DictSweAbs.pgf"
langDict :: Language
langDict = read "DictSwe"





-- Terminal color output
type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)


fgcol :: Int -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"

red = 1
green = 2
yellow = 3
blue = 4    -- correct this!
pink = 5

pipeIt2graphviz :: PGF -> Language -> Tree -> IO (FilePath,FilePath)
pipeIt2graphviz pgf lang t = do
    let dotFileP = "tmptreep.dot"
        pngFileP = "tmptreep.png"
        dotFileA = "tmptreea.dot"
        pngFileA = "tmptreea.png"
    writeFile dotFileP $ graphvizParseTree pgf lang t
    readProcess "dot" ["-Tpng",dotFileP,"-o",pngFileP] []
    writeFile dotFileA $ graphvizAbstractTree pgf (True,True) t
    readProcess "dot" ["-Tpng",dotFileA,"-o",pngFileA] []
    return (pngFileP,pngFileA)

