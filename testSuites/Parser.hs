module Main where
import System.Timeout
import System.Environment (getArgs)
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
import PGF
import XMLHelp hiding (words,parse)
--import Parser hiding (pgfFile)
--import TheFixer

-- use with tee -a outfile
data Parsed = Pars {t :: Tree, s :: String, n :: Int}
  deriving Show

data Count = C {ok :: Int, reject :: Int, total :: Int, lexErr :: Int, time :: Int}
type CState = StateT Count IO
type Sent = (Id,String)
-- 40 s, maybe too much
timeLimit =  20*10^6

main = do
  (file:_) <- getArgs
  putStr "Parsing input file..."
  sents <- mainF file (return . map getSentence)
  putStr " Ok\nReading PGF..."
  pgf <- readPGF pgfFile
  putStrLn " Ok\n"
  --absMap <- readAbbs 
  let morpho = buildMorpho pgf lang
  c <- execStateT (mapM_ (tryparse pgf morpho) sents) newCount
  print c
  return ()

tryparse :: PGF -> Morpho -> Sent -> CState ()
tryparse pgf morpho sents = do 
  count sents
  mapM_ parse' pgf morpho sents

parse' :: PGF -> Morpho -> Sent -> CState ()
parse' pgf morpho (s,"") = addEmpty >> putEmptyMsg s
parse' pgf morpho (s,st) = do
  let unknown = badWords morpho st
  if null unknown then parseOk pgf s st
        else putLexMsg s unknown >> addBadLex

parseOk :: PGF -> Sent-> CState ()        
parseOk pgf s = do
  tree <- io $ timeout timeLimit $ return $! parse pgf lang textType (snd s)
  maybe (addTime >> putTimeMsg s) (getBestTree s tree)


badWords :: Morpho -> String -> [String]
badWords morpho s = [w| w <- words s, null $ lookupMorpho morpho w]

getBestTree :: Sent -> [Tree] -> CState ()
getBestTree s tree = do
  let treelist = map (rank . toPars) tree
  let newTrees = map t $ sortBy (comparing n) treelist
  io $ putMsg s newTrees
  unless (null newTrees) addOk
  io $ appendFile outFile (formatRes (s,newTrees))

addOk,addEmpty, addBadLex :: CState ()
addOk     = modify $ \s -> s {ok = succ (ok s)}
addEmpty  = modify $ \s -> s {reject = succ (reject s)}
addBadLex = modify $ \s -> s {lexErr = succ (lexErr s)}
addTime   = modify $ \s -> s {time = succ (time s)}
count :: [String] -> CState ()
count ss = modify  $ \s -> s {total = length ss + total s}

putTimeMsg s   = output s blue   " was timed out" 
putEmptyMsg s  = output s turkos " was rejected"  
putLexMsg s ws = output s red    (" was not parsed (unknown words: "
                                  ++unwords ws++")" )

output :: Sent -> Color -> String -> CState ()
output s c str = do
  io $ putStrLn $ show s ++ color c str 
  io $ appendFile outFile $ "\n" ++ show s ++ str


textType = fromJust $ readType "Text"
uttType  = fromJust $ readType "Phr"
lang     = fromJust $ readLanguage "BigSwe" --"BigNewSwe"
pgfFile =  "testOld/Big.pgf" -- "BigNew.pgf"
outFile = "testis.txt"


toPars :: Tree -> Parsed
toPars t = Pars t (showExpr [] t) 5

-- guessing game...
rank :: Parsed -> Parsed
rank (Pars t s n) = Pars t s $ n + sum [i | (cat,i) <- rules, cat `isInfixOf` s]

-- [(GFConstructor,Point)] The more points given the lower ranking
rules :: [(String,Int)]
rules = [ ("GenNP"  , 2),("ApposCN" ,  2),("MassNP"   ,  1),("NumCard"   ,  1),
          ("UseCl"  ,1),("dropAtt", 1),("man_nn",  1)
         ,("faar_nn", 1),("en_nn"   ,  1),("foer_nn"  ,  1),("mycken_nn" ,  1),("nyy_nn",  1)
         ,("vill_ab", 1),("titt_nn" ,  1),("leva_nn"  ,  1)
         ,("haer_nn", 1),("dehaer_N", -1),("dethaer_N", -1),("denhaer_N" , -1),("(Refl" , -1)
         ,("PrepCN",  1)]
         -- ReflVP and ReflGenVP rather than without 
            
io :: IO a -> CState a
io = lift

newCount :: Count
newCount = C 0 0 0 0 0

instance Show Count where
  show (C ok rej tot lex time) 
     = "*******\n Result: passed: "++show ok++"\t rejected: "
       ++show rej++"\t timed out: "++show time++"\n\t of totally "
       ++show tot++" sentences."++"\n"++show lex++" had unknown words"




-------
--formatRes :: ( -> String
formatRes (s,[])  = show s ++ " was not Parsed"++"\n"
formatRes (s,trs) = show s ++ "("++show (length trs)++" trees): \n" ++ unlines (map (showExpr []) trs) ++ "\n"

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
turkos = 5    -- correct this!

putMsg :: Sent -> [Tree] -> IO ()
putMsg s [] = putStrLn $ show s ++ color red " was not parsed"
putMsg s ps = putStrLn $ show s ++ (color green " was parsed in "
                                      ++show (length ps)++" ways")


