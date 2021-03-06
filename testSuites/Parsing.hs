module Parsing where
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
import XMLHelp hiding (words,parse)
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
  maps <- mkLexMap
  pgf <- readPGF pgfFile
  return (maps,pgf)

processparse :: String -> PGF -> Language -> LexMap -> IO (Maybe FilePath)
processparse str pgf langOld maps = do
  (pgf',l') <- mkDir maps (prepare str) langOld pgf
  pgfNew <- readPGF pgf'
  let morpho = buildMorpho pgfNew l'
  tree <- evalStateT (tryparse pgfNew morpho l' ("1",str)) (newCount False "pipfil")
  maybe (return Nothing) (createFile pgf) tree
 where prepare    = words . map toLower
       createFile pgf t = do
           writeFile "tmptree" $ graphvizParseTree pgf (read "BigParseSwe") t
           return (Just "tmptree")

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

showPDF :: IO ()
showPDF =  do
  -- createHandle
   putStrLn "There will be a tree"
   --readProcess "showdot" ["tmptree"]  []

--run :: String -> Bool -> FilePath -> IO Count
--run file strict out = do  
--  putStrLn "Parsing input file..."
--  sents <- liftM head $ mainF file (return . map getSentence)
--  putStrLn " Creating new dictionary..."
--  (pgf',lang') <- mkDir (words $ concatMap (map toLower . snd) sents) langOld pgfFile 
--  putStrLn " Ok\nReading PGF..."
--  pgf <- readPGF pgf' --File
--  putStrLn " Ok\n"
--  let morpho = buildMorpho pgf lang'
--  c <- execStateT (mapM_ (tryparse pgf morpho lang') sents) (newCount strict out)
--  print c
--  return c

tryparse :: PGF -> Morpho -> Language -> Sent -> CState (Maybe Tree)
tryparse pgf morpho lang sents = do 
  count 
  parse' pgf morpho lang sents

parse' :: PGF -> Morpho -> Language -> Sent -> CState (Maybe Tree)
parse' pgf morpho lang (s,"") = do 
   addEmpty
   putEmptyMsg (s,"")
   return Nothing
parse' pgf morpho lang s = do
  let fix     = fixPunctuation $ replaceNumbers s 
      unknown = badWords morpho (snd fix)
  strict <- gets strict
  out    <- gets outputFile
  if null unknown then parseOk pgf lang fix
        else do putLexMsg s unknown
                io $ appendFile out (formatRes strict (s,[])++"\n")
                addBadLex
                return Nothing

parseOk :: PGF -> Language -> Sent-> CState (Maybe Tree)        
parseOk pgf lang s = do
  strict <- gets strict
  let eval = if strict then ($!) else ($)
  tree <- io $ timeout timeLimit $ return `eval` parse pgf lang textType (map toLower $ snd s)
  maybe (addTime >> putTimeMsg s) (getBestTree s) tree
  return $ maybe Nothing (Just . head) tree

testa :: String -> IO ()
testa s = do
  pgf <- readPGF "BigParse.pgf"
  let morpho = buildMorpho pgf (read "BigParseSwe")
  print $ lookupMorpho morpho $ map toLower s
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
getBestTree :: Sent -> [Tree] -> CState ()
getBestTree s tree = do
  --let treelist = map (rank . toPars) tree
  --let newTrees = map t $ sortBy (comparing n) treelist
  io $ putMsg s tree
  unless (null tree) addOk
  strict <- gets strict
  out    <- gets outputFile
  io $ appendFile out (formatRes strict (s,tree)++"\n")
  -- this is for play, remove
 -- io $ rawSystem "dot -Tpdf"
 -- graphvizParseTree 
 -- io $ putStrLn (formatRes True (s,tree)++"\n")

addOk,addEmpty, addBadLex :: CState ()
addOk     = modify $ \s -> s {ok = succ (ok s)}
addEmpty  = modify $ \s -> s {reject = succ (reject s)}
addBadLex = modify $ \s -> s {lexErr = succ (lexErr s)}
addTime   = modify $ \s -> s {time = succ (time s)}
count     = modify $ \s -> s {total = 1 + total s}

putTimeMsg s   = output s blue   " was timed out" 
putEmptyMsg s  = output s pink " was rejected"  
putLexMsg s ws = output s red 
                        (" was not parsed (unknown words: "
                             ++unwords ws++")" )

output :: Sent -> Color -> String -> CState ()
output s c str = do
  io $ putStrLn $ show s ++ color c str 
  io $ appendFile outFile $ "\n" ++ show s ++ str


textType = fromJust $ readType "Text"
uttType  = fromJust $ readType "Phr"
langOld  = fromJust $ readLanguage "BigSwe" --"BigSwe" --"BigNewSwe"
pgfFile =  "../gf/Big.pgf" -- "../gf/BigTest.pgf" -- "../gf/Big.pgf" -- "BigNew.pgf"
outFile = "testisNP.txt"


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

newCount :: Bool -> FilePath -> Count
newCount = C 0 0 0 0 0

instance Show Count where
  show (C ok rej tot lex time _ _) 
     = "*******\n Result: passed: "++show ok++"\t rejected: "
       ++show rej++"\t timed out: "++show time++"\n\t of totally "
       ++show tot++" sentences."++"\n"++show lex++" had unknown words"

isStrict (x:xs) = ("S" `isPrefixOf` x)
isStrict []     = False


-------
formatRes strict (s,[])  | strict =  show s ++ " was not Parsed"
                         | otherwise = show (fst s,"0")
formatRes strict (s,trs) | strict = show s ++ "("++show (length trs)++" trees)" 
                                    ++ ":\n" ++ unlines (map (showExpr []) trs) 
                         | otherwise = show (fst s,show $ length trs)


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

putMsg :: Sent -> [Tree] -> IO ()
putMsg s [] = putStrLn $ show s ++ color red " was not parsed"
putMsg s ps = putStrLn $ show s ++ (color green " was parsed in "
                                      ++show (length ps)++" ways")

--- graphviz, by Thomas
pipeIt2graphviz :: String -> IO BS.ByteString
pipeIt2graphviz code = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc "dot" ["-T","png"])
                      { std_in  = CreatePipe,
                        std_out = CreatePipe,
                        std_err = Inherit }

    hSetEncoding outh latin1
    hSetEncoding inh  utf8

    -- fork off a thread to start consuming the output
    output  <- BS.hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (BS.length output) >> putMVar outMVar ()

    -- now write and flush any input
    hPutStr inh code
    hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> fail ("pipeIt2graphviz: (exit " ++ show r ++ ")")

