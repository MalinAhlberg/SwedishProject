module TestGuessProg where
import PGF  
-- import ParseGF
import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Control.Arrow (first)
import TestGuesser
import WordGuess
import MkTables
import System.Process        (runCommand, waitForProcess)
import System.Directory      (doesFileExist)
import System.Environment    (getArgs)
import System.FilePath.Posix (addExtension)

-- todo: ta ord från irregswe om de finns exakt där.
-- ta alltid med hela stammen, om inget annat funkar. utan ändelse.
type TestState  = StateT TState IO
data Result       = R [String] [String] Code
type LoopResult   = (Code,[String])
data TState       = S {pgf :: PGF, name :: FilePath, retry :: Int,
                       oks :: [String],retries :: [[String]],code :: Code,
                       lng :: Language}

{- usage: runghc TestGuessProg verblist grammarName (-n noOfWords)
         if no number is given, 20 words will be picked  -}

{-
construct a grammar from words in imported file (V.list)
runs 3 times, as defined in WordGuess, ask the user to confirm each
word declination and finally writes the grammar NameFinal.gf
-}

{- To do: verbs in irregswe should be copied to the new grammar,
   the final grammar should not extend IrregSwe 
   -}
main = do
 (list:name:args) <- getArgs
 let n = getNum args
 vs     <- getVerbs list n
 state1 <- execStateT (loop vs) (firstState name)
 state2 <- execStateT (loop (retries state1)) (contState 1 name (code state1))
 state3 <- execStateT (loop (retries state2)) (contState 2 name (code state2))
 guessRound 3 [] (name++"3") (code state3) 
 stateF <- execStateT (mkFileLookNice name 3) (finalState name 3) 
 putStrLn "Bam bam bam"


loop :: [[String]] -> TestState () 
loop vs = do
 st <- get
 guessed <- io $ guessRound (retry st) vs (name st) (code st)
 checkPGF
 interactTest guessed
 extractGood 

--interactTest :: ([[String]],[Prefixed]) -> TestState ()
interactTest :: [[String]] -> TestState ()
--interactTest (vs,prefs) = do
interactTest vs = do
  pgfF <- gets pgf 
  lng  <- gets lng
  let morpho = buildMorpho pgfF lng
  res <- io $ execStateT (mapM (analyseAllSent morpho) vs) emptyCount
  mapM_ confirm (toKeep res)
  mapM_ updateRetries (toRetry res)
  --mapM_ (tryAddPrefs morpho) prefs
  yes <- io $ askAboutSingles (single res)
  when yes $ mapM_ (confirm . first (:[])) (single res) 
  
-- asks the user to confirm the word declined
confirm :: ([String],LexID) -> TestState ()
confirm = uncurry confirmP 
confirmP :: [String] -> LexID -> TestState ()
confirmP ws id = do
  st <- get
  let l = (read $  langName $ name st)
  tbl@(s,tbls) <- io $ getTables (pgf st) l id
  case tbls of
     []  -> updateRetries ws 
     _   -> do io $ putStrLn $ "Generated the table: \n"++ showSmallTable tbl
               askAboutTable ws tbl

{-
-- functions for declining prefixed words
tryAddPrefs ::  Morpho -> Prefixed -> TestState () 
tryAddPrefs morpho w =
 case lookupMorpho morpho (word w) of
  [] -> updateRetries (whole w)
  xs -> do res <- (sameStem w . fst . head) xs  -- or mapM
           stemForms (getMaybe [res]) w         -- fullista!

stemForms :: Maybe (Prefixed,Lemma) -> Prefixed -> TestState () 
stemForms Nothing           w = updateRetries (whole w) >> return ()
stemForms (Just (w,stemid)) _ = do
  st <- get
  l <- gets lng
  let stem = show stemid
  tbl@(s,tbls) <- io $ getTables (pgf st) l stem
  let forms = map (pref w++) $ getSmallForms tbl       
  updateCode $ addPrefixed (whole w:forms)

sameStem :: Prefixed -> Lemma -> TestState (Maybe (Prefixed,Lemma))
sameStem w l = do
  io $ putStrLn $ "Is "++whole w++" declined as "++show l++"?"
  ans <- io getLine
  case ans of
       ('y':_) -> return $ Just (w,l)
       ('n':_) -> updateRetries (whole w) >> return Nothing
       ('d':_) -> return Nothing
       _       -> sameStem w l

           -}
--- States 
firstState :: FilePath -> TState
firstState n = S {pgf = undefined, name = n, retry = 0,
                  oks = [], retries = [], code = mempty,
                  lng = fromJust $ readLanguage $ langName n}
contState :: Int -> FilePath -> Code -> TState
contState i n c = S {pgf = undefined, name = n++show i, retry = i,
                    oks = [], retries = [], code = c,
                    lng = fromJust $ readLanguage $ langName (n++show i)}
finalState :: FilePath -> Int -> TState
finalState n i = S {pgf = undefined, name = n++show i, retry = i,
                   oks = [], retries = [], code = mempty,
                   lng = fromJust $ readLanguage $ langName (n++show i)}

updateOks:: String -> TestState ()
updateOks w = modify (\s -> s {oks = w:oks s})
              >> (io $ putStrLn $ "updated oks "++w)
updateRetries :: [String] -> TestState ()
updateRetries w = modify (\s -> s {retries = w:retries s})
updateCode :: Code -> TestState ()
updateCode c = modify (\s -> s {code = c `mappend` code s})
 

--- functions for giving the verb constructors a good name
mkFileLookNice :: FilePath -> Int -> TestState ()
mkFileLookNice name i = do
  checkPGF
  correct abstractFile  name i  
  correct concreteFile name i
--  ta bort guess
                 
-- correct :: getCorrectFileFunction -> File -> Version -> TestState ()
-- changes the verb constructors to the word in infinitiv 
correct :: (FilePath -> FilePath) -> FilePath -> Int -> TestState ()
correct f name i = do   
     old <-  io $ readFile $ f $ name++show i
     pgf <- gets pgf
     l   <- gets lng
     let new = fix pgf l old
     io $ writeFile (f $ name++"Final") new 
  where fix pgfF l = 
         unlines . map (unwords . map (changeName pgfF name i l). words) . lines

changeName :: PGF -> FilePath -> Int -> CId -> String -> String
changeName pgf file i lang n
   | "_V" `isSuffixOf` n = getInf pgf lang n ++"_V"
   | oldName file i n    = newName file n
   | otherwise           = n
oldName file i name = (file++show i) `isPrefixOf` name
newName file   name = if langString `isSuffixOf` name 
                        then langName $ file++"Final"
                        else file++"Final"

getInf pgf lang s = let exp = fromJust (readExpr s) in
  normalize $ fromJust $ lookup inf $ head $ tabularLinearizes pgf lang exp

-- Table fuctions
askAboutTable :: [String] -> Table -> TestState () 
askAboutTable ws tbl@(s,_) = do
  io $ putStrLn "Do you want it? (y/n/a/d)"
  ans <- io getLine  
  case ans of
       ('y':_) -> updateOks s     
       ('n':_) -> updateRetries ws
       ('a':_) -> bigTable ws tbl
       ('d':_) -> return ()
       _       -> askAboutTable ws tbl

bigTable :: [String] -> Table -> TestState ()
bigTable ws tbl = do          
  io $ putStrLn $ showTable tbl
  askAboutTable ws tbl

askAboutSingles :: Show a => [a] -> IO Bool
askAboutSingles xs =
 case xs of
  [] -> return False
  _  -> do putStrLn "Do you want to look at the single words?"
           getAns True False

getAns :: a -> a -> IO a
getAns y n = getAns' [('y',y),('n',n)]

getAns' :: [(Char,a)] -> IO a
getAns' alts = do
  ans <- getLine
  case lookup (head ans) alts of
     Just a  -> return a
     Nothing -> putStrLn "Invalid answer" >> getAns' alts


-- File handling

-- takes the correct code and saves it to the state
extractGood :: TestState ()
extractGood = do
  file <- gets name
  okList <- gets oks
  abscode <- io $ readFile $ abstractFile file
  concode <- io $ readFile $ concreteFile file
  let abs  = correctLines okList abscode
  let conc = correctLines okList concode
  updateCode $ Code abs conc

-- compiles the pgf and update the state
-- saves the state to File.errlog and the correct code to Fileerr.gf 
checkPGF :: TestState ()
checkPGF = do
  n <- gets name
  pgfExist <- io $ mkPGF n
  unless pgfExist saveAndExit
  io $ putStrLn "reading pgf..."
  pgfF <- io $ readPGF (pgfVersion n)
  modify (\s -> s {pgf = pgfF, name = n})

-- moves the gf-files to the correct folder, and compiles the pgf
mkPGF :: FilePath -> IO Bool
mkPGF gfFile = do 
  let sweFile = concreteFile gfFile
  runCommand ( "cp "++ abstractFile gfFile++" ../gf") >>= waitForProcess 
  runCommand ( "cp "++ sweFile           ++" ../gf") >>= waitForProcess  
  putStrLn "Writing pgf..."
  gfProc <- runCommand $ "gf --make "++"../gf/"++ sweFile
  waitForProcess gfProc 
  ok <-  doesFileExist (pgfVersion gfFile)
  putStrLn $ "PGF file ok: "++ show ok
  return ok

-- saves the state and correct code, then fails
saveAndExit :: TestState ()
saveAndExit = do
  st <- get
  io $ writeGF (code st) (name st++"err")
  io $ writeFile (name st++".errlog") $ "Oks: "++show (oks st)
                                        ++"\nRetrying: "++show (retries st)
  error "PGF fail"

-- helper functions
getVerbs :: String -> Int -> IO [[String]]
getVerbs inp n = (take n . map words . lines) <$> readFile inp

getNum :: [String] -> Int
getNum args = case findArg "-n" args of
  Just n  -> read n
  Nothing -> 20

abstractFile = (`addExtension` "gf")
concreteFile = (`addExtension` "gf") . (++"Swe")
pgfVersion   = (`addExtension` "pgf")
langName     = (++"Swe")
langString   = "Swe"

getMaybe :: [Maybe a] -> Maybe a
getMaybe = listToMaybe . catMaybes

io :: IO a -> TestState a 
io = lift

----- from Parser
findArg :: String ->[String] -> Maybe String
findArg flag (x:file:_) | flag == x = Just file
findArg flag (x:xs)                 = findArg flag xs
findArg _    []                     = Nothing

--- form ParseGF
correctLines l = unlines . filter (inList l) . filter (not . null) . lines

inList l = (`elem` l) {-. getWord-} . head . words 
