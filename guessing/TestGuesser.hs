module TestGuesser where
import PGF
import System.Environment (getArgs)
-- import Parser hiding (pgfFile,getLang)
import Color
import Control.Monad.State
import Data.List
import Data.Maybe
import Debug.Trace

data TestResult = Good String | Bad String | Unsure String
data Count = Count {good :: Int, bad :: Int, toKeep :: [(String,LexID)],
                    toRetry :: [[String]], single :: [(String,LexID)]}
type Tester a = StateT Count IO a
type Analyse = [(String,[Lemma])]
type LexID = String

nameVlist = [["verb"]]
-- read a pgfFile, and tries to analyse them, then prints results
main1 = do
    pgf <- readPGF pgfFile
    let morpho = buildMorpho pgf getLang
    (res,counts) <-runStateT (mapM (analyseAllSent morpho) nameVlist) emptyCount
    mapM_ showResult res
    putStrLn $ "Happy with "++show (good counts) 
                ++", sad about "++show (bad counts)
    putStrLn $ "To keep: "++ show (toKeep counts)

--- State stuff
emptyCount = Count 0 0 [] [] []
incrBad  :: Tester ()
incrBad    = modify $ \s -> s {bad = 1+bad s}
incrBadR :: [String] -> Tester ()
incrBadR w = modify $ \s -> s {bad = 1+bad s,toRetry = w:toRetry s}
incrGood,incrSingle :: (String,LexID) -> Tester ()
incrGood w = modify $ \s -> s {good = 1+good s, toKeep = w:toKeep s}
incrSingle (w,"ERR")  = modify $ \s -> s {toRetry = [w]:toRetry s}
incrSingle w = modify $ \s -> s { single = w : single s } 

showResult :: TestResult -> IO ()
showResult (Good s) = putStrLn $ color green s
showResult (Bad  s) = putStrLn $ color red   s
showResult (Unsure s) = putStrLn $ color yellow s 

analyseAllSent :: Morpho -> [String] -> Tester TestResult
analyseAllSent morpho str = do
    let res = [(w,map fst $ lookupMorpho morpho w) | w <- str]
    case str of
         [x] -> incrSingle (x,getID res) >> return (unsure x)
         xs  -> lookAtResult str res
  
lookAtResult str@(s:strs) res  
  | containsFails res = incrBadR str >> return (noParseFail str res)
  | containsNoDifferent res =
     incrGood (s,getID res) >> return (successMsg str)
  | otherwise = incrBad >> return (manyParseFail str (nub res))

getID :: Analyse -> LexID
getID x = if ([] `elem`) $ map snd x
            then "ERR"
            else (show . head . snd . head) x

noParseFail str res = Bad $ head str ++ " did not generate form/s "
                            ++ show (filter (null . snd) res)
successMsg str      = Good $ head str ++ " succedeed in generating "
                             ++ show (length str) ++" forms" 
manyParseFail str xs = Bad $ "Different solutions for forms of "
                             ++ head str ++": " ++ show xs
unsure str          = Unsure $ str ++ " only occured in one form"

containsNoDifferent :: Analyse -> Bool
containsNoDifferent = (==1) . length . lemmas

containsFails :: Analyse -> Bool
containsFails = ([] `elem`) . map snd

lemmas = nub . concatMap snd
pgfFile  = "Verb.pgf"
lang = "VerbSwe"
getLang :: Language
getLang = fromJust $ readLanguage lang
