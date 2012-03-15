{-# LANGUAGE ViewPatterns #-}
module NewestChunk where 
import PGF hiding (Tree)
import Control.Applicative
import Control.Arrow
import Control.Monad.Writer.Lazy hiding (ap)
import Control.Monad.State.Lazy hiding (ap)
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Data.Either
import Data.Tree
import qualified Debug.Trace as DB
import System.IO
import Types

-- IDEA: save a [String] during the tree processing,
-- possibly rank them, then parse at the end.
-- Keep the node numbers, so that we can reconstruct the tree later
-- TODO clean up Types, implement chunking for ambiguous sentences, or with many trees!
-- if words are unknown, we should know this by now!

data PState = PS {currentStates :: [[String]]       --the current parse state
                 ,gfParser      :: Type -> String -> [Expr]
                 ,replaced      :: [(Int,[Expr])]  --list of expressions to replace the ints with later
                 ,counter       :: Int             --counter for replacing
                 }
type Parser = WriterT [String] (State PState)


limit, chunklimit, disambiglimit :: Int
limit = 500
chunklimit = 200
disambiglimit = 700
backuplimit = 20
saveStatelimit = 8 

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  let (trejs,st) = runState (execWriterT (parseX tree)) parser 
  appendFile "newfancyoutput" (unlines $ trejs++["\n"])
  let strs = currentStates st 
  ok <- limitAndRank pgf <$> findParse (sortBy (comparing noMetas) strs)
  putStrLn $ "string 1: "++unwords (concat $ take 1 strs)
  when (null ok) $ putStrLn "No parse trees :("
  return ok


 where parser     = initPState pgf lang startType
      
      -- TODO rank it if possible, otherwise get chunks and rank them
       findParse :: [[String]] -> IO [Expr]
       findParse []  = return []
       findParse (st:sts) = do
           putStrLn $ "parsing "++unwords st
           --let parse  = foldr parseToks (Right $ startState pgf lang phrText) st
           let parsed = parse pgf lang phrText (unwords st)
           case parsed of
                --Right pst -> do
                []    -> findParse sts
                trees -> return trees
                  --let output = fst ({-# SCC "getParseOutput1" #-}getParseOutput pst phrText Nothing)
                  --case output of
                   --    ParseOk tr -> return tr 
                   --    _          -> findParse sts

                --- _         -> findParse sts

       limitAndRank :: PGF -> [Expr] -> [Expr]  -- TODO use threshold!!
       limitAndRank pgf = take limit . map fst . rankTreesByProbs pgf

       --parseToks :: Token -> Either a ParseState -> Either a ParseState
       parseToks tok (Right parseState) = nextState parseState (simpleParseInput tok)
       parseToks tok failure            = failure
       noMetas :: [String] -> Int
       noMetas = length . filter (=='?') . concat
       
       
initPState pgf lang startType =  
  let startS = startState pgf lang startType
  in  PS { --recTypes = [[sent]], recState =  [[Ok $ startS ]], skip = [False]
          currentStates = [[]] --, pieces =  []
         ,gfParser = parse pgf lang
         --,emptyPState = initState pgf lang
         ,replaced = [], counter = 0 --, isInnerS = 0, gettingPieces = False
         --,sentences = [], pgf = pgf
         }

startState pgf lang startType = initState pgf lang startType

-- parse the words
parseX' ::  Tree String -> Parser [String]
parseX' (Node w []) = do
  putTrace $ "add word "++w
  addWord w
  return [w]                         

-- some nouns don't have surrounding NP, but should be roubust anyway
parseX' (Node x ts) | "NN" `isPrefixOf` x && length x>2 = parseX (Node "NN" ts) -- put all variants of NNxxx to NN

                    | isSaveNode x        =  {-# SCC "parseXSave" #-}
                                             putTrace ("saves node" ++show x)
                                          >> saveState (getCat x) ts
                    | isSkipNode x        = enableSkip ts

-- otherwise, just continue
parseX' (Node x [t]) = putTrace ("one left in "++x ) 
                    >> parseX t 
parseX' (Node x (t:ts)) = do
  putTrace ("many left in "++x)   
  w  <- parseX t               
  ws <- parseX (Node "XX" ts)  -- continue, but don't use x anymore
  return (w++ws)

isSaveNode = (`elem` map fst saveNodes)


saveNodes = --("NP",nps),("PP",[adv]), 
            [("SS",[npsub{-,np-}]),("OO",[npobj,np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",nps)
            --,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            --,("CPP",[adv]),("CS",[sent]),("CVP",[vpx]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp]) --,("VP",[vpx])
            ,("NN",[np])] --TODO change v to all vs

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]

getCat :: String -> [Type]
getCat a = DB.trace ("fromJust on"++a)$ (fromJust . (`lookup` saveNodes)) a
getStr a = DB.trace ("fromJust on"++show a)$ toGFStr a 

saveState ::  [Type] -> [Tree String] -> Parser [String]
saveState recover ts = do
  begin <- gets currentStates
  str <- concat <$> mapM parseX ts
  parser <- gets gfParser
  let parsable = concatMap (\typ -> parser typ (unwords str)) recover
  putTrace $ "chunk parsed "++unwords str++"as "++show recover++" got "++show (map (showExpr []) (take 1 parsable))
  when (null parsable) $ do let types = fromJust $ getStr recover
                            addNews (map (++[types]) begin)
  return str
  
enableSkip :: [Tree String] -> Parser [String] 
enableSkip = saveState [] 

addWord ::  String -> Parser ()
addWord str = do
  modify $ \s -> s {currentStates = map (++[str]) $ currentStates s}
  x <- gets currentStates
  putTrace $ "in current "++unwords (concat $ take 1 x)

addNews :: [[String]] -> Parser ()
addNews strs = modify $ \s -> s {currentStates = currentStates s++strs}

putTrace ::  String -> Parser ()
putTrace str = do
  c <- gets currentStates
  let m = length c
  tell [show m ++","++str]

parseX tree = do
  putTrace $ "parseX with tree "++show tree
  parseX' tree

instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"++unlines (map (showExpr []) tree)
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"

