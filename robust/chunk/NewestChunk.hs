{-# LANGUAGE ViewPatterns #-}
module NewestChunk where 
import PGF hiding (Tree)
import Control.Applicative
import Control.Arrow hiding ((+++))
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
 
--TODO limit trees! chunking didn't really help, we must look for nice places
--     to split the sentence. IK (not IK++) or other skip nodes

data PState = PS {currentStates :: [String]       --the current parse state
                 ,gfParser      :: Type -> String -> [Expr]
                 ,rankThem      :: [Expr] -> [Expr]
                 ,chunks        :: [(Id,[Expr])]  --list of expressions to replace the ints with later
                 ,saveChunks    :: [(Id,[Expr])]  
                 ,counter       :: Int             --counter for replacing
                 ,lastExpr      :: Maybe (Id,[Expr])
--                 ,backups       :: 
                 }
type Parser = WriterT [String] (State PState)
type Id     = String


limit, chunklimit, disambiglimit :: Int
limit = 500
chunklimit = 20
disambiglimit = 700
backuplimit = 20
saveStatelimit = 8 

parseText :: Tree (Id,String) -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  -- parse the flat string!!
  let (trejs,st) = runState (execWriterT (parseX tree)) parser 
  appendFile "lastfancyoutput" (unlines $ trejs++["\n"])
  let res = lastExpr st
 -- let strs = currentStates st 
  -- testa att byta alla adv mot adV
--  ok <- limitAndRank <$> findParse {-(sortBy (comparing noMetas)-} strs
  --putStrLn $ "string 1: "++concat (take 1 strs)
  case res of 
       Nothing       -> do putStrLn "No parse trees :("
                           combinePieces st
       Just (_,expr) -> do let unparsed = saveChunks st
                               pieces = map (\(i,expr) -> (mkApp (mkCId i) expr)) unparsed
                           putStrLn $ "saved chunks: "++show (length $ saveChunks st)
                           let res = limitAndRank expr
                           putStrLn $ "have got a result " -- ++show (length expr)
                           return (res ++pieces)

--  if null ok then do putStrLn "No parse trees :("
--                     combinePieces st
--             else do let unparsed = saveChunks st
--                         pieces = limitAndRank $ map (\(i,expr) -> (mkApp (mkCId i) expr)) unparsed
--  --                   putStrLn $ "saved chunks: "++show (saveChunks st)
--                     return (ok++pieces)

 where parser     = initPState pgf lang startType
      
      -- TODO rank it if possible, otherwise get chunks and rank them
       --findParse :: [String] -> IO [Expr]
       --findParse []  = return []
       --findParse (st:sts) = do
       --    putStrLn $ "parsing "++ st
       --    let parsed = parse pgf lang phrText st
       --    case parsed of
       --         []    -> findParse sts
       --         trees -> return trees

       limitAndRank,limitAndRankChunk :: [Expr] -> [Expr]  -- TODO use threshold!!
       limitAndRank = limitAndRank' limit
       limitAndRankChunk = limitAndRank' chunklimit
       limitAndRank' i exs = case length (take topLimit exs) of
                                  toplimit -> splitINPUT!! --TODO START here. 
                                  _        ->  take i $ map fst
                                             $ rankTreesByProbs pgf exs

       --parseToks :: Token -> Either a ParseState -> Either a ParseState
       parseToks tok (Right parseState) = nextState parseState (simpleParseInput tok)
       parseToks tok failure            = failure
       noMetas :: [String] -> Int
       noMetas = length . filter (=='?') . concat

       combinePieces st = do
         let chunkorder = sequence $ map (limitAndRankChunk . snd) $ saveChunks st -- ++ chunks st
         return $ take limit $ map (\ex -> mkApp meta ex) chunkorder
       
initPState pgf lang startType =  
  let startS = startState pgf lang startType
  in  PS { --recTypes = [[sent]], recState =  [[Ok $ startS ]], skip = [False]
          currentStates = [""] --, pieces =  []
         ,gfParser = parse pgf lang
         --,emptyPState = initState pgf lang
         ,chunks = [], counter = 0 --, isInnerS = 0, gettingPieces = False
         ,saveChunks = []
         ,rankThem   = map fst . rankTreesByProbs pgf
         ,lastExpr   = Nothing 
         --,sentences = [], pgf = pgf
         }

startState pgf lang startType = initState pgf lang startType

-- parse the words
parseX' ::  Tree (Id,String) -> Parser String
parseX' (Node (i,w) []) = do
-- bad idea? is word unknown? maybe XX if it is. then 
  putTrace $ "add word "++w
  addWord w
  -- TODO lemma this word, and add it as a chunk
  return w                         

-- some nouns don't have surrounding NP, but should be roubust anyway
parseX' (Node (i,x) ts) 
     | "NN" `isPrefixOf` x && length x>2 = parseX (Node (i,"NN") ts) -- put all variants of NNxxx to NN

     | isSaveNode x        =  {-# SCC "parseXSave" #-}
                              putTrace ("saves node" ++show x)
                           >> saveState (getCat x) i ts
     | isSkipNode x        = enableSkip ts

--"VN" ("AN" but could also be apposition)
-- otherwise, just continue
parseX' (Node x [t]) = putTrace ("one left in "++show x ) 
                    >> parseX t 
parseX' (Node (i,x) (t:ts)) = do
  putTrace ("many left in "++ x)   
  w  <- parseX t               
  ws <- parseX (Node (i,"XX") ts)  -- continue, but don't use x anymore
  return (w+++ws)

isSaveNode = (`elem` map fst saveNodes)


saveNodes = --("NP",nps),("PP",[adv]), ("FV",[v]),("IV",[v]),
            [("SS",[npsub{-,np-}]),("OO",[npobj]) --,np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("RA",advs),("+A",advs), ("CNP",nps),("AT",[ap])
            ,("ES",[npsub]),("FS",[npsub]),("EO",[npobj]),("FO",[npobj])
            --,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            --,("CPP",[adv]),("CS",[sent]),("CVP",[vpx]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp]) --,("VP",[vpx])
            ,("NN",nps),("ROOT",[phrText])] --TODO change v to all vs

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]

getCat :: String -> [Type]
getCat a = DB.trace ("fromJust on Cat "++a)$ (fromJust . (`lookup` saveNodes)) a
getStr a id = DB.trace ("fromJust on Str "++show a)$ (fromJust (toGFStr a) +++id)

saveState ::  [Type] -> Id -> [Tree (Id,String)] -> Parser String
saveState recover id ts = do
  begin   <- emptyCurrentStates
  oldChunks <- emptyChunks
  str     <- unwords <$> mapM parseX ts
  parser  <- gets gfParser
  ranker  <- gets rankThem
  -- TODO if more than chunklimit, rank them and add metas, add to savestate
  let parsable = concatMap (\typ -> parser typ str) recover
  fixed <- gets currentStates
  let parsefixed = findOkSentence fixed parser
  newChunks <- emptyChunks
  putChunks oldChunks
  putTrace $ "chunk parsed "++ str++" as "++show recover++" got "
                            ++show (map (showExpr []) (take 1 parsable))
  putTrace $ "chunk parsed fixed "++ intercalate "," fixed
  -- if we can't parse the chunk, we save its subchunks and adds a dummy word to the sentence
  case length parsable of
       0          ->if null parsefixed 
                       then do let types = getStr recover id
                               putTrace $ "could not parse chunk "++id
                               putString (map (+++types) begin)
                               addSaveChunks newChunks
                               setLastExpr Nothing
                       else do putTrace $ "could only parse fixed chunk "++id
                               addChunks [(id,parsefixed)] -- :newChunks
                               putString (concatMap (\fixStr -> map (+++fixStr) begin) fixed)
                               setLastExpr $ Just (id,parsefixed)
       n           | n > limit -> do putTrace $ "will have to limit chunk "++show n
                                     let expr = ranker parsable
                                         types = getStr recover id
                                     putString (map (+++types) begin)
                                     addSaveChunks [(id,expr)]
                                     setLastExpr $ Just (id,expr)
                   | otherwise      -> -- if we can parse it, we add the tree to the state
                                       -- and don't save the dummy words from within the chunk
                                      do putTrace $ "could parse chunk "++id
                                         putTrace $"don't have to limit chunk "++show n
                                         addChunks [(id,parsable)]
                                         putString (map (+++str) begin)
                                         setLastExpr $ Just (id,parsable)
  return str
 where findOkSentence :: [String] -> (Type -> String -> [Expr]) -> [Expr]
       findOkSentence fixed parser = concat <$> take 1 
                                   $ filter (not. null ) 
                                   $ map tryParse fixed
          where tryParse fixStr = concatMap (\typ -> parser typ fixStr) recover


enableSkip :: [Tree (Id,String)] -> Parser String 
enableSkip ts = do
  begin <- gets currentStates
  str <- concat <$> mapM parseX ts
  putTrace $ "skip processed "
  addStrings begin
  return str
 
addChunks :: [(Id,[Expr])] -> Parser ()
addChunks c = modify $ \s -> s {chunks = chunks s++c} 
addSaveChunks :: [(Id,[Expr])] -> Parser ()
addSaveChunks c = do 
  ranker <- gets rankThem
  let cs = map (shorten ranker) c
  modify $ \s -> s {saveChunks = chunks s++cs} 
 where shorten ranker (i,ex) = (i,if length ex > chunklimit then ranker ex else ex)

emptyChunks :: Parser [(Id,[Expr])] 
emptyChunks = do
  c <- gets chunks
  modify $ \s -> s {chunks = []} 
  return c

putChunks :: [(Id,[Expr])] -> Parser ()
putChunks c = modify $ \s -> s {chunks = c} 

-- exchange the sentences
putString :: [String] -> Parser ()
putString strs = modify $ \s -> s {currentStates = strs} 

-- adds a word to all the sentences
addWord ::  String -> Parser ()
addWord str = do
  modify $ \s -> s {currentStates = map (+++str) $ currentStates s}
  x <- gets currentStates
  putTrace $ "in current "++unwords (take 1 x)

-- add a new sentence
addStrings :: [String] -> Parser ()
addStrings strs = modify $ \s -> s {currentStates = currentStates s++strs}

emptyCurrentStates :: Parser [String] 
emptyCurrentStates = do
  c <- gets currentStates
  modify $ \s -> s {currentStates = [""]} 
  return c

setLastExpr :: Maybe (Id,[Expr]) -> Parser ()
setLastExpr e = modify $ \s -> s {lastExpr = e}


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

meta = mkCId "?" 

(+++) :: String -> String -> String
a +++ b = a ++" "++b
