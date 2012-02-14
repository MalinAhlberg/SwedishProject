module NewerChunk where 
import PGF hiding (Tree)
import Control.Monad.State hiding (ap)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Tree
import Types

data PState = PS {recTypes :: [[Type]]       --stack of types which unknown words should be parsed as
                 ,recState :: [ParseState]   --stack of states where parsing should be recovered from
                 ,skip :: [Bool]                    --stack of booleans telling whether a word may be skipped
                 ,currentState :: ParseState --the current parse state
                 ,pieces :: [[Expr]]         --relevant pieces parsed so far
                 ,emptyPState :: Type -> ParseState --an empty parse state
                 }
type Parser = State PState

limit = take 100

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  let startState = initState pgf lang startType
      parser     = PS {recTypes = [[s]], recState =  [startState], skip = [False]
                      ,currentState = startState, pieces =  [], emptyPState = initState pgf lang}
      (pst,st) = runState (parseX tree) parser 
  case pst of
       Right ps -> case fst (getParseOutput ps startType Nothing) of
                        ParseOk trees   -> return $ limit trees
                        ParseIncomplete -> putStrLn "incomplete" >> putPiecesTogether st
                        TypeError x     -> putStrLn "type error" >> putPiecesTogether st
                        ParseFailed i   -> putStrLn "parse fail" >> putPiecesTogether st
       Left _   ->  putPiecesTogether st
 where putPiecesTogether st = let piec = pieces st in
            return $ [mkApp (mkCId "Text") [mkApp meta c] | c <- combinations $ map (limit) piec] -- need some sort of limit
       combinations = sequence --[]     = [[]]
       --combinations (x:xs) = [a:as | a <- x , as <- combinations xs]
--TODO
-- 0. recover suddar ut mer än ett ord. hur lösa??
-- 1. problem: v parsas ej som ? utan som alla kombinationer 
--      -> byt ut okända Vn i tb-trädet mot tex VX som vi lägger till.


-- parse the words
parseX ::  Tree String -> Parser (Either ParseState ParseState)
parseX (Node w []) = trace ("parsing "++w) $ let nextTok = simpleParseInput w in
  do state <- gets currentState
     case nextState state nextTok of
          Right ps  -> do trace ("parse success "++w) $ backUpForAdv w ps
                          --putCurrentState ps
                          return $ Right ps
          Left  er  -> do skipOk <- getSkip
                          if skipOk then return (Right state)
                                    else do
                                         types  <- getRecTypes
                                         rstate <- getRecState
                                         let ps' = recoverFrom types rstate 
                                         trace ("parse fail "++w++", recover as "++show types) 
                                                   $ putCurrentState ps'
                                         return $ Left ps'
  where recoverFrom :: [Type] -> ParseState -> ParseState
        recoverFrom typs state = do
             let nextTok :: ParseInput 
                 nextTok = simpleParseInput "XXX"
                 lastSt = nextState state nextTok
             case lastSt of
                  Right _  -> error "could parse XXX"
                  Left  er -> fst $ recoveryStates typs er

              

-- some tags have a backup-plan
parseX (Node "ROOT" [t]) = parseX t -- only one node in root
parseX (Node "ROOT" ts)  = do       -- if there are more, we don't know what to do
  res <- parsePieces ts
  savePieces $ metatize res
  st <- gets currentState 
  return $ Left st


parseX (Node "S"  ts) = do 
   res <- parseX (Node "XX" ts)
   case res of 
        Right ps -> case fst (getParseOutput ps text Nothing) of
                         TypeError _ -> trace "found a type error " combinePieces -- may be incomplete, because part of other sentence etc.
                         _           -> return res
        Left  ps -> combinePieces
 where combinePieces = do
         oldPieces <- emptyPieces 
         res       <- parsePieces ts
         let s = map (mkApp (mkCId "S")) $ sequence $ metatize res 
         savePieces $ s : oldPieces
         st <- gets currentState 
         return $ Right st


parseX (Node x ts) | isSaveNode x = trace ("saves node" ++show x) $  saveState (getCat x) ts
                   | isSkipNode x = enableSkip ts


-- otherwise, just continue
parseX (Node x [t]) = trace ("one left in "++x ) $ parseX t 
parseX (Node x (t:ts)) = trace ("many left in "++x) $ do 
  res1 <- parseX t 
  case res1 of
      Right ps -> parseX (Node ("XX"++x) ts)
      Left  ps -> return (Left ps)

parsePieces :: [Tree String] -> Parser [[(Type,ParseState)]]
parsePieces ts = sequence [tryParse t | t <- ts] 
  where toNode (Node x _) = x
        fromEither :: Either ParseState ParseState -> ParseState
        fromEither (Left  st) = st
        fromEither (Right st) = st
        tryParse (Node t ts) = let cats = if isSaveNode t then (getCat t++[utt]) else [utt]
                               in sequence [emptyState c >> parseX (Node t ts) 
                                            >>= \x -> return (c,fromEither x) | c <- cats]

isSaveNode = (`elem` map fst saveNodes)

saveNodes = [("NP",[np]),("PP",[adv]),("SS",[npsub,np]),("OO",[np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",[np])
            ,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            ,("CPP",[adv]),("CS",[s]),("CVP",[vp]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp])] --TODO change v to all vs

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]

getCat :: String -> [Type]
getCat = fromJust . (`lookup` saveNodes)

saveState ::  [Type] -> [Tree String] -> Parser (Either ParseState ParseState)
saveState recover ts = do
  local (pushRecTypes recover >> parseX (Node "XX" ts))
  res <- gets currentState
  return (Right res)

enableSkip :: [Tree String] -> Parser (Either ParseState ParseState)
enableSkip ts = do
   modify $ \s -> s {skip = True:skip s}
   res <- parseX (Node "XX" ts)
   modify $ \s -> s {skip = drop 1 (skip s)}
   return res


metatize :: [[(Type,ParseState)]] -> [[Expr]]
metatize = map putMetas
  where putMetas :: [(Type,ParseState)] -> [Expr]
        putMetas []      = [mkApp meta []]
        putMetas ((t,p):ps)  = 
           case fst (getParseOutput p t Nothing) of 
                ParseOk trees   -> nub trees  
                _               -> putMetas ps

putCurrentState ::  ParseState -> Parser ()
putCurrentState ps = modify $ \s -> s {currentState = ps}

getRecTypes ::  Parser [Type]
getRecTypes = liftM head $ gets recTypes 

getSkip ::  Parser Bool
getSkip  = liftM head $ gets skip 

getRecState = liftM head $ gets recState 

pushState ::  ParseState -> Parser ()
pushState st = trace ("push State ") $  modify $ \s -> s {recState = st  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = trace ("push types "++ show typ) $ modify $ \s -> s {recTypes = typ : recTypes s}

popStates :: [ParseState] -> [[Type]] ->  Parser ()
popStates ps t = modify $ \s -> s {recTypes = t, recState = ps}

savePieces :: [[Expr]] -> Parser ()
savePieces exps = modify $ \s -> s {pieces = exps++ pieces s} 

local m = do
  state   <- gets currentState
  rstates <- gets recState
  rtypes  <- gets recTypes
  pushState state
  res <- m
  popStates rstates rtypes
  return res


emptyState :: Type -> Parser ()
emptyState typ = do
  st <- gets emptyPState
  modify $ \s -> s {recTypes = [[text,utt]], recState = [st typ], currentState = st typ
                   ,pieces = pieces s, emptyPState = st, skip = [False]}


emptyPieces :: Parser [[Expr]]
emptyPieces = do
  ps  <- gets pieces
  modify $ \s -> s {pieces = []}
  return ps


backUpForAdv w state = do
  types <- getRecTypes
  if adV `elem` types then  parseAsAdv
          else  putCurrentState state
 where parseAsAdv = do
         let nextTok = trace "backing up for adv" $ simpleParseInput w
             badTok = simpleParseInput  "XX"
         case nextState state nextTok of
              Right st -> case nextState st badTok of
                               Left er -> putCurrentState $ fst 
                                                $ recoveryStates advs er
                               _       -> error "could parse XX"

              _        -> trace  "bad" $ putCurrentState state
meta = mkCId "?" 


