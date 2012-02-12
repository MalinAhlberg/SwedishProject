module NewerChunk where 
import PGF hiding (Tree)
import Data.Tree
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.Either
import Debug.Trace
import Types

data PState = PS {recTypes :: [[Type]], recState :: [ParseState], currentState :: ParseState }
type Parser = State PState

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  let startState = initState pgf lang startType
  let pst = evalState (parseX tree) (PS [[s]] [startState] startState)
  case fst (getParseOutput (fromEither pst) startType Nothing) of
       ParseOk trees   -> return trees
       ParseIncomplete -> putStrLn "incomplete" >> return []
       TypeError x     -> putStrLn "type error" >> return []
       ParseFailed i   -> putStrLn "parse fail" >> return []
 where fromEither :: Either ParseState ParseState -> ParseState
       fromEither (Left  st) = st
       fromEither (Right st) = st

--TODO
-- 1. problem: v parsas ej som ? utan som alla kombinationer 
--      -> byt ut okända Vn i tb-trädet mot tex VX som vi lägger till.
-- 2. om vi har en okänd i, typ +A vad göra då?
-- ha ?S VP NP .. se påbörjad, men kan kanske avhjälpas av mer taggar??
-- 3. ta hand om punktation (PU mm) (tex om vi inte kan parsa ett komma, skit i det?
-- eller lägg till 'och'?
-- paranteser får delas upp


-- parse the words
parseX ::  Tree String -> Parser (Either ParseState ParseState)
parseX (Node w []) = trace ("parsing "++w) $ let nextTok = simpleParseInput w in
  do state <- gets currentState
     case nextState state nextTok of
          Right ps  -> do trace ("parse success "++w) $ putCurrentState ps
                          return $ Right ps
          Left  er  -> do types  <- getRecTypes
                          rstate <- getRecState
                          let ps' = recoverFrom types rstate 
                          trace ("parse fail "++w) $ putCurrentState ps'
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
parseX (Node "S"  ts) = do 
   saveState [s] ts
--   Right pst <- gets currentState 
--   case fst $ getParseOutput nextSt s Nothing of
--        ParseOk trees -> return (Rigth pst)
--        _             -> do let vps = filter (`elem` ["FV","VG","OA","OO","NA","PL"])
--                                ss  = filter (`elem` ["SS"])

parseX (Node "NP" ts) = saveState  nps ts
parseX (Node "PP" ts) = saveState  advs ts
parseX (Node "SS" ts) = saveState [npsub] ts
parseX (Node "OO" ts) = saveState (advs{- ++nps-}) ts
parseX (Node "OA" ts) = saveState (advs {- ++nps-}) ts
parseX (Node "TA" ts) = saveState  advs ts
parseX (Node "XA" ts) = saveState  advs ts
parseX (Node "VA" ts) = saveState  advs ts
parseX (Node "MA" ts) = saveState  advs ts
parseX (Node "KA" ts) = saveState  advs ts
parseX (Node "CA" ts) = saveState  advs ts
parseX (Node "AA" ts) = saveState  advs ts
parseX (Node "+A" ts) = saveState  advs ts
parseX (Node "FV" ts) = saveState [v] ts --,v2,v3,v2a,vs,vq,va,v2s,v2q,vv] ts
parseX (Node "IV" ts) = saveState [v] ts --,v2,v3,v2a,vs,vq,va,v2s,v2q,vv] ts
-- FO mfl?

-- otherwise, just continue
parseX (Node x [t]) = trace ("one left in "++x ) $ parseX t
parseX (Node x (t:ts)) = trace ("many left in "++x) $ do 
  res1 <- parseX t 
  case res1 of
      Right ps -> parseX (Node x ts)
      Left  ps -> return (Left ps)

saveState ::  [Type] -> [Tree String] -> Parser (Either ParseState ParseState)
saveState recover ts = do
  state   <- gets currentState
  rstates <- gets recState
  rtypes  <- gets recTypes
  pushState state
  pushRecTypes recover
  mapM parseX ts -- correct??
  res <- gets currentState
  pushState res
  trace ("saveState: "++show recover) (popStates rstates rtypes)
  return (Right res)


putCurrentState ::  ParseState -> Parser ()
putCurrentState ps = modify $ \s -> s {currentState = ps}

getRecTypes ::  Parser [Type]
getRecTypes = liftM head $ gets recTypes --or [] if empty

getRecState ::  Parser ParseState
getRecState = liftM head $ gets recState -- or init if empty (should never happen?)

pushState ::  ParseState -> Parser ()
pushState st = trace ("push State ") $  modify $ \s -> s {recState = st  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = trace ("push types "++ show typ) $ modify $ \s -> s {recTypes = typ : recTypes s}

popStates :: [ParseState] -> [[Type]] ->  Parser ()
popStates ps t = modify $ \s -> s {recTypes = t, recState = ps}



instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"








