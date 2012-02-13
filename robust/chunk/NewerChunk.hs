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

data PState = PS {recTypes :: [[Type]], recState :: [ParseState], currentState :: ParseState
                 ,pieces :: [Expr], emptyPState :: ParseState, skip :: [Bool]}
type Parser = State PState

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  let startState = initState pgf lang startType
  let (pst,st) = runState (parseX tree) (PS [[s]] [startState] startState [] startState [False])
  case pst of
       Right ps -> case fst (getParseOutput ps startType Nothing) of
                        ParseOk trees   -> return trees
                        ParseIncomplete -> putStrLn "incomplete" >> return [mkApp (mkCId "INCOMPLETE") []]
                        TypeError x     -> putStrLn "type error" >> return [mkApp (mkCId "TYPEERROR") []]
                        ParseFailed i   -> putStrLn "parse fail" >> return [mkApp (mkCId "PARSEFAIL") []]
       Left _   -> do let piec = pieces st
                      return [mkApp meta piec]

--TODO
-- 0. recover suddar ut mer än ett ord. hur lösa??
-- 0.5 lägg till en lösning som filtrerar bort adv och försöker igen? tex 'hur klok är nu en hund'
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
          Right ps  -> do trace ("parse success "++w) $ backUpForAdv w ps
                          --putCurrentState ps
                          return $ Right ps
          Left  er  -> do skipOk <- getSkip
                          if skipOk then return (Right state)
                                    else do
                                         types  <- getRecTypes
                                         rstate <- getRecState
                                         let ps' = recoverFrom types rstate 
                                         trace ("parse fail "++w++", recover as "++show types) $ putCurrentState ps'
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
--parseX (Node x (Node "S" ts):tss) = do
parseX (Node "ROOT" ts) = do
  res <- sequence [parseX t >>= emptyState | t <- ts] 
  savePieces $ metatize res
  st <- gets currentState 
  return $ Left st


parseX (Node "S"  ts) = do 
   ps <- saveState [s] ts
   st <- gets currentState 
   return $ Right st

  -- case ps of
  --      Right _ -> return ps
  --      Left  _  -> error "s is angry"{-
   {-case fst $ getParseOutput pst s Nothing of
        ParseIncomplete    -> trace "S fail, try recover" $ parseX (Node "!" [])
        _                  -> return (Right pst)
        -}

        --ParseOk trees -> return (Rigth pst)
--        _             -> do let vps = filter (`elem` ["FV","VG","OA","OO","NA","PL"])
--                                ss  = filter (`elem` ["SS"])

parseX (Node x ts) | isSaveNode x = trace ("saves node" ++show x) $  saveState (getCat x) ts
                   | isSkipNode x = enableSkip ts
{-
parseX (Node x (t:t':ts)) | isAdvNode x = do
    saveState (getCat advs)
    res <- parseX (Node x (t':ts)
    case res of
         Left ps -> -}

-- otherwise, just continue
parseX (Node x [t]) = trace ("one left in "++x ) $ parseX t --local (parseX t)
parseX (Node x (t:ts)) = trace ("many left in "++x) $ do 
  res1 <- parseX t --- local
  case res1 of
      Right ps -> parseX (Node ("XX"++x) ts)
      Left  ps -> return (Left ps)

isSaveNode = (`elem` map fst saveNodes)

saveNodes = [("NP",[np]),("PP",[adv]),("SS",[np]),("OO",[np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",[np])
            ,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            ,("CPP",[adv]),("CS",[s]),("CVP",[vp]),("NAC",[utt])
            ,("++",[conj])] --TODO change v to all vs

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


metatize :: [ParseState] -> [Expr]
metatize = map putMetas
  where putMetas :: ParseState -> Expr
        putMetas ps = 
           case fst (getParseOutput ps utt Nothing) of --utt??
                ParseOk trees   -> head trees  --obs head
                _               -> (mkApp meta []) 
            

putCurrentState ::  ParseState -> Parser ()
putCurrentState ps = modify $ \s -> s {currentState = ps}

getRecTypes ::  Parser [Type]
getRecTypes = liftM head $ gets recTypes --or [] if empty

getSkip ::  Parser Bool
getSkip  = liftM head $ gets skip 

getRecState = liftM head $ gets recState -- or init if empty (should never happen?)

pushState ::  ParseState -> Parser ()
pushState st = trace ("push State ") $  modify $ \s -> s {recState = st  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = trace ("push types "++ show typ) $ modify $ \s -> s {recTypes = typ : recTypes s}

popStates :: [ParseState] -> [[Type]] ->  Parser ()
popStates ps t = modify $ \s -> s {recTypes = t, recState = ps}

savePieces exps = modify $ \s -> s {pieces = exps++ pieces s} 

local m = do
  state   <- gets currentState
  rstates <- gets recState
  rtypes  <- gets recTypes
  pushState state
  res <- m
  popStates rstates rtypes
  return res


emptyState x = do
  st <- gets emptyPState
  modify $ \s -> s {recTypes = [[text,utt]], recState = [st], currentState = st
                   ,pieces = pieces s, emptyPState = st, skip = [False]}
  return (fromEither x)
 where fromEither :: Either ParseState ParseState -> ParseState
       fromEither (Left  st) = st
       fromEither (Right st) = st


backUpForAdv w state = do
  types <- getRecTypes
  if adV `elem` types then do
                 let nextTok = trace "backing up for adv" $ simpleParseInput w
                     badTok = simpleParseInput  "XX"
                 case nextState state nextTok of
                      Right st -> case nextState st badTok of
                                       Left er -> putCurrentState $ fst $ recoveryStates advs er
                                       _       -> error "could parse XX"
                      _        -> error "bad"
          else putCurrentState state 

meta = mkCId "?" 



