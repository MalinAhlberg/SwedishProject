module NewerChunk where 
import PGF hiding (Tree)
import Control.Monad.State hiding (ap)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Data.Either
import Data.Tree
import Types

data PState = PS {recTypes      :: [[Type]]       --stack of types which unknown words should be parsed as
                 ,recState      :: [[Result]]     --stack of states where parsing should be recovered from
                 ,skip          :: [Bool]         --stack of booleans telling whether a word may be skipped
                 ,currentStates :: [Result]       --the current parse state
                 ,pieces        :: [[Expr]]       --relevant pieces parsed so far
                 ,emptyPState   :: Type -> ParseState --an empty parse state
                 ,trace         :: [String]
                 }
type Parser = State PState
data Result = Ok ParseState | Recover ParseState | Failed 
instance Eq Result where
    (Ok _)      == (Ok _)      = True
    (Recover _) == (Recover _) = True
    Failed      == Failed      = True
    _           == _           = False
type Success = Bool

limit = take 500

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  let startState = initState pgf lang startType
      parser     = PS {recTypes = [[s]], recState =  [[Ok startState]], skip = [False]
                      ,currentStates = [Ok startState], pieces =  []
                      , emptyPState = initState pgf lang, trace = []}
      (pst,st) = runState (parseX tree) parser 
  writeFile "output" (unlines $ trace st)
  getOutput pst st

 where getOutput pst st | pst && not (best==Failed) && canProduce = return $ limit trees
                        | otherwise                               = putPiecesTogether st
        where best       = getBest $ currentStates st
              canProduce = isOkParse result
              trees      = getTrees result
              result     = fst (getParseOutput (getPState best) startType Nothing)
              getTrees (ParseOk t) = t
              putPiecesTogether st = let piec = pieces st in
                    return $ [mkApp (mkCId "Text") [mkApp meta c] | c <- combinations $ map (limit) piec] -- need some sort of limit
              combinations = sequence
              isOkParse     (ParseOk t) = True
              isOkParse     _           = False

{-
  case pst of -- take first, if all failed, put pieces together --TODO case for getBest returns 
       True         -> case getBest $ currentStates st of
                            Ok ps      -> getOutput ps
                            Recover ps -> getOutput ps
                            Failed     -> putPiecesTogheter st
       _            ->  putPiecesTogether st
       -}
       --getOutput pst = case fst (getParseOutput pst startType Nothing) of
       --                 ParseOk trees   -> return $ limit trees
       --                 ParseIncomplete -> putStrLn "incomplete" >> putPiecesTogether st
       --                 TypeError x     -> putStrLn "type error" >> putPiecesTogether st
       --                 ParseFailed i   -> putStrLn "parse fail" >> putPiecesTogether st



--TODO
-- 0. recover suddar ut mer än ett ord. hur lösa??
-- 1. problem: v parsas ej som ? utan som alla kombinationer 
--      -> byt ut okända Vn i tb-trädet mot tex VX som vi lägger till.


-- parse the words
parseX ::  Tree String -> Parser Success
parseX (Node w []) | length (words w) == 0 = return True --for removed names 
                   | length (words w) > 1  = liftM last $ mapM (parseX . flip Node []) $ words w -- compound
                   | otherwise             = 
  do states <- gets currentStates --parsa på alla states, ev lägg till fler
     newStates <- mapM parseNext states
     putCurrentStates $ concat newStates
     return True -- better check?

 where parseNext :: Result -> Parser [Result]
       parseNext (Ok state) = do
              putTrace ("parsing "++w) 
              skipOk <- getSkip
              putTrace ("skip ok: "++show skipOk) 
              let nextTok = simpleParseInput w 
              case nextState state nextTok of
                   Right ps  -> do putTrace ("parse success "++w)
                                   st <- backUpForAdv state
                                   let st' = if skipOk then [Ok state]
                                                       else []
                                   return ([Ok ps]++st++st')   
                   Left  er  -> do if skipOk then return [Ok state]
                                             else do
                                               types  <- getRecTypes
                                               rstate <- getRecState
                                               ps' <- mapM (recoverFrom types) rstate 
                                               putTrace ("parse fail "++w++", recover as "++show types) 
                                               return ps' --recover på vissa? vissa har försvunnit
         where recoverFrom :: [Type] -> Result -> Parser Result 
               recoverFrom typs (Ok state) = do
                      case toGFStr typs of
                           Just tok -> do
                                 putTrace ("recover word "++tok)
                                 let  nextTok  =  simpleParseInput tok
                                      newState = nextState state nextTok
                                 case newState of
                                      Right e  -> putTrace ("recover success") >> (return $  Recover e)
                                      Left  er -> putTrace ("recover fail")    >> (return $  Failed)
                           Nothing  -> do -- if not in list, we use normal recover
                                 let nextTok :: ParseInput 
                                     nextTok = simpleParseInput "XXX"
                                     lastSt = nextState state nextTok
                                 case lastSt of
                                      Right _  -> error "could parse XXX"
                                      Left  er ->  putTrace ("super recover") 
                                                >> return (Recover $ fst $ recoveryStates typs er)
               recoverFrom typs state = return state
       parseNext badState = return [badState]


-- some tags have a backup-plan
parseX (Node "ROOT" [t]) = parseX t -- only one node in root
parseX (Node "ROOT" ts)  = do       -- if there are more, we don't know what to do
  res <- parsePieces ts
  savePieces $ metatize res
  --st <- gets currentState 
  return False  


parseX (Node "S"  ts) = do 
   parseX (Node "XX" ts)
   st <- gets currentStates
   case getBest st of 
        Ok ps -> case fst (getParseOutput ps text Nothing) of
                         TypeError _ ->  putTrace "found a type error "
                                      >> combinePieces -- may be incomplete, because part of other sentence etc.
                         _           -> return True 
        _     -> combinePieces
 where combinePieces = do
         oldPieces <- emptyPieces 
         res       <- parsePieces ts
         let s = map (mkApp (mkCId "S")) $ sequence $ metatize res 
         savePieces $ s : oldPieces
         return False -- TODO what to do here? return False, or add s to state, or return True..?


parseX (Node x ts) | "NN" `isPrefixOf` x && length x>2 = parseX (Node "NN" ts)
                   | isSaveNode x        =  putTrace ("saves node" ++show x)
                                         >> saveState (getCat x) ts
                   | isSkipNode x        = enableSkip ts

-- otherwise, just continue
parseX (Node x [t]) = putTrace ("one left in "++x ) 
                    >> parseX t 
parseX (Node x (t:ts)) = do
  putTrace ("many left in "++x)   --will not need to be checked here anymore
  parseX t               --look at x here
  parseX (Node "XX" ts)  --but not again
--  case res1 of
--      Right ps -> parseX (Node ("XX"++x) ts) -- TODO get all Ok and continue, let the other sleep
--      Left  ps -> return (Left ps)

parsePieces :: [Tree String] -> Parser [[(Type,Result)]]
parsePieces ts = sequence [tryParse t | t <- ts] 
  where toNode (Node x _) = x
        fromEither :: Either ParseState ParseState -> ParseState
        fromEither (Left  st) = st
        fromEither (Right st) = st
        tryParse (Node t ts) = let cats = if isSaveNode t then (getCat t++[utt]) else [utt]
                               in sequence [do emptyState c
                                               parseX (Node t ts) 
                                               res <- getBest <$> gets currentStates
                                               return (c,res) | c <- cats]

isSaveNode = (`elem` map fst saveNodes)

saveNodes = [("NP",nps),("PP",[adv]),("SS",[npsub,np]),("OO",[npobj,np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",nps)
            ,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            ,("CPP",[adv]),("CS",[s]),("CVP",[vpx]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp]),("VP",[vpx])
            ,("NN",nps)] --TODO change v to all vs

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]

getCat :: String -> [Type]
getCat = fromJust . (`lookup` saveNodes)

saveState ::  [Type] -> [Tree String] -> Parser Success
saveState recover ts = do
  res <- gets currentStates
  let oks =  filter isOkResult res 
  case oks of
       [] -> return False
       _  -> do
            local (putCurrentStates oks >> pushRecTypes recover >> parseX (Node "XX" ts))
            news <- gets currentStates
            let recovered = cleanUp news
            putCurrentStates (recovered++filter (not . isOkResult) res)
            return (not $ null recovered) --if we have any left..
cleanUp = map recoverSt  -- . filter isAlive
  where isAlive                = not . (==Failed)   
        recoverSt (Recover st) = Ok st
        recoverSt x            = x
isOkResult (Ok _) = True
isOkResult _      = False

enableSkip :: [Tree String] -> Parser Success 
enableSkip ts = do
   modify $ \s -> s {skip = True:skip s}
   res <- parseX (Node "XX" ts)
   modify $ \s -> s {skip = drop 1 (skip s)}
   return res

getBest = head . sortBy (comparing resultOrder)
 where resultOrder (Ok _)      = 1
       resultOrder (Recover _) = 2
       resultOrder _           = 3

metatize :: [[(Type,Result)]] -> [[Expr]]
metatize = map putMetas
  where putMetas :: [(Type,Result)] -> [Expr]
        putMetas []               = [mkApp meta []]
        putMetas ((t,Failed):ps)  = [mkApp meta []]
        putMetas ((t,res):ps)     = 
           case fst (getParseOutput (getPState res) t Nothing) of 
                ParseOk trees   -> nub trees  
                _               -> putMetas ps

getPState (Recover ps) = ps
getPState (Ok      ps) = ps
getPState _            = error "getPState on Failed"

putCurrentStates ::  [Result] -> Parser ()
putCurrentStates ps = modify $ \s -> s {currentStates = ps}

--addCurrentState :: ParseState -> Parser ()
--addCurrentState ps = modify $ \s -> s {currentStates = currentStates s ++[ps]} --add last

getRecTypes ::  Parser [Type]
getRecTypes = liftM head $ gets recTypes 

getSkip ::  Parser Bool
getSkip  = liftM head $ gets skip 

getRecState :: Parser [Result]
getRecState = liftM head $ gets recState 

pushStates ::  [Result] -> Parser ()
pushStates st = do
  putTrace ("push State ")
  modify $ \s -> s {recState = st  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = do
  putTrace ("push types "++ show typ) 
  modify $ \s -> s {recTypes = typ : recTypes s}

popStates :: [[Result]] -> [[Type]] ->  Parser ()
popStates ps t = modify $ \s -> s {recTypes = t, recState = ps}

savePieces :: [[Expr]] -> Parser ()
savePieces exps = modify $ \s -> s {pieces = exps++ pieces s} 

local :: Parser a -> Parser a
local m = do
  state   <- gets currentStates
  rstates <- gets recState
  rtypes  <- gets recTypes
  pushStates $ {-filter (not . (== Failed))-} state
  res <- m
  popStates rstates rtypes
  return res

putTrace ::  String -> Parser ()
putTrace str = modify $ \s -> s {trace = str:trace s}

emptyState :: Type -> Parser ()
emptyState typ = do
  st <- gets emptyPState
  modify $ \s -> s {recTypes = [[text,utt]], recState = [[Ok $ st typ]], currentStates = [Ok $ st typ]
                   ,pieces = pieces s, emptyPState = st, skip = [False]}


emptyPieces :: Parser [[Expr]]
emptyPieces = do
  ps  <- gets pieces
  modify $ \s -> s {pieces = []}
  return ps


-- need list of states, remove as they fail and take the first if it succeedes.
backUpForAdv :: ParseState -> Parser [Result]
backUpForAdv state = do --TODO putState Later of sooner!
  types  <- getRecTypes
  if adV `elem` types then parseAsAdV
                               else return [] 
 where parseAsAdV = do
         putTrace "backing up for adv" 
         let nextTok = simpleParseInput $ fromJust $ toGFStr [adV] 
         case nextState state nextTok of
              Right st -> return [Ok st]
              _        -> return []
--addStateForSkippedToken state = state

meta = mkCId "?" 


