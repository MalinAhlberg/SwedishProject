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
import System.IO
import Types

data PState = PS {recTypes      :: [[Type]]       --stack of types which unknown words should be parsed as
                 ,recState      :: [[Result]]     --stack of states where parsing should be recovered from
                 ,skip          :: [Bool]         --stack of booleans telling whether a word may be skipped
                 ,currentStates :: [Result]       --the current parse state
                 ,pieces        :: [[Expr]]       --relevant pieces parsed so far
                 ,emptyPState   :: Type -> ParseState --an empty parse state
                 ,replaced      :: [(Int,[Expr])]
                 ,counter       :: Int
                 ,pgf           :: PGF
                 ,isInnerS      :: [()]
                 ,gettingPieces :: Bool
                 ,trace         :: [String]
                 }
type Parser = State PState
data Result = Ok ParseState | Recover ParseState | Failed 
instance Eq Result where
    (Ok _)      == (Ok _)      = True
    (Recover _) == (Recover _) = True
    (Failed)    == (Failed)  = True
    _           == _           = False

type Success = Bool

limit, chunklimit, disambiglimit :: Int
limit = 500
chunklimit = 200
disambiglimit = 700

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  let (success,st) = runState (parseX tree) parser 
  res <- limitOutput success st 
  appendFile "outputdis2" (unlines $ trace st++["\n"])
  return res
  --appendFile "outputdis" (unlines $ trace st'++["\n"])
  --getOutput success st'

 where parser     = PS {recTypes = [[sent]], recState =  [[Ok startState]], skip = [False]
                       ,currentStates = [Ok startState], pieces =  []
                       ,emptyPState = initState pgf lang
                       ,replaced = [], counter = 0, isInnerS = [], gettingPieces = False
                       ,pgf = pgf, trace = []}
       startState = initState pgf lang startType
      
       limitOutput :: Success -> PState -> IO [Expr]
       limitOutput success st | success && canProduce = return $ take limit $ rankThem trees
                                     --if limit <= length trees 
                                     --    then return trees
                                     --    else do 
                                     --     -- let dis = execState (disambigParse tree) parser 
                                     --     -- appendFile "outputdis" (unlines $ trace dis++["\n"])
                                     --     -- appendFile "comps" (unlines $ map show $ replaced dis)
                                     --      getOutput success st -- $ dis
                              | otherwise = getOutput success st
         where canProduce = isOkParse result
               result     = if isOkParse textoutput then textoutput
                                      else fst (getParseOutput (getPState best) utt Nothing) 
               textoutput = fst (getParseOutput (getPState best) startType Nothing) 
               best       = getBest $ currentStates st
               trees      = getTrees result
               getTrees  (ParseOk t) = t
               isOkParse (ParseOk t) = True
               isOkParse _           = False
        
               rankThem trees = map fst $ rankTreesByProbs pgf trees
               getOutput success st {-| success && canProduce = return $ take limit trees
                                    | otherwise             -}= take limit <$> putPiecesTogether st
                                                              --TODO limit shouldn't be needed here

               putPiecesTogether st = let piec = pieces st in
                     return $ [mkApp (mkCId "Text") [mkApp meta c] | c <- combinations $ map (take limit) piec] 
               combinations = sequence

-- parse the words
parseX ::  Tree String -> Parser Success
parseX (Node w []) | length (words w) == 0 = return True --for removed parts of names 
                   | length (words w) > 1  = liftM last  -- compounds
                                           $ mapM (parseX . flip Node []) 
                                           $ words w
                   | otherwise             = 
  do states <- gets currentStates 
     newStates <- mapM parseNext states
     putCurrentStates $ concat newStates
     return True 

 where parseNext :: Result -> Parser [Result]
       parseNext (Ok state) = do
              putTrace ("parsing "++w) 
              skipOk <- getSkip
              putTrace ("skip ok: "++show skipOk) 
              let nextTok = simpleParseInput w 
              case nextState state nextTok of
                   Right ps  -> do putTrace ("parse success "++w)
                                   st <- backUp{-ForAdv-} state
                                   let st' = if skipOk then [Ok state]
                                                       else []
                                   return ([Ok ps]++st++st')   
                   Left  er  -> do if skipOk then return [Ok state]
                                             else do
                                               types  <- getRecTypes
                                               rstate <- getRecState
                                               ps' <- mapM (recoverFrom types) rstate 
                                               putTrace ("parse fail "++w++", recover as "++show types) 
                                               return ps' 
       parseNext badState = return [badState]


parseX (Node "ROOT" [t]) = parseX t -- only one node in root
parseX (Node "ROOT" ts)  = do       -- if there are more, we don't know what to do
  res <- parsePieces ts
  --pgf       <- gets pgf
  savePieces $ res -- metatize pgf res
  return False  


--TODO maybe make better check so that 'så att ?s' funkar
parseX (Node "S"  ts) = do 
   inner   <- gets isInnerS
   oldPieces <- emptyPieces 
   piecing <- gets gettingPieces
   modify $ \s -> s {isInnerS = ():inner}
   parseX (Node "XX" ts)
   st  <- gets currentStates
   let failFunction = {-if piecing then -}combinePieces st oldPieces piecing-- else return True 
   res <- case getBest st of 
               Ok ps -> case fst (getParseOutput ps phrText Nothing) of
--                                TypeError _ ->  putTrace "found a type error "  
--                                             >> combinePieces st-- may be incomplete, because part of other sentence etc.
                                ParseOk x        -> return True
                                ParseIncomplete  -> if not (null inner) && not piecing  --is embedded, does not have to be complete
                                                       then putTrace ("inner and piecing "++ show (not $ null inner) ++ show (not piecing)) 
                                                            >> failFunction 
                                                       else return True
                                x                ->  putTrace ("failed on S " ++show x) >> failFunction
               _         -> putTrace "doesn't like best, failing" >> failFunction --TODO shouldn't we also do pieces, in case outer S succeeds, but innner fails?
   modify $ \s -> s {isInnerS = inner}
   return res
-- TODO START here. What happens?? Why does it start piecing when it should parse IP and be happy?

 where combinePieces st oldPieces piecing = do
   --      oldPieces <-
         emptyPieces 
         putTrace "emptying pieces, parsing news"
         res       <- parsePieces ts
         --pgf       <- gets pgf
         let s = map (mkApp (mkCId "S")) $ sequence res -- metatize pgf res 
         putTrace $ "got pieces "++show (map (showExpr []) s)
         savePieces $ s : oldPieces
         unless piecing $ recoverSentence st
         return False 
       recoverSentence st = do 
         rec <- mapM (recoverFrom [sent]) st
         putCurrentStates rec
 

-- some nouns don't have surrounding NP, but should be roubust anyway
parseX (Node x ts) | "NN" `isPrefixOf` x && length x>2 = parseX (Node "NN" ts)
                   | isSaveNode x        =  putTrace ("saves node" ++show x)
                                         >> saveState (getCat x) ts
                   | isSkipNode x        = enableSkip ts
                  -- | isPunct x           = do inner <- gets isInnerS
                  --                            if null inner then parseX (Node "X" ts)
                  --                                          else enableSkip ts

-- otherwise, just continue
parseX (Node x [t]) = putTrace ("one left in "++x ) 
                    >> parseX t 
parseX (Node x (t:ts)) = do
  putTrace ("many left in "++x)   
  parseX t               --look at x here --why do we do this??
  parseX (Node "XX" ts)  --but not again

recoverFrom :: [Type] -> Result -> Parser Result 
recoverFrom typs (Ok state) = 
  case toGFStr typs of
       Just tok -> do
             putTrace ("recover word "++tok)
             let  nextTok  =  simpleParseInput tok
                  newState = nextState state nextTok
             case newState of
                  Right e  -> putTrace ("recover success") >> (return $ Recover e)
                  Left  er -> putTrace ("recover fail")    >> return Failed
       Nothing  -> do -- if not in list, we use normal recover
             let nextTok :: ParseInput 
                 nextTok = simpleParseInput "XXX"
                 lastSt = nextState state nextTok
             case lastSt of
                  Right _  -> return Failed 
                  Left  er ->  putTrace ("super recover") 
                           >>  return (Recover $ fst $ recoveryStates typs er)
recoverFrom typs state = return state



parsePieces :: [Tree String] -> Parser [[Expr]] --[[(Type,Result)]]
parsePieces ts = do modify $ \s -> s {gettingPieces = True}
                    sequence [tryParse t | t <- ts] 
  where tryParse :: Tree String -> Parser [Expr]
        tryParse (Node t []) = return [mkApp meta []]
        tryParse (Node t ts) = do
                let cats = if isSaveNode t then (getCat t++[utt]) else [utt]
                ps <- concat <$> sequence [do emptyState c
                                              parseX (Node t ts) 
                                              res <-  getBest <$> gets currentStates
                                              pieces <- gets pieces
                                              expr <-  acceptOrContinue c res pieces
                                              return expr {-(c,res)-} | c <- cats]
                putTrace $ "making pieces, in node "++t++"found "++show (length ps)
                return ps

         where 
          acceptOrContinue :: Type -> Result -> [[Expr]] -> Parser [Expr]
          acceptOrContinue c (Ok st) olds = do
             putTrace $ "accept "++show c++"?"
             pgf <- gets pgf
             case fst (getParseOutput st c Nothing) of 
                  ParseOk trees   -> do putTrace $ "yes, accept "++show c
                                        return $ map fst $ take chunklimit $ rankTreesByProbs pgf (nub trees)
                  _               -> do putTrace $ "no, not good parseState for "++show c
                                        return [mkApp meta []] --locallyPiece ts olds

          acceptOrContinue c _       olds = do 
              putTrace $ "no, no good result for "++show c
              --locallyPiece ts olds --{-
              return [mkApp meta []] --concat <$> parsePieces ts 
          locallyPiece ts olds = do 
              emptyPieces
              res <- concat <$> parsePieces ts
              savePieces olds
              return res




isSaveNode = (`elem` map fst saveNodes)

isDisambigNode = (`elem` map fst disambigNodes)


saveNodes = [("NP",nps),("PP",[adv]),("SS",[npsub,np]),("OO",[npobj,np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",nps)
            ,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            ,("CPP",[adv]),("CS",[sent]),("CVP",[vpx]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp]),("VP",[vpx])--,("S",[utt])
            ,("NN",nps)] --TODO change v to all vs

disambigNodes = [("NP",np),("PP",adv),("SS",np),("OO",np)
               ,("OA",adv),("AP",ap),("CPP",adv),("CS",sent)
               ]
{-
typeNodes = [("NP",np),("PP",adv),("SS",np),("OO",np)
            ,("OA",adv),("TA",adV),("XA",adV),("VA",adV)
            ,("MA",adV),("KA",adV),("CA",adV),("AA",adV)
            ,("+A",adV),("FV",v), ("IV",v),("CNP",np)
            ,("AP",ap),("AVP",adV),("CAP",ap),("CAVP",adV)
            ,("CPP",adv),("CS",sent),("CVP",vpx),("NAC",utt)
            ,("++",conj),("SP",comp),("VP",vpx)
            ,("NN",np)] --TODO change v to all vs

-}

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]
isPunct    x = any (`isPrefixOf` x) ["IP","IU","I?"]

getCat :: String -> [Type]
getCat = fromJust . getCatMaybe
getCatMaybe :: String -> Maybe [Type]
getCatMaybe = (`lookup` saveNodes)
getTypeNode :: String -> Type
getTypeNode = fromJust . (`lookup` disambigNodes)

saveState ::  [Type] -> [Tree String] -> Parser Success
saveState recover ts = do
  res  <- gets currentStates
  recSt <- getRecState
  recTyp <- getRecTypes
  let oks =  filter isOkResult res  --only the ones that are ok when we start should be considered
  case oks of
       [] -> return False
       _  -> do
            local (putCurrentStates oks >> pushRecTypes recover >> parseX (Node "XX" ts))
            news <- gets currentStates
            recovered <- cleanUp recSt recTyp news
            putCurrentStates recovered
            return (not $ null recovered) --if we have any left..
cleanUp s t res = concat <$> mapM (recoverSt s t) res 
  where recoverSt :: [Result] -> [Type] -> Result -> Parser [Result]
        recoverSt _     _      (Recover st) = return [Ok st]
        recoverSt recSt recTyp Failed           = mapM (recoverFrom recTyp) recSt  -- should be recovered at next level instead
        recoverSt _     _      ok               = return [ok]

isOkResult (Ok _) = True
isOkResult _          = False

enableSkip :: [Tree String] -> Parser Success 
enableSkip ts = do
   modify $ \s -> s {skip = True:skip s}
   res <- parseX (Node "XX" ts)
   modify $ \s -> s {skip = drop 1 (skip s)}
   return res

getBestMaybe t = case getBest t of
   Ok p -> Just $ Ok p
   _    -> Nothing

getBest = head . sortBy (comparing resultOrder)
 where resultOrder (Ok _)      = 1
       resultOrder (Recover _) = 2
       resultOrder _           = 3

metatize :: PGF -> [[(Type,Result)]] -> [[Expr]]
metatize pgf = map putMetas
  where putMetas :: [(Type,Result)] -> [Expr]
        putMetas []               = [mkApp meta []]
        putMetas ((t,res):ps)     = 
           case fst (getParseOutput (getPState res) t Nothing) of 
                ParseOk trees   -> map fst $ take chunklimit $ rankTreesByProbs pgf (nub trees)
                _               -> putMetas ps

getPState (Recover ps) = ps
getPState (Ok      ps) = ps
getPState _                = error "getPState on Failed"

putCurrentStates ::  [Result] -> Parser ()
putCurrentStates ps = modify $ \s -> s {currentStates = ps}

getRecTypes ::  Parser [Type]
getRecTypes = liftM head $ gets recTypes 

getSkip ::  Parser Bool
getSkip  = liftM head $ gets skip 

getRecState :: Parser [Result]
getRecState = liftM head $ gets recState 

pushRecState ::  [Result] -> Parser ()
pushRecState st = do
  putTrace ("push State ")
  modify $ \s -> s {recState = st  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = do
  putTrace ("push types "++ show typ) 
  modify $ \s -> s {recTypes = typ : recTypes s}

restoreStates :: [[Result]] -> [[Type]] ->  Parser ()
restoreStates ps t = modify $ \s -> s {recTypes = t, recState = ps}

savePieces :: [[Expr]] -> Parser ()
savePieces exps = modify $ \s -> s {pieces = exps++ pieces s} 

local :: Parser a -> Parser a
local m = do
  state   <- gets currentStates
  rstates <- gets recState
  rtypes  <- gets recTypes
  pushRecState state
  res <- m
  restoreStates rstates rtypes
  return res

localEmpty :: Type -> [Type] -> Parser a -> Parser (a,PState)
localEmpty typ rec m = do
   old <- get
   emptyState typ
   pushRecTypes rec
   res <- m
   new <- get
   put old
   return (res,new)

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


-- Since we do not have much information about adverbs in our lexicon,
-- we remember the possibility that an adverb (Adv) was acctually used
-- as AdV. The AdV will then use the 'meta' word ?adV
backUp{-ForAdv-} :: ParseState -> Parser [Result]
backUp{-ForAdv-} state = do 
  types  <- getRecTypes
  if adV `elem` types then parseAsAdV
          else if v `elem` types 
               then return [] -- TODO parseAsVs
               else return [] 
 where parseAsAdV = do
         putTrace "backing up for adv" 
         let nextTok = simpleParseInput $ fromJust $ toGFStr advs -- [adV,adv]  --good?
         case nextState state nextTok of
              Right st -> return [Ok st]
              _        -> return []
              --TODO
       parseAsVs = do
         putTrace "backing up for verb" 
         let nextTok = simpleParseInput $ fromJust $ toGFStr verbs  --------TODO OBS add verbs in Types and Grammar
         case nextState state nextTok of                        --- also allow all types for chunking vps
              Right st -> return [Ok st]
              _        -> return []


meta = mkCId "?" 


disambigParse :: Tree String -> Parser Success
disambigParse n@(Node x ts)  
    | isDisambigNode x = do
         putTrace "is in a disambiguation situation :O"
         let typs  = getCat x
         let typ   = getTypeNode x
         (ok,st) <- localEmpty typ typs $ parseX n
         let tr  = currentStates st
         if length tr <= disambiglimit || null ts --600 ca
              then do s <- goDeeper ts
                      bit <- gets currentStates
                      rankAndSave bit typ typs
                      return s
              else goFurther ts
    | null ts   = parseX n
    | otherwise = goDeeper ts


goDeeper [] = error "tried to go deeper on empty"
goDeeper (t:ts) = do
  disambigParse t
  disambigParse (Node "X" ts)

goFurther [] = error "tried to go further on empty"
goFurther (t:ts) = do
  parseX t
  parseX (Node "X" ts)

rankAndSave :: [Result] -> Type -> [Type] -> Parser ()
rankAndSave pstates typ typs = do
  let trees =  concat $ catMaybes $ map (extractTrees typ) pstates
  pgf <- gets pgf
  pst <- gets currentStates
  let best = take chunklimit $ rankTreesByProbs pgf trees 
  putReplaced best
  --typs <- mapM getCats best
  --let typ = fromMaybe typs (getCatMaybe x)
  mapM (recoverFrom typs) pst
  putTrace $ "rankAndSaved for "++show typs
 --where getCats (expr,_) = do
 --        pgf <- gets pgf
 --        let eTs = inferExpr pgf expr
 --        case eTs of
 --             Left e      -> error $ "could not infer type for expr " -- ++show expr++show e
 --             Right (_,t) -> return t
  
extractTrees :: Type -> Result -> Maybe [Expr]
extractTrees typ pstate = do
  best <- getBestMaybe [pstate] --fult
  let trees      = getTrees textoutput
      textoutput = fst (getParseOutput (getPState best) typ Nothing) 
      getTrees (ParseOk t) = t
  Just trees



putReplaced :: [(Expr,Double)] -> Parser ()
putReplaced expr = do
  i <- gets counter
  modify $ \s -> s {counter = i+1, replaced = (i,map fst expr):replaced s}


{-
    | isDisambigNode && isAmbigouos = do
           old <- gets parseState
           -- if allTrees far too many, do this below, otherwise use them directly
           ok <- local (disambigParses t)
           ---check if ok, put pack old state?
           bit <- gets currentState
           putCurrentState old
           if isBig bit then rankAndSave bit
                        else parseX (Node "X" ts)

    | otherwise                   = return True -- ?
-}
 
{-
&& hardParts  =
                            do addToState (typ++i) (rankByProbs res)
                               parseNext typ
                               disambigParses ts
                       | isAmbigouos && goodParts = 
                            do let bits = disambigsParses parts
                               addToState (typ++i) (rankByProbs bits)
                               parseNext typ
                               disambigParses ts
                      | otherwise     = 
                            do parseNext t
                               disambigParses ts
                               
  where addToState = undefined -- add to be exchanged when done
        isAmbigouos = undefined -- Bool, if more trees than limit (length result > limit)
        result      = parseWithEmptyStateAs typ t
        parseNext   = parseNormal t -- without emptying state
        -}

instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"++unlines (map (showExpr []) tree)
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"


