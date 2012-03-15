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

data PState = PS {recTypes      :: [[Type]]       --stack of types which unknown words should be parsed as
                 ,recState      :: [[Result]]     --stack of states where parsing should be recovered from
                 ,skip          :: [Bool]         --stack of booleans telling whether a word may be skipped
                 ,currentStates :: [Result]       --the current parse state
                 ,pieces        :: [[Expr]]       --relevant pieces parsed so far
                 ,emptyPState   :: Type -> ParseState --an empty parse state
                 ,replaced      :: [(Int,[Expr])]  --list of expressions to replace the ints with later
                 ,counter       :: Int             --counter for replacing
                 ,sentences     :: [Tree String]
                 ,pgf           :: PGF             --the pgf
                 ,isInnerS      :: Int             --stack keeping track of if a sentence is embedded or not
                 ,gettingPieces :: Bool            --are we parsing chunks or the whole sentence?
--                 ,trace         :: [String]        --logging trace
                 --,checkpoints   :: [([Result],Int,String)]
                 }
type Parser = WriterT [String] (State PState)
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
backuplimit = 20
saveStatelimit = 8 

parseText :: Tree String -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  let ((success,trejs),st) = runState (runWriterT (parseX tree)) parser 
  appendFile "outputdis4" (unlines $ trejs++["\n"])
  limitOutput success st 

 where parser     = initPState pgf lang startType
      
       limitOutput :: Success -> PState -> IO [Expr]
       limitOutput success st | success && canProduce = do putStrLn $ "success "++show success
                                                           sents <- parseSentences st lang
                                                           return $ {-# SCC "huvudrank" #-} (take limit $ rankThem trees)
                                                                    ++{-# SCC "birank" #-}  (rankThem $ concat sents)
                                                           
                              | otherwise = do putStrLn $ "could not produce. success :"++show success
                                               {-# SCC "bitrank" #-} take limit <$> putPiecesTogether st
         where canProduce = isOkParse result
               result     = if isOkParse textoutput then textoutput
                                      else fst ({-# SCC "getParseOutput1" #-}getParseOutput (getPState best) utt Nothing) 
               textoutput = {-# SCC "textoutput" #-} fst ({-# SCC "getParseOutput" #-} getParseOutput ({-# SCC "getPState" #-} getPState best) startType Nothing) 
               best       = getBest $ currentStates st
               trees      = getTrees result
               getTrees  (ParseOk t) = t
               isOkParse (ParseOk t) = True
               isOkParse x           = DB.trace ("got a bad result "++show x) False
        
               rankThem trees = map fst $ rankTreesByProbs pgf trees

               putPiecesTogether st = let piec = pieces st in
                     return $ [mkApp (mkCId "Text") [mkApp meta c] 
                              | c <- combinations $ map (take chunklimit) piec] 
               combinations = sequence

initPState pgf lang startType =  
  let startS = startState pgf lang startType
  in  PS {recTypes = [[sent]], recState =  [[Ok $ startS ]], skip = [False]
         ,currentStates = [Ok startS], pieces =  []
         ,emptyPState = initState pgf lang
         ,replaced = [], counter = 0, isInnerS = 0, gettingPieces = False
         ,sentences = [], pgf = pgf}

startState pgf lang startType = initState pgf lang startType

-- parse the words
parseX' ::  Tree String -> Parser Success
parseX' (Node w []) | length (words w) == 0 = return True --for removed parts of names 
                    | length (words w) > 1  = liftM last  -- compounds
                                            $ mapM (parseX . flip Node []) 
                                            $ words w
                    | otherwise             = {-# SCC "parseXword" #-}
  do states    <- gets currentStates 
     newStates <- mapM parseNext states
     putCurrentStates $ concat newStates
     return True 

 where parseNext :: Result -> Parser [Result]
       parseNext (Ok state) = {-# SCC "parseNext" #-} do
              putTrace ("parsing "++w) 
              skipOk <- getSkip
              putTrace ("skip ok: "++show skipOk) 
              let nextTok = simpleParseInput w 
              case {-# SCC "nextState" #-}nextState state nextTok of
                   Right ps  -> do putTrace ("parse success "++w)
                                   --st <- backUp state
                                   let st' = if skipOk then [Ok state]
                                                       else []
                                   return ([Ok ps]++st')-- ++st)   
                   Left  er  -> do if skipOk then return [Ok state]
                                             else do
                                               types  <- getRecTypes
                                               rstate <- getRecState
                                               ps' <- mapM (recoverFrom types) rstate 
                                               putTrace ("parse fail "++w++", recover as "++show types) 
                                               return ps' 
       parseNext badState = return [badState]


parseX' (Node "ROOT" [t]) = parseX t -- only one node in root
parseX' (Node "ROOT" ts)  = {-# SCC "parseRoot" #-}do       -- if there are more, we don't know what to do
  res <- parsePieces ts
  savePieces $ res 
  return False  


parseX' node@(Node "S"  ts) = {-# SCC "parseS" #-}do 
   old       <- gets currentStates
   inner     <- gets isInnerS
   case inner of
        0 -> putTrace "parsing a s" >> parseS old inner
        n -> do modify $ \s -> s {sentences = node : sentences s}
                putTrace "save the sentence for later"
                res <- recoverSentence old inner
                return True -- $ any isOkResult res

 where parseS old inner =  {-# SCC "parseSreal" #-}do 
         piecing   <- gets gettingPieces
         oldPieces <- emptyPieces 
         modify $ \s -> s {isInnerS = inner + 1}
         parseX (Node "XX" ts)
         new  <- gets currentStates
         let failFunction = combinePieces old oldPieces piecing inner
         let out = {-# SCC "out" #-} findBest (filter isOkResult new) 
         res <- case out of 
              Just x    -> return True
              Nothing   ->  putTrace ("failed on S ") >> failFunction
         modify $ \s -> s {isInnerS = inner}
         putTrace $ "has parsed a sentence, result was "++show res
         return res

       findBest :: [Result] -> Maybe ParseOutput
       findBest (map (\(Ok x) -> fst $ {-# SCC "getParseOutput4" #-}getParseOutput x phrText Nothing) -> xs)
                   = {-# SCC "findBest" #-}findParseOk xs `mplus` findParseInc xs
         where
           findParseOk (x@ParseOk{}:_) = Just x
           findParseOk (_:xs)          = findParseOk xs
           findParseOk []              = Nothing
       
           findParseInc (ParseIncomplete:_) = Just ParseIncomplete
           findParseInc (_:xs)              = findParseInc xs
           findParseInc []                  = Nothing

-- TODO Insert pieces afterwards, keep node numbers to put them back

       combinePieces oldst oldPieces piecing inner = {-# SCC "combinePieces" #-}do
         emptyPieces 
         putTrace "emptying pieces, parsing news"
         res       <- parsePieces ts
         let s = map (mkApp (mkCId "S")) $ sequence res 
         putTrace $ "got pieces "++show (map (showExpr []) s)
         savePieces $ s : oldPieces
         unless piecing $ (recoverSentence oldst inner >> return ())
         return False 

       recoverSentence :: [Result] -> Int -> Parser [Result]
       recoverSentence st inner = {-# SCC "recoverSentence" #-}do 
         rec <- case inner of
                     0 -> putTrace "recover outer" >> mapM (recoverFully [sent]) st
                     _ -> putTrace "recover inner" >> mapM recoverConjAndSent st
         putCurrentStates rec
         return rec

       recoverConjAndSent st= do mid <- recoverFully [conj] st
                                 recoverFully [sent] mid 

       recoverFully t st = do mid <- recoverFrom t st
                              case mid of
                                   Recover st' -> return $ Ok st'
                                   _           -> return $ mid


-- some nouns don't have surrounding NP, but should be roubust anyway
parseX' (Node x ts) | "NN" `isPrefixOf` x && length x>2 = parseX (Node "NN" ts) -- put all variants of NNxxx to NN

                    | isSaveNode x        =  {-# SCC "parseXSave" #-}putTrace ("saves node" ++show x)
                                          >> saveState (getCat x) ts
                    | isSkipNode x        = enableSkip ts

-- otherwise, just continue
parseX' (Node x [t]) = putTrace ("one left in "++x ) 
                    >> parseX t 
parseX' (Node x (t:ts)) = do
  putTrace ("many left in "++x)   
  parseX t               
  parseX (Node "XX" ts)  -- continue, but don't use x anymore


recoverFrom :: [Type] -> Result -> Parser Result 
recoverFrom typs (Ok state) = 
  case toGFStr typs of
       Just tok -> do
             putTrace ("recover word "++tok)
             let  nextTok  =  simpleParseInput tok
                  newState = {-# SCC "nextState5" #-}force $ nextState state nextTok
             case newState of
                  Right e  -> putTrace ("recover success") >> (return $ Recover e)
                  Left  er -> putTrace ("recover fail")    >> return Failed
       Nothing  -> do -- if not in list, we use normal recover
             putTrace "couldn't do a normal recovery"
             let nextTok :: ParseInput 
                 nextTok = simpleParseInput "XXX"
                 lastSt = {-# SCC "nextState4" #-}nextState state nextTok
             case lastSt of
                  Right _  -> return Failed 
                  Left  er ->  putTrace ("super recover") 
                           >>  return (Recover $ fst $ recoveryStates typs er)

recoverFrom typs state = return state


parsePieces :: [Tree String] -> Parser [[Expr]] 
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
                                              return expr
                                          | c <- cats]
                putTrace $ "making pieces, in node "++t++"found "++show (length ps)
                return ps

         where 
          acceptOrContinue :: Type -> Result -> [[Expr]] -> Parser [Expr]
          acceptOrContinue c (Ok st) olds = do
             putTrace $ "accept "++show c++"?"
             pgf <- gets pgf
             case fst ({-# SCC "getParseOutput3" #-}getParseOutput st c Nothing) of 
                  ParseOk trees   -> do putTrace $ "yes, accept "++show c
                                        return $ map fst $ take chunklimit $ rankTreesByProbs pgf (nub trees)
                  _               -> do putTrace $ "no, not good parseState for "++show c
                                        return [mkApp meta []] --locallyPiece ts olds

          acceptOrContinue c _       olds = do 
              putTrace $ "no, no good result for "++show c
              --locallyPiece ts olds 
              return [mkApp meta []] 
          locallyPiece ts olds = do 
              emptyPieces
              res <- concat <$> parsePieces ts
              savePieces olds
              return res

parseSentences :: PState -> Language -> IO [[Expr]]
parseSentences st lang = 
  let sents = sentences st
      pgf'  = pgf st
  in mapM (\s -> do parseText (Node "MS" [s]) pgf' lang phrText) sents


isSaveNode = (`elem` map fst saveNodes)

isDisambigNode = (`elem` map fst disambigNodes)


saveNodes = --("NP",nps),
            [("PP",[adv]), ("SS",[npsub,np]),("OO",[npobj,np])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("+A",advs),("FV",[v]), ("IV",[v]),("CNP",nps)
            --,("AP",[ap]),("AVP",advs),("CAP",[ap]),("CAVP",advs)
            --,("CPP",[adv]),("CS",[sent]),("CVP",[vpx]),("NAC",[utt])
            ,("++",[conj]),("SP",[icomp,comp]) --,("VP",[vpx])
            ,("NN",[np])] --TODO change v to all vs

disambigNodes = [("NP",np),("PP",adv),("SS",np),("OO",np)
               ,("OA",adv),("AP",ap),("CPP",adv),("CS",sent)
               ]


isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]

getCat :: String -> [Type]
getCat = fromJust . getCatMaybe
getCatMaybe :: String -> Maybe [Type]
getCatMaybe = (`lookup` saveNodes)

saveState ::  [Type] -> [Tree String] -> Parser Success
saveState recover ts = do
  res    <- gets currentStates
  recSt  <- getRecState
  recTyp <- getRecTypes
  let oks =  filter isOkResult res  --only the ones that are ok when we start should be considered
  case oks of
       [] -> return False
       _  -> do
            local (   pushRecState oks >> putCurrentStates oks 
                   >> pushRecTypes recover >> parseX (Node "XX" ts))
            backups <- concat <$> mapM (\st -> backUp (unOk st) recover) (take (backuplimit-length oks)  oks)
            news    <- gets currentStates
            recovered <- cleanUp recSt recTyp (news++backups)
            putCurrentStates $ recovered -- ++ filter (not . isOkResult) res
            putTrace "have survived a save node"
            return (not $ null recovered) --if we have any left..
cleanUp s t res = concat <$> mapM (recoverSt s t) res 
  where recoverSt :: [Result] -> [Type] -> Result -> Parser [Result]
        recoverSt _     _      (Recover st) = return [Ok st]
        recoverSt recSt recTyp Failed           = -- should be recovered at next level instead
                                                  mapM (recoverFrom recTyp) recSt 
        recoverSt _     _      ok               = return [ok]

isOkResult (Ok _) = True
isOkResult _      = False
unOk (Ok pst) = pst

enableSkip :: [Tree String] -> Parser Success 
enableSkip ts = do
   modify $ \s -> s {skip = True:skip s}
   res <- parseX (Node "XX" ts)
   modify $ \s -> s {skip = drop 1 (skip s)}
   return res

getBestMaybe t = case getBest t of
   Ok p -> Just $ Ok p
   _    -> Nothing

getBest :: [Result] -> Result
getBest ts = let res  = map (resultOrder &&& id) ts
                 list :: [(Int,Result)]
                 list = filter ((==1) . fst) res ++ filter ((==2) . fst) res
                         ++ res
             in snd $ head list
 where resultOrder (Ok _)      = 1
       resultOrder (Recover _) = 2
       resultOrder _           = 3

getPState (Recover ps) = ps
getPState (Ok      ps) = ps
getPState _            = error "getPState on Failed"

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
  modify $ \s -> s {recState = (take saveStatelimit st)  : recState s}

pushRecTypes ::  [Type] -> Parser ()
pushRecTypes typ = do
  putTrace ("push types "++ show typ) 
  modify $ \s -> s {recTypes = typ : recTypes s}

putStates :: [[Result]] -> [[Type]] ->  Parser ()
putStates ps t = modify $ \s -> s {recTypes = t, recState = ps}

savePieces :: [[Expr]] -> Parser ()
savePieces exps = modify $ \s -> s {pieces = exps++ pieces s} 

local :: Parser a -> Parser a
local m = do
 -- state   <- gets currentStates
  rstates <- gets recState
  rtypes  <- gets recTypes
--  pushRecState state
  res <- m
  putStates rstates rtypes
  return res

putTrace ::  String -> Parser ()
putTrace str = do
  x <- gets recState
  c <- gets currentStates
  let n = sum $ map length x
      m = length c
  tell [show(n,m)++","++str] -- $ \s -> s {trace = str:trace s}

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
backUp :: ParseState -> [Type] -> Parser [Result]
backUp state types = do 
  --types  <- getRecTypes
  if adV `elem` types then parseAsAdV
          else if v `elem` types 
               then return [] -- TODO parseAsVs
               else return [] 
 where parseAsAdV = do
         putTrace "backing up for adv" 
         let nextTok = simpleParseInput $ fromJust $ toGFStr advs 
         case {-# SCC "nextsTate3" #-}nextState state nextTok of
              Right st -> return [Ok st]
              _        -> return []
              --TODO
       parseAsVs = do
         putTrace "backing up for verb" 
         let nextTok = simpleParseInput $ fromJust $ toGFStr verbs  --------TODO OBS add verbs in Types and Grammar
         case {-# SCC "nextState1" #-}nextState state nextTok of                        --- also allow all types for chunking vps
              Right st -> return [Ok st]
              _        -> return []


meta = mkCId "?" 

{-
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
      textoutput = fst ({-# SCC "getParseOutput2" #-}getParseOutput (getPState best) typ Nothing) 
      getTrees (ParseOk t) = t
  Just trees



putReplaced :: [(Expr,Double)] -> Parser ()
putReplaced expr = do
  i <- gets counter
  modify $ \s -> s {counter = i+1, replaced = (i,map fst expr):replaced s}

-}

parseX tree = do
  putTrace $ "parseX with tree "++show tree
  parseX' tree


force :: Either a ParseState -> Either a ParseState
force (Right po) = po `seq` Right po
force (Left xx)  = xx `seq` Left xx

instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"++unlines (map (showExpr []) tree)
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"


{-
mkInputYess tree = ParseInput {-(listToMaybe . M.elems)-} (const Nothing) (\_ -> Just (wildCId,tree,["militär"]))
mkTreeInput :: PGF -> Language -> (Expr,String) -> ParseInput
mkTreeInput pgf lang = 
     mkParseInput pgf lang (\a b -> Nothing) [(mkCId "Expr",\(tr,to) -> Just (tr,[to]))]

testIt = do
  pgf <- readPGF "pgfs/BigParse0.pgf"
  let lang = read "BigParseSwe"
      p = startState pgf lang aa
      --inp = mkTreeInput pgf lang (expr,"militär")
      inp = mkInputYess expr
      st = nextState p inp
  case st of
       Left e   -> putStrLn "gick ej"
       Right st -> let out = getParseOutput st aa Nothing
                   in  putStrLn $ "result "++show out

expr = fromJust $ readExpr "militaer_av_1_1_A"
aoa    = fromJust $ readType "A"   
-}
