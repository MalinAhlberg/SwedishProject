import PGF
import Control.Monad.State hiding (ap)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.List
import Data.Maybe
import Debug.Trace
import Idents

-- speed up! better when compiled? remove unnecessary categories.

type Results s = StateT RState IO s
data RState = RS {parsed     :: [Chunk]
                 ,firstState :: ParseState
                 ,actives    :: TVar Int
                 ,channel    :: TChan [[Chunk]]}
data Chunk = C {start,end :: Int, trees :: [Tree], typ :: Type}
  deriving Show

test str = do
  pgf <- readPGF "../../gf/BigTest.pgf"
  let typ = fromJust $ (readType "NPTyped Subject")
  return $ parse pgf (read "BigTestSwe") typ  str
 
testState = do
  pgf    <- readPGF pgfFile
  let pstate = initS pgf 
  print $ fst $ getParseOutput pstate text Nothing 
 where initS pgf = initState pgf lang startType

testchunk str = do
   (chan,act) <- doit $ words str
   readUntilEmpty chan act


readUntilEmpty :: TChan [[Chunk]] -> TVar Int -> IO ()
readUntilEmpty chan act = do
   e <- atomically $ isEmptyTChan chan
   unless e printRes 
   e' <- atomically $ isEmptyTChan chan
   i  <- atomically $ readTVar act
   unless (e' && i==0) $ readUntilEmpty chan act
 where printRes = do
          tree <- atomically $ readTChan chan
          putStrLn $ map showChunks tree

showChunks :: [Chunk] -> String 
showChunks ch = unlines $ map pretty [(show $ typ c, map (showExpr []) $ trees c) | c <- ch]
  where pretty (t,trs) = t++"\n"++unlines trs++"\n"

 -- This approach: start from beginning, let all types parse as far as they can,
 -- where they stop they let all other continue.

 {- Will get a problem cause we need to decide wether NP, AP, Adv, Pron, Quant
    and Det are prons or subj. Pron ok, cause only Obj when 'sig'. The other need
    types when not super-trivial. Try both and return wanted -}

instance Show ParseOutput where
   show  (ParseOk tree)  = "ParseOK"
   show  (TypeError x)   = "TypeError"
   show  (ParseFailed i) = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"

doit str = do
  pgf    <- readPGF pgfFile
  chan   <- newTChanIO
  tvar <- newTVarIO  (length allTypes)
  let pstate = initS pgf 
      rstate = initRState pstate tvar chan 
  mapM_ forkIO [evalStateT (doParse (0,0) pstate typ str) rstate
               | typ <- allTypes]
  return (chan,tvar)
 where initS pgf = initState pgf lang startType
-- alt : 
  --doParse (0,0) initS (allTypes) str  ----tråda och börja på olika positioner!

doParse :: (Int,Int) -> ParseState -> Type -> [String] -> Results ()
doParse (i,j) state typ (s:str) = let nextTok = simpleParseInput s in
  case nextState state nextTok of
       Right pst -> doParse (i,j+1) pst typ str  -- assumes that if this state is bad, we will get error
                                                 -- next time
       Left er  -> 
          case fst (getParseOutput state typ Nothing) of
               ParseOk trees -> if (i==j) then failParse typ else do
                  tell $ C i j trees typ
                  newPState <- gets firstState
                  rst       <- get
                  updateActives (length allTypes)
                  liftIO $ mapM_ forkIO 
                         [ evalStateT (doParse (j,j+1) newPState t (s:str)) rst
                         | t <- allTypes]

               er            -> failParse typ

doParse (i,j) state typ [] = do 
  case fst (getParseOutput state typ Nothing) of
       ParseOk trees -> do
          tell $ C i j trees typ
          saveSolution typ
          updateActives (-1)
       er            -> failParse typ

combine :: [Chunk] -> [Tree]
combine chs = undefined

-- States
initRState :: ParseState -> TVar Int -> TChan [[Chunk]] -> RState
initRState pgf tvar = RS [] pgf tvar

tell :: Chunk -> Results ()
tell (C i j e t) = modify $ \s -> s {parsed = (C i j e t):parsed s}

updateActives :: Int -> Results ()
updateActives i = do 
  tvar <- gets actives
  liftIO $ atomically (writeTVar tvar . (+i) =<< readTVar tvar) 

failParse :: Type -> Results ()
failParse typ = do
--  liftIO $ putStrLn $ "failed for type "++show typ
  updateActives (-1)
  --could write some error msg to somewhere


saveSolution :: Type -> Results ()
saveSolution typ = do
  --liftIO $ putStrLn $ "could parse using type "++show typ
  res <- gets parsed
  ch  <- gets channel
  liftIO $ atomically $ writeTChan ch [reverse res]

pgfFile = "../../gf/BigTest.pgf"
lang = read "BigTestSwe"
startType = text 
nextTypes :: Type -> [Type]
nextTypes t | t == text = [phr]
            | t == phr  = [utt]
            | t == utt  = [s]
            | t == s    = [cl]
            | t == cl   = class5
            | t `elem` class5 = class6
            | t `elem` class6 = [idet,card,art,rp,ada,subj,conj,voc] --,pol]
class5 = [vp,npsub,advsub]
class6 = [npobj,advobj,v,cn,detobj,detsub
         ,predet,apsub,apobj,rcl]

allTypes = [text,phr,utt,s,cl]++class5++class6


