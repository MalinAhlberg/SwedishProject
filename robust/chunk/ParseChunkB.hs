import PGF
--import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Applicative
import Control.Monad.State hiding (ap)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import CPUTime
import Data.List
import Data.Maybe
import Debug.Trace
import System.TimeIt
import Idents

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

main = do
  t <- getCPUTime
  timeIt $ testchunk "katt sova en hund sover"
  t' <- getCPUTime
  putStrLn $ "Real time "++show (t'- t)

testchunk str = do
   (chan,act) <- doit $ words str
   res <- consume chan act
   putStr $ unlines $ map showChunks $ concat res

-- Good code. Watch and learn
consume :: TChan [[Chunk]] -> TVar Int -> IO [[[Chunk]]]
consume resChan doneTVar = do --unsafeInterleaveIO $ do
     putStrLn "consuming..."
     element <- atomically $ do empty  <- isEmptyTChan resChan
                                active <- readTVar doneTVar
                                if empty then (if active==0 then return Nothing else retry)
                                         else Just <$> readTChan resChan
     case element of
             Nothing -> return []
             Just e  -> (e:) <$> consume resChan doneTVar

{-
BAD CODE. Watch and learn
readUntilEmpty :: TChan [[Chunk]] -> TVar Int -> IO ()
readUntilEmpty chan act = do
   e <- atomically $ isEmptyTChan chan
   unless e printRes 
   e' <- atomically $ isEmptyTChan chan
   i  <- atomically $ readTVar act
   putStrLn $ "Threads left :"++show i
 --  liftIO $ writeFile "trams" $ "Threads left :"++show i
   unless (e' && i==0) $ readUntilEmpty chan act
 where printRes = do
          tree <- atomically $ readTChan chan
          putStrLn $ unlines $ map showChunks tree
          -}

showChunks :: [Chunk] -> String 
showChunks ch = unlines $ map pretty [(show $ typ c, map (showExpr []) $ trees c) | c <- ch]
  where pretty (t,trs) = t++"\n"++unlines trs++"\n"

 -- This approach: start from beginning, let all types parse as far as they can,
 -- where they stop they let all other continue.

 {- Will get a problem cause we need to decide wether NP, AP, Adv, Pron, Quant
    and Det are prons or subj. Pron ok, cause only Obj when 'sig'. The other need
    types when not super-trivial. Try both and return wanted -}

instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"

-- we can assume that there is no grammar rule for combining these
-- we need to rate them and select the best output
-- do stupid choosing when ambigouos, so far
combine :: [[Chunk]] -> Tree
combine chs = let fstch = map fst chs


doit :: [String] -> IO (TChan [[Chunk]], TVar Int)
doit str = do
  (t,pgf)<- timeItT $ readPGF pgfFile
  (t',chan)   <- timeItT $ newTChanIO
  (t'',tvar)   <- timeItT $ newTVarIO  (length allTypes)
  let pstate = initS pgf 
      rstate = initRState pstate tvar chan 
  putStrLn $ "created pgf etc "++show t++"+"++show t'++"+"++show t''
  tt <- timeIt $ mapM_ forkIO [evalStateT (doParse (0,0) pstate typ str) rstate 
                              | typ <- allTypes]
  putStrLn $ "forking "++show t++"+"++show t'++"+"++show t''
  return (chan,tvar)
 where initS pgf = initState pgf lang startType

doParse :: (Int,Int) -> ParseState -> Type -> [String] -> Results ()
doParse (i,j) state typ (s:str) = let nextTok = simpleParseInput s in
  --(tx,nextTok) <- liftIO $ timeItT $! return $! simpleParseInput s 
  --(t,next) <- liftIO $ timeItT $! return $!  
  --liftIO $ putStrLn $ "nextTok, nextstate "++show tx++"+"++show t
  case nextState state nextTok of
       Right pst -> doParse (i,j+1) pst typ str  -- assumes that if this state is bad, we will get error
                                                 -- next time
       Left er  -> 
          case fst (getParseOutput state typ Nothing) of
               ParseOk trees -> if (i==j) then failParse typ else do
                  tell $ C i j trees typ
                  newPState <- gets firstState
                  rst       <- get
                  updateActives (length allTypes-1)
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
  --liftIO $ putStrLn $ "failed for type "++show typ
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

allTypes = [s,npsub,v,npobj,advsub,apobj,apsub] --[text,phr,utt,s,cl]++class5++class6


