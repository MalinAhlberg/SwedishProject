{-# LANGUAGE ViewPatterns #-}
module NewestChunk where 
import PGF hiding (Tree)
import Control.Applicative
import Control.Arrow hiding ((+++),(|||))
import Control.Monad.Writer.Lazy hiding (ap)
import Control.Monad.State.Lazy hiding (ap)
import Data.List
import Data.List.Split
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
                 ,morpho        :: Morpho
                 ,saveChunks    :: [(Id,[Expr])]  
                 ,lastExpr      :: Maybe (Id,[Expr])
                 ,npRate        :: (Int,Int,[Int]) --NPInfo
                 ,npCover       :: (Int,Int,[Int])
                 ,apRate        :: (Int,Int,[Int]) --NPInfo
                 ,apCover       :: (Int,Int,[Int])
                 ,advRate       :: (Int,Int,[Int]) --NPInfo
                 ,advCover      :: (Int,Int,[Int])
                 }
type Parser = WriterT [String] (State PState)
type Id     = String
data Info   = I {ok, tot :: Int, okLength :: [Int]}
    deriving Show
data ChunkInfo =  CI {npR :: Info, apR :: Info, advR :: Info
                     ,npC :: Info, apC :: Info, advC :: Info}
    deriving Show

chunkInfo :: PState -> ChunkInfo
chunkInfo st = CI (info $ npRate st)  (info $ apRate st)  (info $ advRate st)
                  (info $ npCover st) (info $ apCover st) (info $ advCover st)
    where info (i,j,l) = I i j l
                        


toplimit = 500
limit, chunklimit, disambiglimit :: Int
limit = 500
chunklimit = 20
disambiglimit = 700
backuplimit = 20
saveStatelimit = 8 

countChunks :: Tree (Id,String) -> PGF -> Language -> Type -> IO ChunkInfo
countChunks tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  let st = execState (execWriterT (parseX tree)) parser 
  return $ chunkInfo st
 where parser     = initPState pgf lang startType

parseText :: Tree (Id,String) -> PGF -> Language -> Type -> IO [Expr]
parseText tree pgf lang startType = do
  hSetBuffering stdout LineBuffering
  let (trejs,st) = runState (execWriterT (parseX tree)) parser 
  appendFile "night/advTestUt1Corr" (unlines $ trejs++["\n"])
  appendFile "night/npChunk1" $ show $ npRate st 
  let res = lastExpr st
  case res of 
       Nothing       -> do putStrLn "No parse trees :("
                           combinePieces st
       Just (_,expr) -> do let unparsed = saveChunks st
                               pieces = map (\(i,expr) -> (mkApp (mkCId i) expr)) unparsed
                           let res = limitAndRank expr
                           putStrLn $ "have got a result " ++show (length res)
                           return (res ++pieces)

 where parser     = initPState pgf lang startType
      

       limitAndRank,limitAndRankChunk :: [Expr] -> [Expr]  -- TODO use threshold!!
       limitAndRank = limitAndRank' limit
       limitAndRankChunk = limitAndRank' chunklimit
       limitAndRank' i exs = if length (take toplimit exs)== toplimit 
                                then DB.trace "too many" []
                                else take i $ map fst $ rankTreesByProbs pgf exs

       --parseToks :: Token -> Either a ParseState -> Either a ParseState
       parseToks tok (Right parseState) = nextState parseState (simpleParseInput tok)
       parseToks tok failure            = failure
       noMetas :: [String] -> Int
       noMetas = length . filter (=='?') . concat

       combinePieces st = do
         let chunkorder = mapM (limitAndRankChunk . snd) $ saveChunks st
         return $ take limit $ map (\ex -> mkApp meta ex) chunkorder
       
initPState pgf lang startType =  
  let startS = startState pgf lang startType
  in  PS { currentStates = [""]
         ,gfParser = parse pgf lang
         ,chunks = []
         ,saveChunks = []
         ,rankThem   = map fst . rankTreesByProbs pgf
         ,lastExpr   = Nothing 
         ,morpho     = buildMorpho pgf lang
         ,npRate     = (0,0,[])
         ,npCover    = (0,0,[])
         ,apRate     = (0,0,[])
         ,apCover    = (0,0,[])
         ,advRate    = (0,0,[])
         ,advCover   = (0,0,[])
         }

startState pgf lang startType = initState pgf lang startType

-- parse the words
parseX' ::  Tree (Id,String) -> Parser String
parseX' (Node (i,w) []) = do
-- bad idea? is word unknown? maybe XX if it is. then 
  putTrace $ "add word "++w
  addWord w
  m <- gets morpho
  let lemma = map fst (lookupMorpho m w) ||| [meta]
  --addChunks [(i,map (\x -> mkApp meta [mkApp x []]) lemma)]
  -- TODO lemma this word, and add it as a chunk
  return w                         

{-
parseX' (Node (i,"S") ts)  
   | length splits <2 = parseX' (Node (i,"XX") ts)
   | otherwise = do begin <- emptyString
                    ss    <- emptySaveChunks
                    putTrace "can split this sentence"
                    pieces <- sequence [do emptyString
                                           parseX' (Node ('x':i,"Sx") ts)
                                           ex <- gets lastExpr 
                                           putTrace $ "last expr in splitted is "++show ex
                                           return ex

                                       | ts <- splits
                                       ]
                    ranker <- gets rankThem
                    let parts = map (maybe [mkApp meta []] (ranker . snd)) pieces
                        res   = [(i,[mkApp meta ps]) | ps <- combine parts]
                    addSaveChunks res
                    putTrace $ "added saved chunk "++show res
                    putString begin
                    addSaveChunks ss
                    parseX' (Node (i,"XX") ts)

 where splits = splitWhen (nodeIs isSkipNode) ts
       nodeIs f (Node (i,x) _) = f x

-}

-- some nouns don't have surrounding NP, but should be roubust anyway
parseX' (Node (i,x) ts) 
     | "NN" `isPrefixOf` x && length x>2 = parseX (Node (i,"NN") ts) -- put all variants of NNxxx to NN

--   | isAdverbNode x      = putTrace "fixed an adverb" >> return "?advs"
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

isNP  = not . null . (`intersect` nps) 
isAdv = not . null . (`intersect` advs) 
isAP  = (ap `elem`)

saveNodes = 
            [("SS",[npsub]),("OO",[npobj])
            ,("OA",[adv]),("TA",advs),("XA",advs),("VA",advs)
            ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
            ,("RA",advs),("+A",advs), ("CNP",nps),("AT",[ap])
            ,("ES",[npsub]),("FS",[npsub]),("EO",[npobj]),("FO",[npobj])
            ,("AP",[ap]),("++",[conj]),("SP",[icomp,comp])
            ,("NN",nps),("ROOT",[phrText]),("Sx",[phrText])] 

adverbNodes = [("TA",advs),("XA",advs),("VA",advs)
              ,("MA",advs),("KA",advs),("CA",advs),("AA",advs)
              ,("RA",advs),("+A",advs)
              ]

isSkipNode x = any (`isPrefixOf` x) ["IG","IK","IQ","IR","IS","IT"]
isAdverbNode  = (`elem` map fst adverbNodes)

getCat :: String -> [Type]
getCat a = DB.trace ("fromJust on Cat "++a)$ (fromJust . (`lookup` saveNodes)) a
getStr a id = DB.trace ("fromJust on Str "++show a)$ (fromJust (toGFStr a) +++id)

saveState ::  [Type] -> Id -> [Tree (Id,String)] -> Parser String
saveState recover id ts = do
  begin     <- emptyString
  oldChunks <- emptyChunks
  st        <- get --for chunks
  str       <- unwords <$> mapM parseX ts
  parser    <- gets gfParser
  ranker    <- gets rankThem
  -- TODO if more than chunklimit, rank them and add metas, add to savestate
  let parsable = concatMap (\typ -> parser typ str) recover
  fixed <- gets currentStates
  let parsefixed = findOkSentence fixed parser
  newChunks <- emptyChunks
  putChunks oldChunks
  putTrace $ "chunk parsed "++ str++" as "++show recover++" got "
                            ++show (map (showExpr []) (take 1 parsable))
  putTrace $ "chunk parsed fixed "++ intercalate "," fixed
  let (considerable,res) = if null parsable then (theExpr parsefixed,theStr parsefixed) 
                                            else (parsable,str)
      newString          = if null parsable then concatMap (\fixStr -> map (+++fixStr) begin) fixed
                                            else map (+++str) begin
  -- if we can't parse the chunk, we save its subchunks and adds a dummy word to the sentence

  when (isNP  recover) $ setNP  str fixed parsable parsefixed st
  when (isAP  recover) $ setAP  str fixed parsable parsefixed st
  when (isAdv recover) $ setAdv str fixed parsable parsefixed st
  case length (take limit considerable) of
       0          -> do --if null parsefixed 
                     --  then do
                        let types = getStr recover id
                        putTrace $ "could not parse chunk "++id
                        putString (map (+++types) begin)
                        addSaveChunks newChunks
                        setLastExpr Nothing
                   --    else do putTrace $ "could only parse fixed chunk "++id
                   --            addChunks [(id,parsefixed)] -- :newChunks
                   --            putString (concatMap (\fixStr -> map (+++fixStr) begin) fixed)
                   --            setLastExpr $ Just (id,parsefixed)
       n           | n == limit -> do --putTrace $ "will have to limit chunk "++show n
            --                          let xs = groupWith (`elem` splitters) newString
            --                          if length xs > 1 then parser allTypes xs
            --                                
            --               --split!!
                                     putTrace "Wooo, too many chunks"
                                     --let expr = ranker considerable
                                     let types = getStr recover id
                                     putString (map (+++types) begin)
                                     addSaveChunks [(id,considerable)]
                                     setLastExpr $ Just (id,considerable)
                   | otherwise      -> -- if we can parse it, we add the tree to the state
                                       -- and don't save the dummy words from within the chunk
                                      do putTrace $ "could parse chunk "++id++", "++res
                                         putTrace $"don't have to limit chunk "++show n
                                         addChunks [(id,considerable)]
                                         putString newString
                                         setLastExpr $ Just (id,considerable)
  return str
 where findOkSentence :: [String] -> (Type -> String -> [Expr]) -> [([Expr],String)]
       findOkSentence fixed parser = {-first concat <$> -} take 1 
                                   $ filter (not. null . fst ) 
                                   $ map tryParse fixed
          where tryParse :: String -> ([Expr],String)
                tryParse fixStr = (concatMap (\typ -> parser typ fixStr) recover,fixStr)
       theExpr :: [([Expr],String)] -> [Expr]
       theExpr = concat . map fst . take 1 
       theStr  :: [([Expr],String)] -> String
       theStr  = concat . map snd . take 1 


enableSkip :: [Tree (Id,String)] -> Parser String 
enableSkip ts = do
  begin <- gets currentStates
  str <- concat <$> mapM parseX ts
  putTrace $ "skip processed "
  addStrings begin
  return str
 
setNP  = setCounting setNPRate  setNPCover npRate npCover
setAP  = setCounting setAPRate  setAPCover apRate apCover
setAdv = setCounting setAdvRate setAdvCover advRate advCover

setCounting setrate setcover getrate getcover str strfixed parsable fixed st
   | not (null parsable) = do -- increase both
                     let (n,m,l)    = getrate st
                         (cn,cm,cl) = getcover st
                     setrate  (n+1,m+1,phraseLength str:l)
                     setcover (cn+1,cm+1,[]) --phraseLength str:cl)
   | not (null fixed)    = do --increase coverage, but not rate
                     let (cn,cm,cl) = getcover st
                     (i,j,l') <- gets npRate 
                     setcover (cn+1,cm+1,[]) --phraseLength strfixed:cl)
                     setrate  (i,j+1,l')
                      
   | otherwise           = do --none is increased
                     (i,j,l')    <- gets npRate 
                     (ci,cj,cl') <- gets npCover 
                     setrate  (i,j+1,l')
                     setcover (ci,cj+1,cl')

phraseLength = length . glueBinds . words
 where glueBinds (x:"&+":y:ys) = (x++y):ys
       glueBinds (x:xs)        = x:glueBinds xs
       glueBinds []            = []

setNPRate  np  = modify $ \s -> s {npRate   = np} 
setNPCover np  = modify $ \s -> s {npCover  = np} 
setAPRate  ap  = modify $ \s -> s {apRate   = ap} 
setAPCover ap  = modify $ \s -> s {apCover   = ap} 
setAdvRate  adv = modify $ \s -> s {advRate = adv} 
setAdvCover adv = modify $ \s -> s {advCover  = adv} 

-- when any np is in recover, if not (null parsable) then (+1,+0,+1) else 
--                               if not (null parsefixed) (+1,+1,+0) else (+1,+0,+0)
--         anyway: add newString to sent


emptySaveChunks :: Parser [(Id,[Expr])] 
emptySaveChunks = do
  c <- gets chunks
  modify $ \s -> s {saveChunks = []} 
  return c


addChunks :: [(Id,[Expr])] -> Parser ()
addChunks c = modify $ \s -> s {chunks = chunks s++c} 
addSaveChunks :: [(Id,[Expr])] -> Parser ()
addSaveChunks c = do 
  ranker <- gets rankThem
  cs     <- mapM (shorten ranker) c
  modify $ \s -> s {saveChunks = saveChunks s++cs} 
 where shorten rank (i,ex) 
        | length (top ex) == toplimit = do
                putTrace "warning!! limiting unrestricted"
                return (i,rank $ chunk ex)
        | length (chunk ex) == chunklimit = do
                putTrace "limiting a save chunk"
                return (i,rank $ chunk ex)
        | otherwise = return (i,rank ex)
       chunk ex = take chunklimit ex
       top   ex = take toplimit ex

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

emptyString :: Parser [String] 
emptyString = do
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


sumChunks :: [ChunkInfo] -> String
sumChunks ch = 
  let nr = summ npR ch
      nc = summ npC ch
      ar = summ apR ch
      ac = summ apC ch
      avr = summ advR ch
      avc = summ advC ch
      summ f i = let xs =  map f i
                 in  toEnum (sum $ map ok xs) / toEnum (sum $ map tot xs)
                    
  in unlines ["NP rate "++show nr,"NP cover "++show nc
             ,"AP rate "++show ar,"AP cover "++show ac
             ,"Adv rate "++show avr,"Adv cover "++show avc
             ,"NP length "++ratio npR ch
             ,"AP length "++ratio apR ch
             ,"Adv length "++ratio advR ch
             ,""
             ]
  where ratio f i = let l = concatMap (okLength . f) i
                    in  show $ toEnum (sum l) / toEnum (length l)



instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"++unlines (map (showExpr []) tree)
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"

meta = mkCId "?" 

(+++) :: String -> String -> String
a +++ b = a ++" "++b

combine :: [[a]] -> [[a]]
combine = sequence

(|||) :: [a] -> [a] -> [a]
a ||| b = if null a then b else a

{-
combine [] = [[]]
combine (x:xs) = [a:as | a <- x, as <- combine xs]
-}
