import PGF
import Data.Maybe

pgfFile = "../gf/BigTest.pgf"
testa str = do
  pgf <- readPGF pgfFile
  let typ = fromJust $ (readType "QuantTyped Object") --"NPTyped Subject")
  return $ parse pgf (read "BigTestSwe") typ  str

testState = do
  pgf    <- readPGF pgfFile
  let pstate = initS pgf 
  print $ fst $ getParseOutput pstate (fromJust $ readType "Utt") Nothing 
 where initS pgf = initState pgf  (read "BigTestSwe") (fromJust $ readType "Utt")


test str = do 
  pgf <- readPGF pgfFile
  let res = parseWithRecovery pgf (read "BigTestSwe") (fromJust $ readType "Utt")
                                 [(fromJust $ readType "Adv")] Nothing str
  print res

instance Show ParseOutput where
   show  (ParseOk tree)    = "ParseOK"
   show  (TypeError x)     = "TypeError"
   show  (ParseFailed i)   = "ParseFailed"
   show  (ParseIncomplete) = "Incomplete"

{-

-- state med gammalt träd, nytt träd, mappning mellan utbytta och deras träd
main s tree = do
  let s1 = namesAndCompounds s
  --let s2 = removeUnknown
  let ts = parse s2
      lastStr = if null ts then map simplify ts tree
                           else return s1
      res = parse lastStr
  case res of
       []     -> return metaAttach
       (x:xs) -> return (reconnect xs)


simplify :: Tree String -> m (Tree String,String)

simplify (Node x ts) 


                     | parsabel x = do 
                           let ss = str ts 
                               e = parse ss
                           if null e then  if terminalBr ts then parseFinal x ts
                                           else do 
                                                 subtrees = map simplify ts 
                                                 save (meta x) subtrees
                                                 return (meta x,metaStr x)
                                     else return ss
                     | otherwise = do 
                            let subtrees = map simplify ts
                                sent     = words $ map snd subtrees
                                trees    = map fst subtrees
                            return (Node x trees,sent)
                              
terminalBr (Node x (Node y []:_)) = True
terminalBr _                      = False

parseFinal cat ts = let s = get str
                           
            
metaAttach = do
  tr <- get newTree
  göra meta och sätt på grenar, exakt enligt TB

save new old = do
   putMapping new old
   -}
