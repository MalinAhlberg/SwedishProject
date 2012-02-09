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
-------------------------------------------------------------------------------

-- state med gammalt träd, nytt träd, mappning mellan utbytta och deras träd
main tree = do
  let s1 = namesAndCompounds (getStr s)
  --let s2 = removeUnknown
  let ts = parse s2
      (lastStr,trees) = if null ts then map simplify ts tree
                                   else return s1
      res = parse lastStr
  case res of
       []     -> return metaAttach
       (x:xs) -> return (reconnect xs)

whenParsing (nextTok:ts) state | paranthesis nextTok = let (inP,rest) = splitAt (endPar nextTok)
                                                       tree1 <-  parse inP newState
                                                       tree2 <-  parse resst state
                                                       return (meta [tree2,tree1])


simplify :: Tree String -> m (Tree String, [Expr]) --,String)
{- What do we really want here?? onödigt?? maybe if we add a gf rule (VPStr : V2 -> Obj -> Str):
simplify (Node "S" ts) = do --parseNP, parseFV +parseOA etc
                 let ss = str ts 
                     e  = parseLowLevel x ss
                 if not ok then group (FV,VG) parse together as (Node "_VP" [FV,VG]
                                simplify rest
                                tryparse 
                                -}
simplify (Node p [Node w []]) = let t = parseAs p ws
                                case t of
                                    [] -> return (mkApp (toGF p) [meta])
                                    xs -> xs
simplify (Node x ts) 
                     | parseabel x = do 
                           let ss = str ts 
                               e  = parseLowLevel x ss
                           s' <- case e of
                             ParseFailed i   -> bruteParse (getSubTree i)  --parse kids in this subtree and exchange by meta ++ kids
                             ParseOk  tr     -> return tr
                             TypeError x     -> bruteParse (Node x ts)
                             ParseIncomplete -> return (mkApp meta (fromlastState))
                           checkIfOk
                           --if null e then  if terminalBr ts then parseFinal x ts
                            --               else do 
                     | otherwise = do 
                            let subtrees = map simplify ts
                                sent     = words $ map snd subtrees
                                trees    = map fst subtrees
                            return (Node x trees,sent)
  where bruteParse (Node w ts) = do kids <- mapM simplify ts --exchange this in tree, add trace in state
                                    return (mkApp meta kids)
        checkIfOk = let ss' = parse s
                        if null ss' then return (mkApp meta subs) else return ss'
                              
terminalBr (Node x (Node y []:_)) = True
terminalBr _                      = False

parseFinal cat ts = let (pos,s) = getTerminals str
                    parseWithRecovery cat pos ts
                           
getTerminals (Node p [Node w _]:xs) = let (ps,ws) = getTerminals xs
                                      in (p:ps,w:ws)
            
metaAttach = do
  tr <- get newTree
  göra meta och sätt på grenar, exakt enligt TB

save new old = do
   putMapping new old
