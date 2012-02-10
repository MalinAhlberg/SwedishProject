import PGF hiding (Tree)
import Data.Tree
import Data.Tree.Zipper
import Control.Monad.State
import Data.Maybe

pgfFile = "../../gf/BigTest.pgf"
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

data PState = PS {replaced :: Map String Expr, tbTree :: TreePos a (Int,String) }

-- state med gammalt träd, nytt träd, mappning mellan utbytta och deras träd
main tree = do
  let s1 = namesAndCompounds (getStr s)
  --let s2 = removeUnknown
  exchangeLeaves s1
  let ts = parse s1
  res <- if null ts then map simplify ts tree >>= bruteParse
                    else return s1
  expr <- resetChanged
  return expr

parseFunction state cat (s:str) | paranthesis nextTok = do
                                 let (inP,rest) = splitAt (endPar nextTok)
                                 tree1 <-  parse inP newState
                                 tree2 <-  parse resst state
                                 return (meta [tree2,tree1])
                                | otherwise = do
                                  let nextTok = simpleParseInput s 
                                      pstate  = nextState state nextTok
                                  parseFunction pstate cat str
parseFunction state cat []   = return state 

simplify :: Tree String -> m [Expr] --,String)
{- What do we really want here?? onödigt?? maybe if we add a gf rule (VPStr : V2 -> Obj -> Str):
simplify (Node "S" ts) = do --parseNP, parseFV +parseOA etc
                 let ss = str ts 
                     e  = parseLowLevel x ss
                 if not ok then group (FV,VG) parse together as (Node "_VP" [FV,VG]
                                simplify rest
                                tryparse 
                                -}
simplify (Node p [Node w []]) | parseable p = do
                                  let t = parseAs p ws
                                  case t of
                                    [] -> return (mkApp (toGF p) [meta])
                                    xs -> xs
                              | otherwise   = simplify (Node x ts) 
                              | parseabel x = findAndFixError
                          {- 
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
                            -}
                     | otherwise = mapM simplify ts 
{-                            let subtrees = map simplify ts
                                sent     = words $ map snd subtrees
                                trees    = map fst subtrees
                            return (Node x trees,sent)-}
 --       checkIfOk = parseTree x --uses tree form state
 --                       if null ss' then return (mkApp meta subs) else return ss'

findAndFixError = do
   s <- parseTree
   case s of
     ParseFailed i   -> bruteParse (getSubTree i) >> findAndFixError  --parse kids in this subtree and exchange by meta ++ kids
     ParseOk  tr     -> return tr
     TypeError x     -> exchange (toGF w) meta  >> return (mkExpr meta)
     ParseIncomplete -> do tr <- parseOpen all --Add ?? at end, parse with all types open and try to get a tree.
                           if null tr then do w <- getCurrentNode 
                                              exchange (toGF w) meta
                                              return (mkExpr meta)
                                      else return tr

bruteParse (Node w ts) = do kids <- mapM simplify ts --exchange this in tree, add trace in state
                            exchange (toGF w) kids
                            return (mkApp meta kids)
                        
                              
exchange pos kids = do
  tree <- gets tbTree
  modify $ \s -> s {tbTree    = setTree (Node pos []) (tbTree s),
                    replaced  = insert pos kids $ replaced s}

exchangeLeaves ws = do
   tree <- liftM toTree $  gets tbTree
   let ([],tree') = change tree ws
   modify $ \s -> s {tbTree = tree'}
 where change' (Node x []) (w:ws) = (ws,Node w [])
       change' (Node x ts) ws     = let (ws',ts') = change ts ws 
                                    in  (ws',Node x ts')
       change (t : ts)  ws = let (ws',t')     = change' t ws
                                 (ws'',ts')  = change ts ws'
                             in (ws'',t' : ts')
getCurrentNode = liftM label $ gets
getSubTree i = --find CHild


meta     = mkCId "?"
mkExpr e = mkApp e []
toGF     = id -- to be replaced with fancy function
{-
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
-}
