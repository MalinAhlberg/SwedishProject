import PGF
import Control.Monad.State hiding (ap)
import Data.List
import Data.Maybe
import Idents

type Results s = State RState s
data RState = RS {covered :: [Int], parsed :: [Chunk], input :: [String], firstState :: ParseState} 
data Chunk = C {start,end :: Int, trees :: [Tree], typ :: Type}

test str = do
  pgf <- readPGF "../../gf/BigTest.pgf"
  let typ = fromJust $ (readType "NPTyped Subject")
  return $ parse pgf (read "BigTestSwe") typ  str
 
 -- This approac: try find chunks of the highest category possible, and 
 -- then parse the rest. Combine
 -- Another approach: start from beginning, let all types parse as far as they can,
 -- where they stop they let all other continue.

 {- Will get a problem cause we need to decide wether NP, AP, Adv, Pron, Quant
    and Det are prons or subj. Pron ok, cause only Obj when 'sig'. The other need
    types when not super-notcomplex. Try both and return wanted -}

doit str = do
  pgf <- readPGF pgfFile
  let state = initS pgf 
      s = evalState (doParse (0,0) state startType str) (initRState str state)
                                     ----tråda och börja på olika positioner! eller nej.
                                     ----de som börjar tidigare kan döda de andra om de blir klara?
  return s
 where initS pgf = initState pgf lang startType
-- alt : 
  --doParse (0,0) initS (allTypes) str  ----tråda och börja på olika positioner!

doParse :: (Int,Int) -> ParseState -> Type -> [String] -> Results ()
doParse (i,j) state typ (s:str) = let nextTok = simpleParseInput s in
     case nextState state nextTok of
          Right pst -> doParse (i,j+1) pst typ str  -- assumes that if this state is bad, we will get error
                                                    -- next time
          Left er   -> 
             case fst (getParseOutput state typ Nothing) of--save this info
                  ParseOk trees -> do
                      tell $ C i j trees typ
                      newState <- gets firstState
                      doParse (j+1,j+1) newState typ (str)  --start anew from next word
                  er            -> doParse (i,j+1)state typ str

doParse (i,j) state typ [] = do
   chs <- unparsed  {-:: [(Start,[String]] -- the parts of sentence that are left-}
   case chs of
        (x:xs) -> do
                   newState <- gets firstState
                   mapM_  (\(n,s) -> sequence_ $ (mapM (doParse (n,n) newState) (nextTypes typ)) s) chs --tråda dessa
      --   :: (Int,[String]) -> [Results ()]
        []     -> do --nothing left to parse
                   ps <- gets parsed  
                   let res = combine ps
                   return ()
   --choose the longest partions that cover all parts
   --combine
   return ()

combine :: [Chunk] -> [Tree]
combine chs = undefined

-- States
initRState str = RS [] [] str
tell :: Chunk -> Results ()
tell (C i j e t) = modify $ \s -> s {covered = [i..j] `union` covered s, parsed = (C i j e t):parsed s}

unparsed :: Results [(Int,[String])]
unparsed = undefined

pgfFile = undefined
lang = undefined
startType = text 
nextTypes :: Type -> [Type]
nextTypes t | t == text = [phr]
            | t == phr  = [utt]
            | t == utt  = [s]
            | t == s    = [cl]
            | t == cl   = class5
            | t `elem` class5 = class6
            | t `elem` class6 = [idet,card,art,rp,ada,subj,conj,voc,pol]
class5 = [vp,npsub,advsub]
class6 = [npobj,advobj,v,cn,detobj,detsub
         ,predet,apsub,apobj,rcl]

allTypes = [text,phr,utt,s,cl]++class5++class6

