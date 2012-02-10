{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, 
             ScopedTypeVariables, FlexibleContexts, TypeSynonymInstances,
             UndecidableInstances #-}
module MonadSP (Rule(..)
               ,Grammar
               ,WriteState
               ,Mapp(..)
               ,grammar
               ,P
               ,parse
               ,parseAs
               ,cat
               ,checkWord
               ,pick
               ,pickInside
               ,word
               ,word2
               ,lemma
               ,lemma'
               ,analyse
               ,inside
               ,insideSuff
               ,transform
               ,many
               ,many1
               ,opt
               ,optEat
               ,consume
               ,wordlookup
               ,write
               ,trace
             ) where
import Data.Tree
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import PGF hiding (Tree,parse)


infix 1 :->


--- funktion som bara hittar en sak inuti och inte slänger saker på vägen?

data Rule    m t e = t :-> P t e m e
type Parser  m t e = PGF -> t -> [Tree t] -> m [e] 
type Def e     = [e] -> e
type Grammar m t e = t -> PGF -> Morpho -> Parser m t e -> ([e] -> e) -> [Tree t] -> m e
type WriteState = ([String],Mapp,Mapp)
data Mapp = Mapp {unMapp :: Map.Map String Int}
  deriving (Show) --TODO

instance Show t => Show (Rule m t e) where
  show (t :-> x) = show t


--grammar :: (MonadWriter WriteState m,MonadState s m,Ord t,Show t,Show e, Ord e) 
--        => Def e -> Parser m t e -> [Rule m t e] -> Grammar m t e
grammar def parser rules = gr 
  where
    gr = \tag pgf m p def ts -> do
      
      let retry = \pgf m p def ts -> case ts of 
            [Node w []] -> return (def [])
            ts          -> def `liftM` sequence [gr tag pgf m p def ts 
                                                | Node tag ts <- ts]
      let getDeeper f = \pgf m p def ts -> do
            stored <- get 
            r <- unP f gr pgf m p def ts 
            case r of
              Just (e,[]) -> return e
              Just (e,xs) -> trace ["Rest parse"] >> retry pgf m p def ts -- use xs here?
              Nothing     -> put stored          >> retry pgf m p def ts
      
      parsed <- parser pgf tag ts
      if null parsed then case Map.lookup tag pmap of
                          Just f   -> failure tag >> getDeeper f pgf m parser def ts 
                          Nothing  -> retry pgf m parser def ts 
                     else success tag >> return (last $ sort parsed) --obs head
    -- If many rules match, try all of them (mplus)
    pmap = Map.fromListWith mplus (map (\(t :-> r) -> (t,r)) rules)


newtype P t e m a = P {unP :: Grammar m t e -> PGF -> Morpho -> Parser m t e -> Def e
                      -> [Tree t] -> m (Maybe (a,[Tree t]))} 

instance Monad m => Monad (P t e m) where
  return x = P $ \gr pgf m p def ts -> return (Just (x,ts))
  f >>= g  = P $ \gr pgf m p def ts -> unP f gr pgf m p def ts >>= \r -> case r of
                                  Just (x,ts') -> unP (g x) gr pgf m p def ts'
                                  Nothing      -> return Nothing

instance MonadState s m => MonadPlus (P t e m) where
  mzero     = P $ \gr pgf m p def ts -> return Nothing
  mplus f g = P $ \gr pgf m p def ts -> do 
    store <- get
    res <- unP f gr pgf m p def ts
    case res of
      Just x  -> return (Just x)
      Nothing -> put store >> unP g gr pgf m p def ts
      
instance MonadState s m => MonadState s (P t e m) where
  put s = P $ \gr pgf m p def ts -> put s >> return (Just ((),ts))
  get   = P $ \gr pgf m p def ts -> get >>= \s -> return (Just (s,ts))
  
instance MonadWriter w m => MonadWriter w (P t e m) where
  tell w = P $ \gr pgf m p def ts -> tell w >> return (Just ((),ts))
  listen = error "listen not implemented for P"
  pass   = error "pass not implemented for P"
                                     
instance Monoid Mapp where
    mempty  = Mapp $ Map.empty 
    mappend (Mapp m1) (Mapp m2) = Mapp $ Map.unionWith (+) m1 m2 
    mconcat = error "mconcat not implemented"

one :: String -> Map.Map String Int
one s = Map.fromList [(s,1)]

write :: MonadWriter WriteState m => String -> P t e m ()
write = trace . return

trace   x = tell (x,mempty,mempty)
success x = tell ([],Mapp $ one x,Mapp $ mempty)
failure x = tell ([],Mapp $ mempty,Mapp $ one x)

instance MonadTrans (P t e) where
  lift m = P $ \gr t morpho p def ts -> m >>= \r -> return (Just (r,ts))

parse :: Monad m => Grammar m t e -> PGF -> Morpho -> Parser m t e -> Def e -> Tree t -> m e
parse gr pgf morpho parser def (Node tag ts) = gr tag pgf morpho parser def ts

silent m     = (m,[])
speak  s (m,w) = (m,s:w)
speaks s (m,w) = (m,s++w)
addS   s m = (m,s)
add    s m = (m,[s])


-- ingen lista i signaturen..
cat :: (Monad m,Eq t,Show t) => [t] -> P [t] e m e
cat tag = P $ catGr tag
catGr :: (Monad m,Eq t,Show t) => [t] -> Grammar m [t] e -> PGF -> Morpho 
      -> Parser m [t] e -> Def e -> [Tree [t]] -> m (Maybe (e,[Tree [t]]))
catGr = \tag gr pgf morpho p def ts ->
  case ts of
    Node tag1 ts1 : ts | tag `isPrefixOf` tag1
                           -> gr tag1 pgf morpho p def ts1 >>= \r -> return (Just (r,ts))
    _                      -> return Nothing



{-
--combineCats :: (Monad m,Eq t,Show t) => [t] -> [[t]] -> P [t] e m e
combineCats topcat tags = P $ \gr pgf m p def ts -> do
   let (ts',ts'') = splitTreeIfSame tags ts 
   if null ts then return Nothing  -- this is not the correct set of categories
              else do parsed <- p pgf topcat ts' 
                      case parsed of
                       [] -> do res <- go gr pgf m p def tags ts' 
                                let exp = map (filterBads def) res
                                return $ Just (def exp,ts'')
                       xs -> return $ Just (head xs,ts'') --obs head
 where go gr pgf m p def ts trs = 
                  do let trs' = map (:[]) trs
                     mapM (\(t,tr) -> 
                            catGr t gr pgf m p def tr) (zip ts trs')
       filterBads def Nothing      = def []
       filterBads def (Just (e,t)) = e
       splitTreeIfSame :: [t] -> [Tree t] -> ([Tree t], [Tree t])
       splitTreeIfSame = undefined --OBS! TODO
       -}
                                          
parseAs :: (MonadWriter WriteState m,Monad m) => String -> P String e m e
parseAs cat = P $ \gr pgf m p def ts -> do
  case ts of
    Node tag1 ts1 : ts' -> 
             do parsed <- p pgf cat ts1
                case parsed of
                   [] -> failure cat >> return Nothing 
                   xs -> success cat >> (return $ Just (head xs,ts')) --obs head
    _                   -> return $ Nothing


-- returns the next word without removing it from the tree
checkWord :: (MonadWriter WriteState m,Monad m,Show t,Eq t) => P [t] e m [t]
checkWord = P $ \gr pgf morpho p def ts -> 
  case ts of
    (Node tag1 [Node tag2 [Node w []]] : rest) -> return $ Just (w,ts)
    _                              -> trace ["checkword fail, "++show ts] >> return Nothing

word :: (Monad m,Show t,Eq t) => [t] -> P [t] e m [t]
word tag = P $ \gr pgf morpho p def ts -> return $
  case ts of
    (Node tag1 [Node w []] : ts) | tag `isPrefixOf` tag1 
                                               -> Just (w,ts)
    _                                          -> Nothing


word2 :: (Monad m,Eq t) => t -> P t e m t
word2 tag = P $ \gr pgf morpho p def ts -> return $
  case ts of
    (Node tag1 [Node tag2 [Node w []]] : ts) | tag == tag1 -> Just (w,ts)
    _                                                      -> Nothing

pick :: Monad m => P [t] e m a -> P [t] e m a 
pick f = P $ \gr pgf morpho p def ts ->  do
   res <- unP f gr pgf morpho p def ts 
   unP (return ()) gr pgf morpho p def ts  --put pack old tree
   return res

pickInside :: (MonadWriter WriteState m,Eq t,Show t, Show a)=> [t] -> P [t] e m a -> P [t] e m a          
pickInside = inside' isPrefixOf (\x -> Just (x,[]))

inside, insideSuff :: (MonadWriter WriteState m,Eq t,Show t,Show a)=> [t] -> P [t] e m a -> P [t] e m a          
insideSuff = inside' isSuffixOf (const Nothing)
inside     = inside' isPrefixOf (const Nothing)

inside' :: (MonadWriter WriteState m,Eq t,Show t,Show a)=>
              ([t] -> [t] -> Bool) -> (a -> Maybe (a, [Tree [t]])) -> [t] -> P [t] e m a -> P [t] e m a          
inside' isEq g tag f = P $ \gr pgf morpho p def ts ->
  case ts of
    Node tag1 ts1 : ts | tag `isEq` tag1 -> do
                trace [show tag++" "++show tag1]
                unP f gr pgf morpho p def ts1 >>= \r -> case r of
                                Just (x,[]) -> return (Just (x,ts))
                                Just (x,xs) -> trace ["inside fail "++show xs++" will return "++show (g x)] >> return (g x)
                                Nothing     -> return Nothing
    _                       -> return Nothing


magicLookup :: (String -> String -> Bool) -> String -> String -> String -> Morpho -> PGF -> [Lemma]
magicLookup f w cat0 an0 morpho pgf = [ lem 
                                | (lem, an1) <- lookupMorpho morpho (map toLower w)
                                , let cat1 = maybe "" (showType []) (functionType pgf lem)
                                , cat0 == cat1 && an0 `f` an1
                                ] 

wordlookup :: MonadWriter WriteState m => String -> String -> String -> P String e m CId
wordlookup w cat0 an0 = P $ \gr pgf morpho p def ts -> do
  trace ["wordlookup: " ++ w ++ show ts ++ show cat0]
  let wds = magicLookup (==) w cat0 an0 morpho pgf
  trace [show wds]
  case wds of
    (wd:_) -> return $ Just (wd,ts)
    []     -> return Nothing
  
lemma' :: MonadWriter WriteState m => String -> String -> P String e m CId
lemma' cat = liftM head . lemmas isPrefixOf cat
  
lemma :: MonadWriter WriteState m => String -> String -> P String e m CId
lemma cat = liftM head . lemmas (==) cat

lemmas :: MonadWriter WriteState m => (String -> String -> Bool) -> String 
       -> String -> P String e m [CId]
lemmas f cat0 an0 = P $ \gr pgf morpho p def ts -> do
   trace ["lemma: "++show ts++show cat0]
   case ts of
     Node w [] : ts -> case magicLookup f w cat0 an0 morpho pgf of
                          (id:ids) -> trace ["lemma ok"] >> return (Just (id:ids,ts))
                          _        -> trace ["no word "++w++cat0++an0]  >> return Nothing
     _              -> trace ["tried to lemma a tag"]    >> return Nothing


analyse :: MonadWriter WriteState m => String -> String -> P String e m String
analyse str tag =  P $ \gr pgf morpho p def ts -> do
    let ana = [an | (lem, an) <- lookupMorpho morpho (map toLower str)
                  , let cat = maybe "" (showType []) (functionType pgf lem)
                  , tag `isPrefixOf` cat ]
    if null ana then return Nothing else return (Just (head ana,ts))

 

transform :: Monad m => ([Tree t] -> [Tree t]) -> P t e m ()
transform f = P $ \gr pgf morpho p def ts -> return (Just ((),f ts))

many :: MonadState s m => P t e m a -> P t e m [a]
many f = do x  <- f
            xs <- many f
            return (x:xs)
         `mplus`
            return []

many1 :: MonadState s m => P t e m a -> P t e m [a]
many1 f = do x  <- f
             xs <- many f
             return (x:xs)

opt :: MonadState s m => P t e m a -> a -> P t e m a
opt f x = mplus f (return x)  

optEat :: MonadState s m => P t e m a -> a -> P t e m a
optEat f x = mplus f (consume >> return x)  
             --consume brought here by Malin!
             --if tex lemma fails, the word shouldn't 
             --stay in the toBeParseTree, is hence consumed


consume :: Monad m => P t e m ()
consume = P $ \gr pgf morpho p def ts ->
  case ts of
   Node x w:ws -> return (Just ((),ws))

