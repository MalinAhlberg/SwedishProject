{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}
{- module MonadSP  ( Rule(..), Grammar, grammar
             , P, parse
             , cat, word, word2, lemma, inside, transform
             , many, many1, opt
             , optEat, consume, wordlookup,write -- Malins
             ) where
-}
module MonadSP where
import Data.Tree
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import PGF hiding (Tree,parse)
-- import qualified Monad as P

infix 1 :->

test = True
trace' = if test then trace else flip const

--- funktion som bara hittar en sak inuti och inte slänger saker på vägen?

data Rule  m s t e = t :-> P m s t e e
type Grammar m t e = t -> PGF -> Morpho -> [Tree t] -> m e

instance Show t => Show (Rule m s t e) where
  show (t :-> x) = show t

grammar :: (MonadWriter [String] m,Ord t,Show t,Show e) => ([e] -> e) -> [Rule m s t e] -> s -> Grammar m t e
grammar def rules sinit = gr
  where
    gr = \tag ->
      case Map.lookup tag pmap of
        -- f :: P s t e e 
        Just f  -> \pgf m ts -> unP f gr pgf m ts sinit >>= \r -> case r of
              Just (e,_,[]) -> return e
              Just (e,_,xs) -> tell ["Rest parse"] >> return (def [])
              _           -> case ts of
                                  [Node w []] -> return (def [])
                                  ts          -> def `liftM` sequence [gr tag pgf m ts | Node tag ts <- ts]
        Nothing -> \pgf m ts -> case ts of
                                  [Node w []] -> return (def [])
                                  ts          -> def `liftM` sequence [gr tag pgf m ts | Node tag ts <- ts]

    -- If many rules match, try all of them (mplus)
    pmap = Map.fromListWith mplus (map (\(t :-> r) -> (t,r)) rules)


newtype P m s t e a = P {unP :: Grammar m t e -> PGF -> Morpho -> [Tree t] -> s -> m (Maybe (a,s,[Tree t]))} 

instance Monad m => Monad (P m s t e) where
  return x = P $ \gr pgf m ts s -> return (Just (x,s,ts))
  f >>= g  = P $ \gr pgf m ts s -> unP f gr pgf m ts s >>= \r -> case r of
                                  Just (x,s',ts') -> unP (g x) gr pgf m ts' s'
                                  Nothing         -> return Nothing

instance Monad m => MonadPlus (P m s t e) where
  mzero     = P $ \gr pgf m ts s -> return Nothing
  mplus f g = P $ \gr pgf m ts s -> liftM2 mplus (unP f gr pgf m ts s) (unP g gr pgf m ts s)
                                     
instance Monad m => MonadState s (P m s t e) where
  put s = P $ \gr p m ts _ -> return (Just ((),s,ts))
  get   = P $ \gr p m ts s -> return (Just (s,s,ts))

write :: MonadWriter w m => w -> P m s t e ()
write w = P $ \gr pgf m ts s -> tell w >> return (Just ((),s,[]))


parse :: Monad m => Grammar m t e -> PGF -> Morpho -> Tree t -> m e
parse gr pgf morpho (Node tag ts) = gr tag pgf morpho ts

silent m     = (m,[])
speak  s (m,w) = (m,s:w)
speaks s (m,w) = (m,s++w)
addS   s m = (m,s)
add    s m = (m,[s])


-- ingen lista i signaturen..
cat :: (Monad m,Eq t,Show t) => [t] -> P m s [t] e e
cat tag = P $ \gr pgf morpho ts s ->
  case ts of
    Node tag1 ts1 : ts | tag `isPrefixOf` tag1
                                       -> gr tag1 pgf morpho ts1 >>= \r -> return (Just (r,s,ts))
    _                                  -> return Nothing

word :: (Monad m,Show t,Eq t) => [t] -> P m s [t] e [t]
word tag = P $ \gr pgf morpho ts s -> return $
  case ts of
    (Node tag1 [Node w []] : ts) | tag `isPrefixOf` tag1 
                                               -> Just (w,s,ts)
    _                                          -> Nothing


word2 :: (Monad m,Eq t) => t -> P m s t e t
word2 tag = P $ \gr pgf morpho ts s -> return $
  case ts of
    (Node tag1 [Node tag2 [Node w []]] : ts) | tag == tag1 -> Just (w,s,ts)
    _                                                      -> Nothing




inside :: (MonadWriter [String] m,Eq t,Show t)=> [t] -> P m s [t] e a -> P m s [t] e a          
inside tag f = P $ \gr pgf morpho ts s ->
  case ts of
    Node tag1 ts1 : ts | tag `isPrefixOf` tag1 -> do
                            tell [show tag++" "++show tag1]
                            unP f gr pgf morpho ts1 s >>= \r -> case r of
                                            Just (x,s',[]) -> return (Just (x,s',ts))
                                            Just (x,s',xs) -> tell ["inside fail "++show xs] >> return Nothing
                                            Nothing        -> return Nothing
    _                       -> return Nothing



{-
insideTake :: (Eq t,Show t )=> [t] -> P s [t] e a -> P s [t] e a
insideTake tag f = P (\gr pgf morpho ts ->
  case ts of
    (Node tag1 ts1 : ts) | trace' (show tag++" "++show tag1) (tag `isPrefixOf` tag1)
                            -> case unP f gr pgf morpho ts1 of
                                            Just (x,[]) -> Just (x,ts)
                                            Just (x,xs) -> Just (x,Node tag1 xs: ts)
                                            Nothing      -> Nothing
    _                       -> Nothing)
-}

-- This was used in wordlookup and lemma, refactored here to avoid copy-paste.
magicLookup :: String -> String -> String -> Morpho -> PGF -> [Lemma]
magicLookup w cat0 an0 morpho pgf = [ lem 
                                | (lem, an1) <- lookupMorpho morpho (map toLower w)
                                , let cat1 = maybe "" (showType []) (functionType pgf lem)
                                , cat0 == cat1 && an0 == an1
                                ] 

wordlookup :: MonadWriter [String] m => String -> String -> String -> P m s String e CId
wordlookup w cat0 an0 = P $ \gr pgf morpho ts s -> do
  tell ["wordlookup: " ++ w ++ show ts ++ show cat0]
  let wds = magicLookup w cat0 an0 morpho pgf
  tell [show wds]
  case wds of
    (wd:_) -> return $ Just (wd,s,ts)
    []     -> return Nothing
  
  
lemma :: MonadWriter [String] m => String -> String -> P m s String e CId
lemma cat = liftM head . lemmas cat

lemmas :: MonadWriter [String] m => String -> String -> P m s String e [CId]
lemmas cat0 an0 = P $ \gr pgf morpho ts s -> do
   tell ["lemma: "++show ts++show cat0]
   case ts of
     Node w [] : ts -> case magicLookup w cat0 an0 morpho pgf of
                          (id:ids) -> tell ["lemma ok"] >> return (Just (id:ids,s,ts))
                          _        -> tell ["no word"]  >> return Nothing
     _              -> tell ["tried to lemma a tag"]    >> return Nothing


transform :: Monad m => ([Tree t] -> [Tree t]) -> P m s t e ()
transform f = P $ \gr pgf morpho ts s -> return (Just ((),s,f ts))

many :: Monad m => P m s t e a -> P m s t e [a]
many f = do x  <- f
            xs <- many f
            return (x:xs)
         `mplus`
         do return []

many1 :: Monad m => P m s t e a -> P m s t e [a]
many1 f = do x  <- f
             xs <- many f
             return (x:xs)

opt :: (MonadWriter [String] m) => P m s t e a -> a -> P m s t e a
opt f x = write ["opt"] >> mplus f (return x)  

optEat :: (MonadWriter [String] m) => P m s t e a -> a -> P m s t e a
optEat f x = write ["optEat"] >> mplus f (consume >> return x)  --consume brought here by Malin!
                                         --if tex lemma fails, the word shouldn't 
                                         --stay in the toBeParseTree, is hence consumed
consume :: Monad m => P m s t e ()
consume = P $ \gr pgf morpho ts s ->
  case ts of
   Node x w:ws -> return (Just ((),s,ws))

----
type C = Int
color :: C -> String -> String
color c s = fgcol c ++ s ++ normal

normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)


fgcol :: Int -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"


red,green :: C
red = 1
green = 2 
 
