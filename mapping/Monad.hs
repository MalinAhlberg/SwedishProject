module Monad ( Rule(..), Grammar, grammar
             , P, parse
             , cat, word, lemma, inside, transform
             , many, many1, opt
             , getDep  -- Malins
             ) where

import Data.Tree
import Data.Char
import Data.List --malin
import Debug.Trace
import qualified Data.Map as Map
import Control.Monad
import PGF hiding (Tree,parse)

infix 1 :->


--- funktion som bara hittar en sak inuti och inte slänger saker på vägen?

data Rule    t e = t :-> P t e e
type Grammar t e = t -> PGF -> Morpho -> [Tree t] -> e

grammar :: (Ord t,Show t) => ([e] -> e) -> [Rule t e] -> Grammar t e
grammar def rules = gr
  where
    gr = \tag ->
      case Map.lookup tag pmap of
        Just f  -> \pgf m ts -> case unP f gr pgf m ts of
                                  Just (e,[]) -> e
                                  _           -> case ts of
                                                   [Node w []] -> def []
                                                   ts          -> def [gr tag pgf m ts | Node tag ts <- ts]
        Nothing -> \pgf m ts -> case ts of
                                  [Node w []] -> def []
                                  ts          -> def [gr tag pgf m ts | Node tag ts <- ts]

    pmap = Map.fromListWith mplus (map (\(t :-> r) -> (t,r)) rules)


newtype P t e a = P {unP :: Grammar t e -> PGF -> Morpho -> [Tree t] -> Maybe (a,[Tree t])}

instance Monad (P t e) where
  return x = P (\gr pgf m ts -> Just (x,ts))
  f >>= g  = P (\gr pgf m ts -> case unP f gr pgf m ts of
                                  Just (x,ts) -> unP (g x) gr pgf m ts
                                  Nothing     -> Nothing)

instance MonadPlus (P t e) where
  mzero     = P (\gr pgf m ts -> Nothing)
  mplus f g = P (\gr pgf m ts -> unP f gr pgf m ts `mplus` unP g gr pgf m ts)


parse :: Grammar t e -> PGF -> Morpho -> Tree t -> e
parse gr pgf morpho (Node tag ts) = gr tag pgf morpho ts

-- ingen lista i singn..
cat :: (Eq t,Show t) => [t] -> P [t] e e
cat tag = P (\gr pgf morpho ts -> 
  case ts of
    (Node tag1 ts1 : ts) |  trace (show tag1 ++ show tag) (tag `isPrefixOf` tag1)
                                       -> Just (gr tag1 pgf morpho ts1,ts)
    _                                  -> Nothing)

word :: Eq t => t -> P t e t
word tag = P (\gr pgf morpho ts -> 
  case ts of
    (Node tag1 [Node w []] : ts) | tag == tag1 -> Just (w,ts)
    _                                          -> Nothing)

inside :: (Eq t,Show t )=> [t] -> P [t] e a -> P [t] e a
inside tag f = P (\gr pgf morpho ts ->
 trace ("inside ") $
  case ts of
    (Node tag1 ts1 : ts) | trace (show tag1 ++ show tag) (tag `isPrefixOf` tag1)
                            -> case unP f gr pgf morpho ts1 of
                                            Just (x,[]) -> Just (x,ts)
                                            _           -> Nothing
    _                       -> Nothing)


{-
later :: Eq t => t -> P t e a -> P t e a
later tag f = P (\gr pgf morpho ts ->
  case ts of
    (Node tag1 ts1 : ts) | tag == tag1 -> case unP f gr pgf morpho ts1 of
                                            Just (x,[]) -> Just (x,ts)
                                            _           -> Nothing
                         | otherwise   -> later tag (\s -> f pgf morpho ts
    _                                  -> Nothing)
-}

lemma :: String -> String -> P String e CId
lemma cat0 an0 = P (\gr pgf morpho ts -> 
  trace ("heer"++show ts++show cat0) $ case ts of
    (Node w [] : ts) -> trace (show [(lemma,an1) | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)
                                    , cat0 == cat1 ])
                                $ case [lemma | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)
                                    , cat0 == cat1 && an0 == an1] of
                          (id:_) -> Just (id,ts)
                          _      -> Nothing
    _                -> Nothing)

transform :: ([Tree t] -> [Tree t]) -> P t e ()
transform f = P (\gr pgf morpho ts -> Just ((),f ts))

--malins
getDep :: Show t =>  P t e t
getDep = P (\gr pgf morpho ts -> 
 trace ("getDep "++show ts) $
  case ts of
   (Node dep ws:ts) -> Just (dep, Node dep ws:ts)
   _                -> Nothing)

many :: P t e a -> P t e [a]
many f = do x  <- f
            xs <- many f
            return (x:xs)
         `mplus`
         do return []

many1 :: P t e a -> P t e [a]
many1 f = do x  <- f
             xs <- many f
             return (x:xs)

opt :: P t e a -> a -> P t e a
opt f x = mplus f (return x)
