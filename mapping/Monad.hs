module Monad ( Rule(..), Grammar, grammar
             , P, parse
             , cat, word, word2, lemma, inside, transform
             , many, many1, opt
             , optEat, consume, insideTake -- Malins
             ) where

import Data.Tree
import Data.Char
import Data.List --malin
import Debug.Trace
import qualified Data.Map as Map
import Control.Monad
import PGF hiding (Tree,parse)

infix 1 :->

test = False
trace' = if test then trace else flip const

--- funktion som bara hittar en sak inuti och inte slänger saker på vägen?

data Rule    t e = t :-> P t e e
type Grammar t e = t -> PGF -> Morpho -> [Tree t] -> e

grammar :: (Ord t,Show t,Show e) => ([e] -> e) -> [Rule t e] -> Grammar t e
grammar def rules = gr
  where
    gr = \tag ->
      case Map.lookup tag pmap of
        Just f  -> \pgf m ts -> case unP f gr pgf m ts of
                                  Just (e,[]) -> e
                                  Just (e,xs) -> trace (color red "\n\nrestParse!\n\n") e
                                  _           -> case ts of
                                                   [Node w []] -> def []
                                                   ts          -> def [gr tag pgf m ts | Node tag ts <- ts]
        Nothing -> \pgf m ts -> case ts of
                                  [Node w []] -> def []
                                  ts          -> def [gr tag pgf m ts | Node tag ts <- ts]

    pmap = Map.fromListWith mplus (map (\(t :-> r) -> (t,r)) rules)

{-
continue = 
   Node w [] -> 
   ts        -> 
   -}

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

-- ingen lista i signaturen..
cat :: (Eq t,Show t) => [t] -> P [t] e e
cat tag = P (\gr pgf morpho ts -> 
  case ts of
    (Node tag1 ts1 : ts) | (tag `isPrefixOf` tag1)
                                       -> Just (gr tag1 pgf morpho ts1,ts)
    _                                  -> Nothing)

word :: (Show t,Eq t) => [t] -> P [t] e [t]
word tag = P (\gr pgf morpho ts -> 
  case ts of
    (Node tag1 [Node w []] : ts) | tag `isPrefixOf` tag1 -> Just (w,ts)
    _                                          -> Nothing)

word2 :: Eq t => t -> P t e t
word2 tag = P (\gr pgf morpho ts -> 
  case ts of
    (Node tag1 [Node tag2 [Node w []]] : ts) | tag == tag1 -> Just (w,ts)
    _                                                      -> Nothing)


inside :: (Eq t,Show t )=> [t] -> P [t] e a -> P [t] e a
inside tag f = P (\gr pgf morpho ts ->
  case ts of
    (Node tag1 ts1 : ts) | trace' (show tag++" "++show tag1) (tag `isPrefixOf` tag1)
                            -> case unP f gr pgf morpho ts1 of
                                            Just (x,[]) -> Just (x,ts)
                                            Just (x,xs) -> trace' ("inside fail "++show xs) 
                                                            Nothing
                                            Nothing      -> Nothing
    _                       -> Nothing)

insideTake :: (Eq t,Show t )=> [t] -> P [t] e a -> P [t] e a
insideTake tag f = P (\gr pgf morpho ts ->
  case ts of
    (Node tag1 ts1 : ts) | trace' (show tag++" "++show tag1) (tag `isPrefixOf` tag1)
                            -> case unP f gr pgf morpho ts1 of
                                            Just (x,[]) -> Just (x,ts)
                                            Just (x,xs) -> Just (x,Node tag1 xs: ts)
                                            Nothing      -> Nothing
    _                       -> Nothing)



lemma :: String -> String -> P String e CId
lemma cat0 an0 = P (\gr pgf morpho ts -> 
  trace' ("lemma: "++show ts++show cat0) $ case ts of
    (Node w [] : ts) -> {-trace' (show [(lemma,an1,an0,cat1,cat0) | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)] ) $-}
                         case [lemma | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)
                                    , cat0 == cat1 && an0 == an1] of
                          (id:_) -> trace' "lemma ok" $ Just (id,ts)
                          _      -> trace' "no word" Nothing
    _                -> trace' "tried to lemma a tag" Nothing)

transform :: ([Tree t] -> [Tree t]) -> P t e ()
transform f = P (\gr pgf morpho ts -> Just ((),f ts))

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
optEat :: P t e a -> a -> P t e a
optEat f x = mplus f (consume >> return x)  --consume brought here by Malin!
                                         --if tex lemma fails, the word shouldn't 
                                         --stay in the toBeParseTree, is hence consumed
--consume :: P t e a
consume = P (\gr pgf morpho ts ->
  case ts of
   (Node x w:ws) -> Just ((),ws))

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

