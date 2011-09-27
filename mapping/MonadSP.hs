{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module MonadSP  ( Rule(..), Grammar, grammar
             , P, parse
             , cat, word, word2, lemma, inside, transform
             , many, many1, opt
             , optEat, consume, wordlookup,write -- Malins
             ) where

import Data.Tree
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Arrow ((***))
import PGF hiding (Tree,parse)
-- import qualified Monad as P

infix 1 :->

test = True
trace' = if test then trace else flip const

--- funktion som bara hittar en sak inuti och inte slänger saker på vägen?

data Rule  s t e = t :-> P s t e e
type Grammar t e = t -> PGF -> Morpho -> [Tree t] -> e

instance Show t => Show (Rule s t e) where
  show (t :-> x) = show t

grammar :: (Ord t,Show t,Show e) => ([e] -> e) -> [Rule s t e] -> s -> Grammar t (e,[String])
grammar def rules sinit = gr
  where
    -- What happens with sharing here?
    sgr = \tag pgf m ts -> fst (gr tag pgf m ts)
    gr = \tag ->
      case Map.lookup tag pmap of
        -- f :: P s t e e 
        Just f  -> \pgf m ts -> case unP f sgr pgf m ts sinit of
              (Just (e,_,[]),w) ->  (e,w)
              -- Just (e,xs) ->  trace (color red "\n\nrestParse!\n\n") e
              (_,w)           -> case ts of
                               [Node _ []] -> (def [],w)
                               ts          -> let res = [gr tag pgf m ts | Node tag ts <- ts]
                                              in (def (map fst res),w++concatMap snd res)
        Nothing -> \pgf m ts -> case ts of
              [Node w []] -> (def [],[])
              ts          -> let res = [gr tag pgf m ts | Node tag ts <- ts] 
                             in (def (map fst res),concatMap snd res)

    -- If many rules match, try all of them (mplus)
    pmap = Map.fromListWith mplus (map (\(t :-> r) -> (t,r)) rules)


newtype P s t e a = P {unP :: Grammar t e -> PGF -> Morpho -> [Tree t] -> s -> (Maybe (a,s,[Tree t]),[String])}

instance Monad (P s t e) where
  return x = P (\gr pgf m ts s -> (Just (x,s,ts),[]))
  f >>= g  = P $ \gr pgf m ts s -> case unP f gr pgf m ts s of
                                  (Just (x,s',ts'),ws) -> case unP (g x) gr pgf m ts' s' of
                                                              (Just y,ws')  -> (Just y,ws++ws')
                                                              (Nothing,ws') -> (Nothing,ws++ws')
                                  (Nothing,ws)        -> (Nothing,ws)

superduperfunktionen :: (a -> b -> c) -> (a' -> b' -> c') -> (a,a') -> (b,b') -> (c,c')
superduperfunktionen f g (x,x') (y,y') = (f x y,g x' y')

instance MonadPlus (P s t e) where
  mzero     = P $ \gr pgf m ts s -> (Nothing,[])
  mplus f g = P $ \gr pgf m ts s -> case unP f gr pgf m ts s of
                                     (Just x,ws) -> (Just x,ws)
                                     (Nothing,ws) -> let (res,w) = unP g gr pgf m ts s
                                                     in  (res,ws++w)
                  
              --  superduperfunktionen mplus (++) (unP f gr pgf m ts s) (unP g gr pgf m ts s)
                  -- parallell states

instance MonadState s (P s t e) where
  put s = P (\gr p m ts _ -> (Just ((),s,ts),[]))
  get   = P (\gr p m ts s -> (Just (s,s,ts),[]))

write :: String -> P s t e ()
write w = P (\gr pgf m ts s -> (Just ((),s,ts),[w]))

parse :: Grammar t e -> PGF -> Morpho -> Tree t -> e
parse gr pgf morpho (Node tag ts) = gr tag pgf morpho ts

silent m     = (m,[])
speak  s (m,w) = (m,s:w)
speaks s (m,w) = (m,s++w)
addS   s m = (m,s)
add    s m = (m,[s])


-- ingen lista i signaturen..
cat :: (Eq t,Show t) => [t] -> P s [t] e e
cat tag = P $ \gr pgf morpho ts s -> silent $
  case ts of
    (Node tag1 ts1 : ts) | (tag `isPrefixOf` tag1)
                                       -> Just (gr tag1 pgf morpho ts1,s,ts)
    _                                  -> Nothing

word :: (Show t,Eq t) => [t] -> P s [t] e [t]
word tag = P (\gr pgf morpho ts s -> silent $
  case ts of
    (Node tag1 [Node w []] : ts) | tag `isPrefixOf` tag1 
                                               -> Just (w,s,ts)
    _                                          -> Nothing)

word2 :: Eq t => t -> P s t e t
word2 tag = P (\gr pgf morpho ts s -> silent $
  case ts of
    (Node tag1 [Node tag2 [Node w []]] : ts) | tag == tag1 -> Just (w,s,ts)
    _                                                      -> Nothing)


inside :: (Eq t,Show t )=> [t] -> P s [t] e a -> P s [t] e a
inside tag f = P (\gr pgf morpho ts s -> 
  case ts of
    (Node tag1 ts1 : ts) | (tag `isPrefixOf` tag1)
                            ->  speak (show tag++" "++show tag1) 
                                  $ case unP f gr pgf morpho ts1 s of
                                            (Just (x,s',[]),w) -> trace' ("inside "++show w) $ addS w $ Just (x,s',ts)
                                            (Just (x,s',xs),w) -> addS (("inside fail "++show xs):w) 
                                                                     Nothing
                                            (Nothing,w)      -> addS w Nothing
    _                       -> silent Nothing)

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
wordlookup :: String -> String -> String -> P s String e CId
wordlookup w cat0 an0 = P (\gr pgf morpho ts s -> 
     do
        let wds = [lemma | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)
                                    , cat0 == cat1 && an0 == an1] 
        speaks (("wordlookup: "++w++show ts++show cat0) : [(show [(lemma,an1,an0,cat1,cat0) | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)] )]) $
          case wds of
               (wd:_)  -> silent $ Just (wd,s,ts)
               []      -> silent Nothing 
       )

lemma :: String -> String -> P s String e CId
lemma cat = liftM head . lemmas cat
lemmas :: String -> String -> P s String e [CId]
lemmas cat0 an0 = P (\gr pgf morpho ts s -> 
   speak ("lemma: "++show ts++show cat0) $
    case ts of
    (Node w [] : ts) -> {-trace' (show [(lemma,an1,an0,cat1,cat0) | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)] ) $-}
                         case [lemma | (lemma, an1) <- lookupMorpho morpho (map toLower w)
                                    , let cat1 = maybe "" (showType []) (functionType pgf lemma)
                                    , cat0 == cat1 && an0 == an1] of
                          (id:ids) -> add "lemma ok"  $ Just (id:ids,s,ts)
                          _      -> add "no word" Nothing
    _                -> add "tried to lemma a tag" Nothing)

transform :: ([Tree t] -> [Tree t]) -> P s t e ()
transform f = P (\gr pgf morpho ts s -> silent $ Just ((),s,f ts))

many :: P s t e a -> P s t e [a]
many f = do x  <- f
            xs <- many f
            return (x:xs)
         `mplus`
         do return []

many1 :: P s t e a -> P s t e [a]
many1 f = do x  <- f
             xs <- many f
             return (x:xs)

opt :: P s t e a -> a -> P s t e a
opt f x = write "opt" >> mplus f (return x)  
optEat :: P s t e a -> a -> P s t e a
optEat f x = write "optEat" >> mplus f (consume >> return x)  --consume brought here by Malin!
                                         --if tex lemma fails, the word shouldn't 
                                         --stay in the toBeParseTree, is hence consumed
--consume :: P t e a
consume = P (\gr pgf morpho ts s ->
  case ts of
   (Node x w:ws) -> silent $ Just ((),s,ws))

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
