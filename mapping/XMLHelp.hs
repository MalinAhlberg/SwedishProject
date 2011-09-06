{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module XMLHelp where
import Prelude hiding (words,id)
import Text.XML.HXT.Core 
import Data.Ord
import Data.List hiding (words)
import Data.Maybe
import qualified Data.Map as M
import System.Environment
import Debug.Trace as Debug
import Data.Char
import qualified Data.Tree as T
import Control.Monad.State
 
-- Functions for parsing XML to a format read by the other Haskell files

data Word     = W  {id :: Id, pos :: Tag, word :: String}
data PhrTag   = Ph {idPh :: Id, cat :: Tag, tags :: [(Tag,Id)]}
data Sentence = Sent {rootS :: Id, words :: [Word], info :: [PhrTag]}
type Tag      = String
type Id       = String

instance Show Sentence where
  show s@(Sent r w info) = showa r s

-- gets the trees into a nice format
showa :: String -> Sentence -> String
showa nr s@(Sent root ws inf) = 
     case (lookup' nr ws,lookup'' nr inf) of
       (Just w,_) -> putWord w
       (_,Just p) -> putPhrase p
  where putWord (W i p w) = par $ p++" "++w
        putPhrase (Ph i c t) =  
             par $ c++" "++unwords (map (\(t,next) -> par $ t++showa next s) t) 
        lookup' y (w@(W x _ _):xs) | y ==x     = Just w
                                   | otherwise = lookup' y xs
        lookup' y [] = Nothing
        lookup'' y (w@(Ph x _ _):xs) | y ==x     = Just w
                                     | otherwise = lookup'' y xs
        lookup'' y [] = Nothing
        par s = "("++s++")"
  


instance Show Word where
  show (W id pos w) = "id "++id++" pos "++pos++" word "++w
  
instance Show PhrTag where
  show (Ph id cat tag) = "id "++id++" cat "++cat++" tags "++show tag

 
instance XmlPickler [Word] where
  xpickle = xpWords

instance XmlPickler Sentence where
  xpickle = xpSentence

xpSentences :: PU [Sentence] 
xpSentences = xpElem "corpus" 
              $ xpWrap (snd, \a -> ((),a))
              $ xpPair (xpElem "head" $ xpUnit) (xpElem "body" $ xpList $ xpSentence)
xpTags :: PU [PhrTag]
xpTags = xpList $ xpElem "nt"
         $ xpWrap (uncurry3 Ph,\p -> (idPh p,cat p,tags p))
         $ xpTriple (xpAttr "id" xpText) (xpAttr "cat" xpText)
                     (xpList $ xpTagMap)

xpTagMap :: PU (Tag,String)
xpTagMap = xpElem "edge"
           $ xpPair (xpAttr "label" xpText)
                    (xpAttr "idref" xpText)

xpSentence :: PU Sentence  
xpSentence = xpElem "s"
             $ xpWrap (uncurry3 Sent,\s -> (rootS s,words s, info s))
             $ xpElem "graph"  
             $  xpTriple (xpAttr "root" xpText)
                         ( xpElem "terminals" xpWords)
                         ( xpElem "nonterminals" xpTags)
xpWords :: PU [Word]
xpWords = xpList $ xpElem "t"  
          $ xpWrap (uncurry3 W,\t -> (id t, pos t,word t)) 
          $ xpTriple (xpAttr "id" xpText)
                     (xpAttr "pos" xpText)
                     (xpAttr "word" xpText)
                     
mainF src =   
  runX (xunpickleDocument xpSentences [withInputEncoding utf8
                                     , withRemoveWS yes] src
        >>> arrIO (putStrLn . unlines . map (show . toTree'))) -- (writeFile dst . show)) 

toTree' s@(Sent root ws inf) = toTree root s
toTree :: String -> Sentence -> T.Tree String
toTree nr s@(Sent root ws inf) = 
     case (lookup' nr ws,lookup'' nr inf) of
       (Just w,_) -> putWord w
       (_,Just p) -> putPhrase p
  where putWord (W i p w) = T.Node p [T.Node w []]
        putPhrase (Ph i c t) = T.Node c 
                                $ map (\(tag,next) -> T.Node tag  [toTree next s]) t
        lookup' y (w@(W x _ _):xs) | y ==x     = Just w
                                   | otherwise = lookup' y xs
        lookup' y [] = Nothing
        lookup'' y (w@(Ph x _ _):xs) | y ==x     = Just w
                                     | otherwise = lookup'' y xs
        lookup'' y [] = Nothing
 

