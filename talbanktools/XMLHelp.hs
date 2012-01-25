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
import Data.Function hiding (id)
 
-- Functions for parsing XML to a format read by the other Haskell files

data Word     = W  {id :: Id, word :: String, pos :: Tag}
data PhrTag   = Ph {idPh :: Id, cat :: Tag, tags :: [(Tag,Id)]}
data Sentence = Sent { idS :: Id, rootS :: Id, words :: [Word]
                     , info :: [PhrTag], ws :: Int}
type Tag      = String
type Id       = String

cmpword :: Word -> Word -> Ordering
cmpword (W id1 _ _) (W id2 _ _) = compare (g id1) (g id2)
  where
    g :: String -> Integer
    g = read . drop 1 . dropWhile (/= '_') 

instance Eq Word where
  (==) = ((EQ ==) .) . cmpword


instance Ord Word where
  compare = cmpword


instance Show Sentence where
  show s@(Sent id r w info _) = showa r s

getSentence :: Sentence -> (Id, String)
getSentence s = let sent = concat . intersperse " " . map word . words in
      (idS s,sent s)

getSentence' :: Sentence -> (Id, [String])
getSentence' s = let sent = map word . words in
      (idS s,sent s)

getSentenceTagged :: Sentence -> (Id,[(Tag,String)])
getSentenceTagged s = let wds = map word $ words s
                          tgs = map pos $ words s 
                      in (idS s,zip tgs wds)
  where  readId i | null n    = 1000 
                  | otherwise = fst $ head n
            where n = reads (tail $ dropWhile (/='_') i) :: [(Int,String)]

-- gets the trees into a nice format
showa :: String -> Sentence -> String
showa nr s@(Sent id root ws inf _) = 
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
             $ xpWrap (makeSentence,\s -> (idS s,(rootS s,words s, info s)))
             $ xpPair
             ( xpAttr "id" xpText)
             $ xpElem "graph"  
             $  xpTriple (xpAttr "root" xpText)
                         ( xpElem "terminals" xpWords)
                         ( xpElem "nonterminals" xpTags)
  where makeSentence (i,(r,ws,tgs)) = Sent i r ws tgs (length ws)

xpWords :: PU [Word]
xpWords = xpList $ xpElem "t"  
          $ xpWrap (uncurry3 W,\t -> (id t, word t,pos t)) 
          $ xpTriple (xpAttr "id" xpText)
                     (xpAttr "word" xpText)
                     (xpAttr "pos" xpText)
                     
mainF src f =   
  runX (xunpickleDocument xpSentences [withInputEncoding utf8
                                     , withRemoveWS yes] src
        >>> arrIO f) 

toNumberedTree :: Sentence -> (String,T.Tree String)
toNumberedTree s@(Sent id root ws inf _) = (root,toTree' root s)

toTree s@(Sent id root ws inf _) = toTree' root s
toTree' :: String -> Sentence -> T.Tree String
toTree' nr s@(Sent id root ws inf _) = 
     case (lookup' nr ws,lookup'' nr inf) of
       (Just w,_) -> putWord w
       (_,Just p) -> putPhrase p
       _          -> error $ "Error in toTree' "++show nr++" could not be found"
  where putWord (W i w p) = T.Node p [T.Node w []] -- corrected 22/11, may cause confusion in old code
        putPhrase (Ph i c t) = T.Node c 
                                $ map (\(tag,next) -> T.Node tag  [toTree' next s]) t
        lookup' y (w@(W x _ _):xs) | y ==x     = Just w
                                   | otherwise = lookup' y xs
        lookup' y [] = Nothing
        lookup'' y (w@(Ph x _ _):xs) | y ==x     = Just w
                                     | otherwise = lookup'' y xs
        lookup'' y [] = Nothing
 
parse src =
  runX (xunpickleDocument xpSentences [withInputEncoding utf8
                                     , withRemoveWS yes] src
        >>> arrIO (return . map toNumberedTree)) 


