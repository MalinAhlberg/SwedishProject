{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SaldoXML where
import Text.XML.HXT.Core 
import Data.Ord
import Data.List hiding (words)
import Data.Maybe
import qualified Data.Map as M
import System.Environment
import Debug.Trace as Debug
import Data.Char
import Data.ByteString.Char8 hiding (break)
import qualified Data.Tree as T
import Control.Monad.State
import Control.Arrow
 
-- Functions for parsing XML to a format read by the other Haskell files

type Lex   = [Entry] --M.Map String Entry
data Entry = E {word :: String, pos :: ByteString, table :: [(ByteString,String)]}
  deriving Show
{-
data Noun = NDef { sg_indef_nom, sg_indef_gen, sg_def_nom, sg_def_gen,
                   pl_indef_nom, pl_indef_gen, pl_def_nom, pl_def_gen
                   :: String}
              
data Adj = ADef {pos_indef_sg_u_nom, pos_indef_sg_u_gen,
                 pos_indef_sg_n_nom, pos_indef_sg_n_gen,
                 pos_indef_pl_nom, pos_indef_pl_gen,
                 pos_def_sg_no_masc_nom, pos_def_sg_no_masc_gen,
                 pos_def_sg_masc_nom, pos_def_sg_masc_gen,
                 pos_def_pl_nom, pos_def_pl_gen
                 :: String}

data Verb = VDef { pres_ind_aktiv, pres_ind_sform, pres_konj_aktiv,
                   pres_konj_sform, pret_ind_aktiv, pret_ind_sform,
                   pret_konj_aktiv, pret_konj_sform,imper, inf_aktiv,
                   inf_sform, sup_aktiv, sup_sform, pres_part_nom,
                   pres_part_gen :: String}
-}


instance XmlPickler Lex where
  xpickle = xpLex
  
xpLex = xpElem "Lexicon"
            $ xpList xpEntry

xpEntry :: PU Entry
xpEntry = xpElem "LexicalEntry"
            $ xpWrap ((\(_,_,gf,_,pos,_,table) -> 
                        E (nameWord (gf,pos)) (pack pos) table)
                     ,(\(E w p t)  -> (w,w,unnameWord w,w,unpack p,"-",t)))
            $ xp7Tuple
              -- (xp6Tuple 
              (xpElem "lem" xpText)
              (xpElem "saldo" xpText)
              (xpElem "gf" xpText)
              (xpElem "p" xpText)
              (xpElem "pos" xpText)
              (xpElem "inhs" xpText)
              (xpElem "table" xpTable)

xpTable :: PU [(ByteString,String)]
xpTable = xpList $ xpElem "form"
          $ xpWrap (first pack,first unpack)
          $ xpPair 
           (xpElem "param" xpText)
           (xpElem "wf"    xpText)

nameWord :: (String,String) -> String
nameWord (name,tag) = name++"_"++toGF tag
unnameWord = fst . break (=='_')  
toGF :: String -> String
toGF "av" = "A" 
toGF "nn" = "N" 
toGF "vb" = "V" 
toGF "ab" = "Adv" 
toGF x    = x


mainF src f =   
  runX (xunpickleDocument xpLex [withInputEncoding utf8
                                     , withRemoveWS yes] src
        >>> arrIO f) --(return . f)) 


  {-
  <LexicalEntry>
    <lem>fort..ab.1</lem>
    <saldo>fort..1</saldo>
    <gf>fort</gf>
    <p>ab_1_fort</p>
    <pos>ab</pos>
    <inhs>-</inhs>
    <table>
     <form><param>pos</param><wf>fort</wf></form>
     <form><param>komp</param><wf>fortare</wf></form>
     <form><param>super</param><wf>fortast</wf></form>
     <form><param>c</param><wf>fort</wf></form>
     <form><param>c</param><wf>fort-</wf></form>
     <form><param>sms</param><wf>fort-</wf></form>

    </table>
   </LexicalEntry>
-}

{-
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
                     

toNumberedTree :: Sentence -> (String,T.Tree String)
toNumberedTree s@(Sent id root ws inf _) = (root,toTree' root s)

toTree s@(Sent id root ws inf _) = toTree' root s
toTree' :: String -> Sentence -> T.Tree String
toTree' nr s@(Sent id root ws inf _) = 
     case (lookup' nr ws,lookup'' nr inf) of
       (Just w,_) -> putWord w
       (_,Just p) -> putPhrase p
       _          -> error $ "Error in toTree' "++show nr++" could not be found"
  where putWord (W i p w) = T.Node p [T.Node w []]
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


-}
