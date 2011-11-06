{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SaldoXML where
import Text.XML.HXT.Core 
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Map as M
import System.Environment
import Debug.Trace as Debug
import Data.Char
import Data.ByteString.Char8 hiding (break,take,concat)
import qualified Data.Tree as T
import Control.Monad.State
import Control.Arrow
import Control.Applicative
 
-- Functions for parsing XML to a format read by the other Haskell files

type Lex     = M.Map String Entry
type LexList =  [(String,Entry)]
data Entry = E {pos :: ByteString, table :: [(ByteString,String)]}
  deriving Show

instance XmlPickler Lex where
  xpickle = xpLex
  
parseDict :: String -> IO (Maybe Lex)
parseDict d = listToMaybe <$> mainF xpLex d return

parseDictList :: String -> IO (Maybe LexList)
parseDictList d = listToMaybe <$> mainF xpLexList d return

xpLex = xpElem "Lexicon"
         $ xpWrap (M.fromList,M.toList)
            $ xpList xpEntry

xpLexList = xpElem "Lexicon"
            $ xpList xpEntry

xpEntry :: PU (String,Entry)
xpEntry = xpElem "LexicalEntry"
            $ xpWrap ((\(lem,_,_,_,pos,_,table) -> 
                         --(nameWord (gf,pos),E (pack pos) table))
                         (lem,E (pack pos) table))
                     ,(\(w,E p t)  -> (w,Just w,unnameWord w,w,unpack p,"-",t)))
            $ xp7Tuple
              (xpElem "lem" xpText)
              xpSaldos
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

-- very ugly indeed, how to solve this?
xpSaldos :: PU (Maybe String)
xpSaldos = xpWrap (\(x,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_) -> x
                 , \x -> (x,x,x,x,x ,x,x,x,x,x, x,x,x,x,x ,x,x,x,x,x)) 
           $ xp20Tuple 
             maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
             maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
             maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
             maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
           
  where maybeSaldo = xpOption $ xpElem "saldo" xpText

nameWord :: (String,String) -> String
nameWord (name,tag) = name++"_"++toGF tag
unnameWord = fst . break (=='_')  
toGF :: String -> String
toGF "av" = "A" 
toGF "nn" = "N" 
toGF "vb" = "V" 
toGF "ab" = "Adv" 
toGF x    = x


mainF xpFunc src f =   
  runX (xunpickleDocument xpFunc [withInputEncoding utf8
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


