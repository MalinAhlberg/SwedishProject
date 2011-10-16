module EncodeSw where

import Char
import Frontend

import qualified Data.Map as Map

sw_encodings = Map.insert "parole" (Just parole_pos,Just parole_inhs,Just parole_param) (Map.insert "SUC" (Just suc_pos,Just suc_inhs,Just suc_param) Map.empty)

parole_pos  _ = ""

parole_inhs _ = []

parole_param (pos,p,inhs) = 
 case pos of
   "pm" -> case words p of
             xs | elem "nom" xs -> "NP00N@0S"
             xs | elem "gen" xs -> "NP00G@0S"
             _ -> parole_param ("nn",p,inhs)
   "ab" -> case p of
             "invar" -> "RG0S"
             "pos"   -> "RGPS"
             "komp"  -> "RGCS"
             "super" -> "RGSS"
             "c"     -> "RG0C"
             "sms"   -> "RG0C"
             _ -> error $ "ab " ++ p
   "av" -> case p of
             "pos indef sg u nom"     -> "AQPUSNIS"
             "pos indef sg u gen"     -> "AQPUSGIS"
             "pos indef sg n nom"     -> "AQPNSNIS"
             "pos indef sg n gen"     -> "AQPNSNIS"
             "pos indef pl nom"       -> "AQPNSNIS"
             "pos indef pl gen"       -> "AQP0PG0S"
             "pos def sg no_masc nom" -> "AQP0SNDS"
             "pos def sg no_masc gen" -> "AQP0SGDS"
             "pos def sg masc nom"    -> "AQPMSNDS"
             "pos def sg masc gen"    -> "AQPMSGDS"
             "pos def pl nom"         -> "AQP0PN0S"
             "pos def pl gen"         -> "AQP0PG0S"
             "komp nom"               -> "AQC00N0S"
             "komp gen"               -> "AQC00G0S"
             "super indef nom"        -> "AQS0PNIS"
             "super def no_masc nom"  -> "AQS00NDS"
             "super def masc nom"     -> "AQSMSNDS"
             "super def masc gen"     -> "AQSMSGDS"
             "c"                      -> "AQC0000C"
             "sms"                    -> "AQC0000C"
             "super indef gen"        -> "AQS0PGIS"
             "super def no_masc gen"  -> "AQS00GDS"
             "invar"                  -> "AQP00N0S"
             _ -> error $ "av " ++ p
   "nn" -> case p of
            "sg indef nom" -> "NC" ++ pgender inhs ++ "SN@IS"
            "sg indef gen" -> "NC" ++ pgender inhs ++ "SG@IS"
            "sg def nom"   -> "NC" ++ pgender inhs ++ "SN@DS"
            "sg def gen"   -> "NC" ++ pgender inhs ++ "SG@DS"
            "pl indef nom" -> "NC" ++ pgender inhs ++ "PN@IS"
            "pl indef gen" -> "NC" ++ pgender inhs ++ "PG@IS"
            "pl def nom"   -> "NC" ++ pgender inhs ++ "PN@DS"
            "pl def gen"   -> "NC" ++ pgender inhs ++ "PG@DS"
            "ci"               -> "NC000@0C"
            "cm"               -> "NC000@0C"
            "sms"              -> "NC000@0C"
            _ -> error $ "nn " ++ p
   "al" ->  case p of
              "sg u indef" -> "DI@US@S"
              "sg n indef" -> "DI@NS@S"
              "sg u def" -> "DF@US@S"
              "sg n def" -> "DF@NS@S"
              "pl indef"   -> "DI@0P@S"
              "pl def"   -> "DF@0P@S"
              _ -> error $ "al " ++ p
   "ie" -> "CIS"
   "in" ->  "I"
   "kn" -> "CCS"
   "nl" ->  
       case p of
         "nom num u"       -> "MCUSNIS"
         "nom num n"       -> "MCNSNIS"
         "nom ord no_masc" -> "MO00N0S"
         "nom ord masc"    -> "MOMSNDS"  
         "gen num u"       -> "MC00G0S"
         "gen num n"       -> "MC00G0S"
         "gen ord no_masc" -> "MO00G0S"
         "gen ord masc"    -> "MCMSGDS"
         _                 -> "*"
   "sn" -> "CSS"
   "pp" -> "SPS"
   "pn" -> case p of
             "nom"        -> "PF@00S@S"
             "ack"        -> "PF@00O@S"
             "poss sg u"  -> "PS@US0@S"
             "poss sg n"  -> "PS@N00@S"
             "poss pl"    -> "PS@0P0@S"
             "sg u nom"   -> "PF@US0@S"
             "sg n nom"   -> "PF@NS0@S"
             "sg u gen"   -> "PS@000@S"        
             "sg n gen"   -> "PS@000@S"
             "pl nom"     -> "PF@0PS@S"
             "pl gen"     -> "PS@000@S"
             _ -> parole_param ("av",p,inhs)
   "vb" -> case p of
             "pres ind aktiv"                -> "V@IPAS"
             "pres ind s-form"               -> "V@IPPS"              
             "pres konj aktiv"               -> "V@SPAS"              
             "pret ind aktiv"                -> "V@IIAS"               
             "pret ind s-form"               -> "V@IIPS"              
             "pret konj aktiv"               -> "V@SIAS"              
             "pret konj s-form"              -> "V@SIPS"             
             "imper"                         -> "V@M0AS"                       
             "inf aktiv"                     -> "V@N0AS"                 
             "inf s-form"                    -> "V@N0PS"           
             "sup aktiv"                     -> "V@IUAS"     
             "sup s-form"                    -> "V@IUPS"
             "pres_part nom"                 -> "AP000N0S"
             "pres_part gen"                 -> "AP000G0S"
             "pret_part indef sg u nom"      -> "AF0USNIS"
             "pret_part indef sg u gen"      -> "AF0USGIS"
             "pret_part indef sg n nom"      -> "AF0NSNIS"
             "pret_part indef pl nom"        -> "AF00PN0S"
             "pret_part indef pl gen"        -> "AF00PG0S"
             "pret_part def sg no_masc nom"  -> "AF00SNDS"
             "pret_part def sg no_masc gen"  -> "AF00SGDS"
             "pret_part def sg masc nom"     -> "AF0MSNDS"
             "pret_part def sg masc gen"     -> "AF0MSGDS"
             "pret_part def pl nom"          -> "AF00PN0S"
             "pret_part def pl gen"          -> "AF00PG0S"
             "c"                             -> "V@000C"
             "sms"                           -> "V@000C"             
             "pres konj s-form"              -> "V@IPSS"             
             "pret_part indef sg n gen"      -> "AF0NSGIS"
             _ -> error $ "vb " ++ p
   "aba" -> "RG0A"
   "ava" -> "AQ00000A"
   "kna" -> "CCA"
   "nna" -> "NC000@0A"
   "pma" -> "NP00N@0A"
   "ppa" -> "SPC"
   "vba" -> "V@000A"
   "vbm" -> (++ "M") $ init $ parole_param ("vb", filt p,inhs)
   "avm" -> (++ "M") $ init $ parole_param ("av",filt p,inhs)
   "abm" -> (++ "M") $ init $ parole_param ("ab",filt p,inhs)
   "inm" -> (++ "M") $ init $ parole_param ("in",filt p,inhs)
   "nlm" -> (++ "M") $ init $ parole_param ("nl",filt p,inhs)
   "nnm" -> (++ "M") $ init $ parole_param ("nn",filt p,inhs)
   "pmm" -> (++ "M") $ init $ parole_param ("pm",filt p,inhs)
   -- "pnm" -> (++ "M") $ init $ parole_param ("pn",filt p,inhs)
   "ppm" -> (++ "M") $ init $ parole_param ("pp",filt p,inhs)
   _    -> "*" -- "abh" "avh" "sxc" "snm" "ssm" 
 where filt x = unwords (filter (\(c:_) -> not (isDigit c)) (words x))



pgender ["n"] = "N"
pgender ["v"] = "0"
pgender _     = "U"

suc_pos x = maybe x id $ Map.lookup x (Map.fromList pos)
 where
  pos =
   [
    ("av","JJ"),
    ("al","DT"),
    ("kn","KN"),
    ("ie","IE"),
    ("sn","SN"),
    ("in","IN"),
    ("nl","RG"),
    ("nn","NN"),
    ("pm","PM"),
    ("pn","PN"),
    ("ab","PL"),
    ("ab","AB"),
    ("pp","PP"),
    ("vb","VB")
   ]
    
   -- ("PC","vb"),
   -- ("AN",""),
   -- ("-",""),
   -- ("UO","")
    --("DT","pn"),
    --("HD","pn"),
    --("DL",""),
    --("MAD",""),
    --("MID",""),
    --("PAD",""),
   -- ("RO","nr"),
    --("HS","pn"),
    --("HP","pn"),
    --("PS","pn"),
    -- ("HA","ab"),

suc_inhs xs =  [maybe s id (Map.lookup s (Map.fromList inhs)) | s <- xs]
 where
   inhs =[
        ("v","UTR/NEU"), -- ?
        ("n","NEU"),
        ("u","UTR")
         ]



suc_param (pos,p,inhs) = unwords $ [ maybe s id (Map.lookup s (Map.fromList param)) | s <- words p]
 where
  param = [("inf","INF"),
           ("konj","KON"),
           ("pret","PRT"),
           ("aktiv","AKT"),
           ("s-form","SFO"),
           ("sup","SUP"),
           ("imper","IMP"),
           ("super","SUV"),
           ("komp","KOM"),
           ("ci","SMS"),
           ("cm","SMS"),
           ("pos","POS"),
           ("pret_part","PRF"),
           ("pl","PLU"),
           ("gen","GEN"),
           ("nom","NOM"),
           ("sg","SIN"),
           ("def","DEF"),
           ("masc","MAS"),
           ("indef","IND"),
           ("pres","PRS"),
           ("n","NEU"),
           ("u","UTR"),
           ("ack","OBJ"),
           ("invar","-")
        --   ("pret_part","PC PRF"),
        --   ("pres_part","PC PRF"),
           --("stark",""),
           --("svag",""),
           --("num",""),
           --("ord",""),
           --("ind",""),
           --("no_masc","")
          ]
           -- ("SIN/PLU",""),
           --    ("SUB/OBJ",""),
           --    ("SUB",""),
           -- ("IND/DEF",""),

