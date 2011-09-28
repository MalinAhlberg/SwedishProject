{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Text.XML.HXT.Core 
import Data.Ord
import Data.List
import System.Environment
import Debug.Trace
import Data.Char
import XMLHelp 
import Control.Monad.State

{-
  Parses XML to text files
  usage: runghc XmlParser [-nva / -num n] xmlFile outFile
  By using the  -nva flag, lists of all verbs, nouns and adjectives are extracted
  By using the  -num n flag, hundred sentences of max n words are extracted
                             where unknown words are replaced by dummy ones.
                             (unknown are specified in XMLHelp.unknown)
  Otherwise, all sentences are processed and written to the outFile
-}

data NVALists = NVA {verbs :: [String],nouns :: [String], adjs :: [String]}
type Result a = State NVALists a

main = do
  args <- getArgs
  case args of
       ("-nva":as) -> mainNVA as
       ("-num":as) -> mainNormal as
       as          -> mainAll as

mainAll [src,dst] = mainF src dst $ arrIO (return . formatForPGF . toParsedS) 
mainNormal [src,dst,n] = mainF src dst $ processListWL (read n)
  
processListWL :: Int -> IOSArrow SList String
processListWL n = arrIO (return . formatForPGF . list) 
  where list = takeGoodWords 100 n 4 . sortBy (comparing lengthS) 
             . toParsedSSimp

toParsedS :: SList -> [ParsedS]
toParsedS = map (\(sId,ws) -> let norm = filter (nodots . snd) ws in
   ParsedS {idNo = sId,lengthS = length norm, str = map snd ws})
toParsedSSimp :: SList -> [ParsedS]
toParsedSSimp = map (\(sId,ws) -> let norm = filter (nodots . snd) ws in
   ParsedS {idNo = sId,lengthS = length norm, str = map simplify ws})

mainNVA (src:args) = 
  runX (xunpickleDocument xpSList  [withInputEncoding utf8
                                  , withRemoveWS yes] src
        >>> doIt 
        >>> arrIO (mapM_ (uncurry writeFile)))


doIt :: IOSArrow SList [(FilePath,String)]
doIt = arrIO (return . getWords . extract)
getWords :: NVALists -> [(FilePath,String)]
getWords nvas= [("Verbs.txt",fix (verbs nvas)),
                ("Nouns.txt",fix (nouns nvas)),
                ("Adj.txt",  fix (adjs nvas))]
  where fix = unlines . map (map toLower)

extract :: SList -> NVALists
extract = (`execState` emptyNVA) . mapM_ (mapM_ addWord . snd)
                           
addWord :: Word -> Result ()
addWord ("VV",v) = modify (addV v)
addWord ("AJ",a) = modify (addA a)
addWord ("NN",a) = modify (addN a)
addWord _        = return ()

addV v s = NVA {verbs = v:verbs s, nouns = nouns s,adjs = adjs s}
addN n s = NVA {verbs = verbs s, nouns = n:nouns s,adjs = adjs s}
addA a s = NVA {verbs = verbs s, nouns = nouns s,adjs = a:adjs s}

emptyNVA = NVA {verbs = [],nouns = [],adjs = []}


