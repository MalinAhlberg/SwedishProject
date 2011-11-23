import XMLHelp hiding (id,words)
import SaldoXML
import Text.XML.HXT.Core hiding (Tree,trace)
import Debug.Trace
import Data.List
import Data.Tree
import Data.Char
import Data.Function
import Data.ByteString.Char8 (pack)
 
-- For extrating certian combination of tags from Talbanken

mainet f src 
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO (return . f))

input = "../Talbanken05_20060604/FPS/P.tiger.xml"

main = do
  putStrLn "Starting ..."
  putStrLn "Reading xml ..."
  xml <- mainet (map toNumberedTree) input
  putStrLn "Finding tags ..."
  --let avps = nub $ concatMap (extractTag "AVP") $ map snd $ concat xml
  let avps = nub $ concatMap (extractTagDeep "ID") $ map snd $ concat xml
  putStrLn "Writing file ..."
  writeFile "ID.tags" $ unlines $ map show avps
  writeFile "ID.txt"  $ unlines $ map printSentences avps

containTag :: String -> Tree String -> Bool
containTag t (Node cat [tr]) = posIs t tr 
containTag t _               = False
  
-- returns the trees that returns words of type tag.
-- the result contains the label, the cat, and then a tree containing the tag
-- we prefer as long branches as possible 
extractTagDeep :: String -> Tree String -> [Tree String]
extractTagDeep tag tr@(Node cat [Node lab [Node cat2 ts]]) 
               | any (containTag tag) ts = [tr]
extractTagDeep tag tr@(Node cat [Node lab ts]) 
               | any (containTag tag) ts = [tr]
extractTagDeep tag (Node t ts) = concatMap (extractTagDeep tag) ts

-- for extracting all trees of type tag.  (all subtrees starting with tag)
extractTag :: String -> Tree String -> [Tree String]
extractTag tag (Node t ts) | tag == t  = [Node t ts]
                           | otherwise = concatMap (extractTag tag) ts


printSentences :: Tree String -> String
printSentences (Node c ts) = c++"\t"++ unwords (concatMap getWords ts)
  where getWords :: Tree String -> [String]
        getWords (Node w []) = [w]
        getWords (Node w ts) = concatMap getWords ts

posIs,wordIs,wordCatIs :: String -> Tree String -> Bool
wordIs t (Node w []) = t == w
wordIs t _           = False
posIs t (Node pos [Node _ []]) = t == pos
posIs t _                      = False
wordCatIs t (Node c [Node p [Node w []]]) = t == w
wordCatIs t _                             = False

test :: Tree String
test = Node "S" [Node "UK" [Node "XP" [Node "HD" [Node "ID" [Node "ett" []]], Node "HD" [Node "två" []]]]
                ,Node "K" []]
