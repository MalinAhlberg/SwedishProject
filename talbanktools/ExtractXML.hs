module ExtractXML where
import XMLHelp hiding (id,words)
import qualified XMLHelp as X
--import SaldoXML
import Text.XML.HXT.Core hiding (Tree,trace)
import Debug.Trace
import Data.List
import Data.Tree
import Data.Char
import Data.Function
import Data.ByteString.Char8 (pack)
import Control.Monad.State
import System.Environment
 
-- For extrating certian combination of tags from Talbanken

mainet f src 
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO (return . f))

input = "../Talbanken05_20060604/FPS/P.tiger.xml"

mainEt 	:: String -> String -> IO ()
mainEt src out
    = do
      runX ( xunpickleDocument xpSentences
                                [withInputEncoding utf8
                                , withRemoveWS yes] src
	        >>>
	        arrIO (return  . concatMap getTags)
	        >>>
            xpickleDocument xpSentences
                            [withIndent yes,
                             withInputEncoding utf8] (out++".xml"))

      return ()
main = do
  (src:out:_) <- getArgs
  putStrLn "Starting ..."
  putStrLn "Reading xml ..."
  xml <- mainEt src out
  putStrLn "Done!"
  --let avps = nub $ concatMap (extractTag "AVP") $ map snd $ concat xml

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

countSentences :: [Sentence] -> Double
countSentences = (\x -> toEnum (sum x)/toEnum (length x)) . map ws
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
                


type SState = State Sentence
emptyS :: Sentence -> PhrTag -> Sentence
emptyS s phr = Sent { idS = idS s ++idPh phr , rootS = idPh phr, X.words = []
                     , info = [phr], ws = 0}
getTags :: Sentence -> [Sentence]
getTags s@(Sent id root ws inf i) = 
   [execState (mapM ((>> reverseRes) . getNr . snd) (tags phr)) (emptyS s phr) | phr <- inf, cat phr == "NP"]
     where
        reverseRes :: SState ()
        reverseRes = modify $ \s -> s {X.words = sort (X.words s),
                                       info    = reverse (info s)}
        getNr :: Id -> SState () 
        getNr  nr = case (lookup' nr ws,lookup'' nr inf) of
                      (Just w,_) -> modify $ \s -> s {X.words = w : X.words s }
                      (_,Just p) -> putPhrase p 
                      _         -> error $ "Error in toTree' "++show nr++" could not be found"
        putPhrase p@(Ph i c t) = do
              modify $ \s -> s {info = p : info s}
              mapM_ (\(tag,next) -> getNr next) t                       
        lookup' y (w@(W x _ _):xs) | y ==x     = Just w
                                   | otherwise = lookup' y xs
        lookup' y [] = Nothing
        lookup'' y (w@(Ph x _ _):xs) | y ==x     = Just w
                                     | otherwise = lookup'' y xs
        lookup'' y [] = Nothing
 


