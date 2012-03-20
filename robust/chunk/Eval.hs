import Control.Applicative
import Control.Arrow hiding (first)
import Control.Monad
import Data.Tree
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Format as Form
import System.IO

import qualified NpChunk as NP

type Id = String
data Result = Result {idN :: Id, isText :: Bool, total :: Bool, mets :: [String], first :: String
                     ,tooBig :: Bool}
 deriving (Show)

tb = "../../Talbanken05_20060604/FPS/P.tiger.xml" 

-- counts the number of sentences for which we could parse the structure, how many that were
-- totally parsed and how many metas that were in them
main = do
   inp <- readFile "night/advTest1Res" --"helaTBAdv"
   let ss   = splitWhen null $ lines inp
       oks  = map mkLine $ filter (not . null) ss
       res  = (length oks,length (filter isText oks),length (filter (\x -> isText x && total x) oks)
              ,length (filter tooBig oks),map (head &&& length) $ group $ sort $ concatMap mets oks)
   appendFile "night/ResultsAdv1" $ show res ++"\n"++ show oks 

isOk l = okStart l -- || hasChunk l
whole x = not $ "Meta" `isInfixOf` x
       
-- for a sentence, returns the relevant data (considering only the first parse tree) 
mkLine :: [String] -> Result  
mkLine (i:xs) = let nice     = any isOk xs
                    semiNice = any hasChunk xs
                    which    = if not nice && semiNice then (firstS . last) else head
                in  Result i nice (whole $ which xs) (if nice then metas $ which xs else [])
                           (which xs) (not nice && semiNice)
 where metas         = filter ("Meta" `isInfixOf`)  . words
   
       firstS :: String -> String
       firstS string = let (start,(p:rest)) = span (/='(') string
                       in start++(p:takeParent "" rest 1)
          where takeParent str rest 0 = str
                takeParent str (x:xs) n | x=='('    = takeParent (str++[x]) xs (n+1)
                                        | x==')'    = takeParent (str++[x]) xs (n-1)
                                        | otherwise = takeParent (str++[x]) xs n


okStart l = "isText" `isPrefixOf` l || "isUtt" `isPrefixOf` l


-- hasChunk also counts the sentences for which there were more parse trees than could be
-- rated, but have instead been just limited and ranked by the first (20-1000) sentences
hasChunk l = let (s,rest ) = span (=='s') l
                 (ns,rest') = span isDigit rest
                 ok         = "_501" `isPrefixOf` rest'
                 end        = drop 6 rest'
             in length s ==1 && not (null ns) && ok && okStart end

-- counts the number of words in each skipped chunk
countmetalength = do
   putStrLn "reading test"
   inp <- readFile "night/npTest1" 
   let ss   = splitWhen null $ lines inp
       oks  = map mkLine $ filter (not . null) ss
       ms   = concatMap (metas . words . first) oks
   putStrLn "splitting done"
   is  <- chunkLength ms
   appendFile "Metalength" $ show (toEnum (sum is)/toEnum (length is)) ++"\n"++show is

-- collects the identifiers of the meta chunks in one sentence (worded)
metas :: [String] -> [Id]
metas ws = let (x,xs) = break (isSuffixOf "Meta") ws
               f = toId . take 1 . drop 1  
           in  if null xs then []
                          else f xs : metas (drop 2 xs)
  where toId :: [Id] -> Id
        toId [x] = filter (`notElem` "\"()") x
        toId _   = []

-- check is Talbanken how long each chunk is
chunkLength :: [Id] -> IO [Int]
chunkLength ids = do
  putStrLn "will parse trees"
  hSetEncoding stdin utf8
  allTrees <- concat <$> Form.parseIdTree tb
  putStrLn "have parsed trees"
  --putStrLn $ "a tree "++ show (head allTrees)
  let idSs  = map ((++"_501") . fst . break (=='_')) ids
      trees = filter ((`elem` idSs) . fst) allTrees  
  putStrLn $ "a idS "++ show (head idSs)
  mapM (findChunks trees) (zip idSs ids)
 where findChunks :: [(Id,Tree (Id,String))] -> (Id,Id) -> IO Int
       findChunks trees (sId,chId) = do
   --       putStrLn $ "a tree "++ show (head trees)
          putStrLn $ "fromJusting "++ sId
          let theTree = fromJust $ lookup sId trees
          putStrLn $ "fromJusting "++ chId
          let theNode = fromJust $ findNode chId theTree
          return $ countLeaves theNode
       findNode n node@(Node (m,_) ts) | n==m      = Just node
                                       | otherwise = msum $ map (findNode n) ts
                                       
-- counts the number of leaves in a tree
countLeaves :: Tree a -> Int
countLeaves (Node w []) = 1
countLeaves (Node n ts) = sum $ map countLeaves ts
              
-- counts the average sentence length in Talbanken
sentenceLength = do
  hSetEncoding stdin utf8
  allTrees <- drop 1900 . take 2323 . concat <$> Form.parseIdTree tb
  allTrees1 <- take 220 . concat <$> Form.parseIdTree tb
  return $ ratio $ map (countLeaves . snd) (allTrees++allTrees1)

npchunks = toEnum (sum $ map fst NP.nps) / toEnum (sum $ map snd NP.nps)

ratio :: [Int] -> Double
ratio xs = (toEnum  $ sum xs) / (toEnum $ length xs)

{-
              

     tree = getTree i tb
    
      -- hitta metas i meningen, ta ut den delen ur trädet, 
      -- hitta alla bimeningar (id, metas i den) : lista A
      -- gå igenom trädet, för varje nod: se om noden är i listan A,
      -- om inte, gå vidare, om ja (x i A) , kolla på x:s barn (=nya metas)
      -- 
      ms = metas tree
      countMetas tree ms
      

      check (Node x []:ns) ys miss 
              | x `elem` (map fst ys) = check ns ys miss
              | otherwise        = check ns ys (x:miss)
      check (Node x ts:ns) ys miss 
              | isJust inList    = check (ns++ps) ys miss
          where inList = lookup x ys
                ps     = fromJust inList
      check (Node x ts:ns) ys miss | otherwise  = check (ns++ts) ys miss

   nodes :: [String] -> [(Id,[Id])]
   nodes = [(head ws, metas ws) | l <- ls, i++"_" `isPrefixOf` l, let ws = words l] 

   metas :: String -> [Id]
   metas ws = let (x,xs) = span (`isSuffix` "Meta") $ words ws
                  f = toId . take 1 . drop 1  
              in  if null xs then []
                             else f xs: meta (drop 2 xs)


   countMetas tree ms = map count ms
     count m = 

-}
{-
- ta första av varje (efter tomrad)
      kolla om den börjar med isUtt eller isText
     räkna!

- för alla som börjar med isUtt eller isText, kolla hur många Metas.
- kolla hur många ord som finns i metat, och som inte finns senare i 
  listan
  -}
