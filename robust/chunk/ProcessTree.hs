{-# LANGUAGE PatternGuards, RankNTypes #-}
module ProcessTree where
import PGF
import Control.Monad.Writer
import Data.List as List
import Debug.Trace
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Tree
import Data.Tree.Zipper hiding (first)
import Compound

-- compound: kattpälsen  -> [(katt &+ pälsen)]
-- ne      : Johan Ros   -> ["X1",""](
--TODO write names to state!!

type ProcessState = Writer ([Lemma], [(Int,String)])
type WorkingTree  = TreePos Full (String,String)

processTree :: Bool -> Lex -> Morpho -> Int -> WorkingTree -> ProcessState WorkingTree
processTree b lex morpho i tree  | isCompound =  saveSms >> moveOn (setLabel' sms tree)
                                 | isFut      = moveOn (setLabel' "ska" tree) -- hackerish way to handle "skall"
--                                 | isUVerb    =  moveOn' $ setLabel vx tree
                                 | isNumber   =  moveOn' $ setLabel' nx tree
                                 | isName     =  saveName >> (moveOn' $ exchangeNames i tree)
                                 | b          =  trace "put to lower" $ moveOnSave $ setLabel' word tree  --is first word, put to lower
                                 | otherwise  =  moveOnSave $ tree

  where isCompound    = trace ("work on word "++word) $ isUnknown && length cmps > 1
        isNumber      = isUnknown && numbertag pos
        --isUVerb       = isUnknown && verbtag pos
        isName        = isUnknown 
        isFut         = pos =="SVPS"
        word          = (if b then lower else id) (snd $ label tree)
        --vx            = ("VX"++show i)   --TODO maybe look at tag?
        nx            = "1"  
        isUnknown     = null lemmas
        lemmas        = map fst $ lookupMorpho morpho word
        pos           = snd $ label $ fromJust $ parent tree
        sms           = intercalate bind cmps
        bind          = " &+ "
        cmps          = compoundUnknown lex word 
        lower (x:xs)  = toLower x:xs 
        lower []      = []
        setLabel' a   = setLabel (fst $ label tree,a)
        saveSms       = tellLemma $ map fst $ (concatMap (lookupMorpho morpho) cmps)
        saveName      = tellName (i,word) --TODO save whole name, not just first part
        moveOn  tr    = maybe (return tr) (processTree False lex morpho i)     (getNextWord tr)
        moveOn' tr    = maybe (return tr) (processTree False lex morpho (i+1)) (getNextWord tr)
        moveOnSave tr =  trace ("saving "++word++" as "++show (nub lemmas)) (tellLemma lemmas)
                      >> maybe (return tr) (processTree False lex morpho i) (getNextWord tr)


compoundUnknown :: Lex ->  String -> [String]
compoundUnknown lex w
         | not (null comps)                   = bestCmp comps
         | otherwise                          = [w]
  where comps  = compound lex w
        bestCmp = getFirstGood . sortBy (comparing length) -- färst delar!
        getFirstGood (x:xs) | length x > 1 = x               -- men minst en (är okänd)
                            | otherwise    = getFirstGood xs -- to avoid things only known to saldo, not to us
        getFirstGood []    = []


exchangeNames :: Int -> WorkingTree -> WorkingTree
exchangeNames i tree | lookNamish word  = moveOnTree (setLabel (id,name)) dropName tree 
                     | otherwise        = tree
   where word    = snd $ label tree
         id      = fst $ label tree
         name    = if List.last word == 's' then "XPN"++show i else "YPN"++show i

dropName  :: WorkingTree -> WorkingTree
dropName tree | lookNamish word || isNameSpec word = moveOnTree (setLabel (id,"")) dropName tree
              | otherwise                          = tree
   where word    = snd $ label tree
         id      = fst $ label tree

-- Modifies the tree with mod, then continues with f on the next word in the tree, if a such exist
moveOnTree :: forall t.  (t -> WorkingTree) -> (WorkingTree -> WorkingTree) 
           -> t -> WorkingTree
moveOnTree mod f tree | Just tree'<- getNextWord $ mod tree = f tree'
                      | otherwise                           = mod tree


lookNamish [] = error "emty name"
lookNamish s = notPn s && isUpper (head s)
isNameSpec = (`elem` ["von","af"])
notPn      = (`notElem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder","Ni"])


--verbtag,
--verbtag   = (=="V") . take 1 . drop 1
numbertag :: String -> Bool 
numbertag = (=="RO") . take 2

getFirstWord :: WorkingTree -> WorkingTree
getFirstWord tree | isLeaf tree = tree
                  | otherwise   = getFirstWord $ fromJust (firstChild tree)

getNextWord :: WorkingTree -> Maybe WorkingTree
getNextWord tree  | hasSiblingLeaf                           = next tree
                  | not (isLast tree)                        = Just $ getFirstWord sibling
                  | isContained tree                         = getNextWord parentNode
                  | otherwise                                = Nothing
    where hasSiblingLeaf = maybe False isLeaf (next tree)
          sibling        = fromJust $ next tree
          parentNode     = fromJust $ parent tree

tellLemma :: [Lemma] -> ProcessState ()
tellLemma l = tell (l,[])
tellName  :: (Int,String) -> ProcessState ()
tellName  n = tell ([],[n])

treed :: TreePos Full [Char]
treed = fromTree $  Node "S" [Node "NP" [Node "X" [Node "den" []
                    ,Node "lilla" []],Node "katten" []],Node "VP" [Node "åt" []]]


