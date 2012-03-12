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

processTree :: Bool -> Lex -> Morpho -> Int -> TreePos Full String -> ProcessState (TreePos Full String)
processTree b lex morpho i tree  | isCompound =  saveSms >> moveOn (setLabel sms tree)
                                 | isFut      = moveOn (setLabel "ska" tree) -- hackerish way to handle "skall"
--                                 | isUVerb    =  moveOn' $ setLabel vx tree
                                 | isNumber   =  moveOn' $ setLabel nx tree
                                 | isName     =  saveName >> (moveOn' $ exchangeNames i tree)
                                 | b          =  trace "put to lower" $ moveOnSave $ setLabel word tree  --is first word, put to lower
                                 | otherwise  =  moveOnSave $ tree

  where isCompound    = trace ("work on word "++word) $ isUnknown && length cmps > 1
        isNumber      = isUnknown && numbertag pos
        --isUVerb       = isUnknown && verbtag pos
        isName        = isUnknown 
        isFut         = pos =="SVPS"
        word          = (if b then lower else id) (label tree)
        --vx            = ("VX"++show i)   --TODO maybe look at tag?
        nx            = ("1")  
        isUnknown     = null lemmas
        lemmas        = map fst $ lookupMorpho morpho word
        pos           = label $ fromJust $ parent tree
        sms           = intercalate bind cmps
        bind          = " &+ "
        cmps          = compoundUnknown lex word 
        lower (x:xs)  = toLower x:xs 
        lower []      = []
        saveSms       = tellLemma $ map fst $ (concatMap (lookupMorpho morpho) cmps)
        saveName      = tellName (i,word)
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


exchangeNames :: Int -> TreePos Full String -> TreePos Full String
exchangeNames i tree | lookNamish word  = moveOnTree (setLabel name) dropName tree 
                     | otherwise        = tree
   where word    = label tree
         name    = if List.last word == 's' then "XPN"++show i else "YPN"++show i

dropName  :: TreePos Full String -> TreePos Full String
dropName tree | lookNamish word || isNameSpec word = moveOnTree (setLabel "") dropName tree
              | otherwise                          = tree
   where word    = label tree

-- Modifies the tree with mod, then continues with f on the next word in the tree, if a such exist
moveOnTree :: forall t.  (t -> TreePos Full String) -> (TreePos Full String -> TreePos Full String) 
           -> t -> TreePos Full String
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

getFirstWord :: TreePos Full  String -> TreePos Full String
getFirstWord tree | isLeaf tree = tree
                  | otherwise   = getFirstWord $ fromJust (firstChild tree)

getNextWord :: TreePos Full  String -> Maybe (TreePos Full String)
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


