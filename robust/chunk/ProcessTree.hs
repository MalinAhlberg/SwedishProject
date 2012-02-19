module ProcessTree where
import PGF
import Control.Monad.Writer
import Data.List as List
import Debug.Trace
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Tree
import Data.Tree.Zipper
import Control.Arrow
import Compound

-- compound: kattpälsen  -> [(katt &+ pälsen)]
-- ne      : Johan Ros   -> ["X1",""](

type ProcessState = Writer [Lemma]

--TODO if not unknown, save lemmas somewhere for extraction later
processTree :: Bool -> Lex -> Morpho -> Int -> TreePos Full String -> ProcessState (TreePos Full String)
processTree b lex morpho i tree  | isCompound =  moveOn  $ setLabel sms tree
--                                 | isUVerb    =  moveOn' $ setLabel vx tree
                                 | isNumber   =  moveOn' $ setLabel nx tree
                                 | isName     =  moveOn' $ exchangeNames i tree
                                 | b          =  moveOnSave  $ modifyLabel lower tree  --is first word
                                 | otherwise  =  moveOnSave  $ tree
  where isCompound    = trace ("work on word "++word) $ isUnknown && "&+" `isInfixOf` sms 
        isNumber      = isUnknown && numbertag pos
        isUVerb       = isUnknown && verbtag pos
        isName        = isUnknown 
        word          = (if b then lower else id) (label tree)
        vx            = ("VX"++show i)   --TODO maybe look at tag?
        nx            = ("1")  
        isUnknown     = null lemmas
        lemmas        = map fst $ lookupMorpho morpho word
        pos           = label $ fromJust $ parent tree
        sms           = compoundUnknown lex morpho word 
        lower (x:xs)  = toLower x:xs 
        lower []      = []
        moveOn  tr    = maybe (return tr) (processTree False lex morpho i)     (getNextWord tr)
        moveOn' tr    = maybe (return tr) (processTree False lex morpho (i+1)) (getNextWord tr)
        moveOnSave tr =  tell lemmas 
                      >> maybe (return tr) (processTree False lex morpho i) (getNextWord tr)


compoundUnknown lex morpho w
         | not (null comps)                   = intercalate bind (bestCmp comps)
         | otherwise                          = w
  where comps  = compound lex w
        bind   = " &+ "
        bestCmp = head . sortBy (comparing length) -- färst delar!

exchangeNames :: Int -> TreePos Full String -> TreePos Full String
exchangeNames i tree | lookNamish word  = dropName tree'
                     | otherwise        = tree
   where word    = label tree
         tree'   = modifyLabel (const name) tree
         name    = if List.last word == 's' then "XPN"++show i else "YPN"++show i

dropName  :: TreePos Full String -> TreePos Full String
dropName tree | lookNamish word || isNameSpec word = moveOn
              | otherwise                          = tree
   where word    = label tree
         tree'   = getNextWord $ modifyLabel (const "") tree -- TODO is this ok for the parser?
         moveOn  | isJust tree' = dropName $ fromJust tree'
                 | otherwise    = modifyLabel (const "") tree

lookNamish [] = error "emty name"
lookNamish s = notPn s && isUpper (head s)
isNameSpec = (`elem` ["von","af"])
notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder","Ni"])


verbtag,numbertag :: String -> Bool --TODO
verbtag   = (=="V") . take 1 . drop 1
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

treed = fromTree $  Node "S" [Node "NP" [Node "X" [Node "den" []
                    ,Node "lilla" []],Node "katten" []],Node "VP" [Node "åt" []]]


