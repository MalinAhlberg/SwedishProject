module ProcessTree where
import PGF
import Data.List as List
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Tree
import Data.Tree.Zipper
--import ParseSaldo
import Control.Arrow
--import Compound
--import Ne

-- compound: kattpälsen  -> [("",katt_nn),("pälsen",päls_nn)]
-- ne      : Johan Ros   -> [("X1",Johan),("",Ros)](

processTree lex morpho i tree  | isCompound = moveOn  $ modifyLabel setCmp tree
                               | isUVerb    = moveOn' $ modifyLabel setX tree
                               | isName     = moveOn' $ exchangeNames i tree
                               | otherwise  = moveOn  $ tree
  where isCompound = isUnknown && "&+" `isInfixOf` sms 
        isName  = isUnknown 
        isUVerb = isUnknown && verbtag pos
        word    = label tree
        moveOn  tr = maybe tr (processTree lex morpho i)     (getNextWord tr)
        moveOn' tr = maybe tr (processTree lex morpho (i+1)) (getNextWord tr)
        setCmp  = const sms
        setX    = const ("VX"++show i)   --TODO add in dict, maybe look at tag?
        isUnknown = null (lookupMorpho morpho word) --TODO toLower on first word!
        pos     = label $ fromJust $ parent tree
        sms     = compoundUnknown lex morpho word 


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
         name    = if List.last word == 's' then "X0"++show i else "Y0"++show i

dropName  :: TreePos Full String -> TreePos Full String
dropName tree | lookNamish word || isNameSpec word = dropName tree'
              | otherwise                          = tree
   where word    = label tree
         tree'   = modifyLabel (const "") tree -- TODO is this ok for the parser?

lookNamish s = notPn s && isUpper (head s)
isNameSpec = (`elem` ["von","af"])
notPn = not . (`elem` ["Dig","Din","Dina","Du","Er","Era","Er","Eder","Ni"])


verbtag :: String -> Bool --TODO
verbtag = undefined
compound = undefined --TODO import from lib

getFirstWord tree | isLeaf tree = tree
                  | otherwise   = getFirstWord $ fromJust (firstChild tree)

getNextWord tree  | hasSiblingLeaf                           = next tree
                  | not (isLast tree)                        = getNextWord sibling
                  | isContained tree                         = getNextWord parentNode
                  | otherwise                                = Nothing
    where hasSiblingLeaf = maybe False isLeaf (next tree)
          sibling        = fromJust $ next tree
          parentNode     = fromJust $ parent tree

treed = fromTree $  Node "S" [Node "NP" [Node "X" [Node "den" []
                    ,Node "lilla" []],Node "katten" []],Node "VP" [Node "åt" []]]


