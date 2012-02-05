{-# LANGUAGE TemplateHaskell #-}
module State where
import PGF
import Data.Label 
import Data.Label.PureM 

data SentenceType = Q | Dir | Top 
  deriving (Show,Eq)
data NPType = Generic | Impers | Normal

data VPForm  = Cop | Sup | VV | VA 
             | V | V2 | V2A | V2Pass 
             | Fut | FutKommer
             | VS         
                                            
  deriving (Eq,Show)


data State = State { _isReflGenVP  :: Bool
           , _isExist      :: Bool
           , _iquant       :: Bool
           , _passive      :: Bool
           , _sentenceType :: SentenceType
           , _complement   :: (VPForm,[Maybe Expr],[Bool])
           --, _object       :: Maybe Expr
           , _tmp          :: Maybe Expr
           , _pol          :: Maybe Bool
           , _subj         :: Maybe Expr
           , _nptype       :: NPType
           }

$(mkLabels [''State])

startState :: State
startState = State {_isReflGenVP = False, _isExist = False
               ,_passive = False
               ,_iquant = False, _complement = (V,[],[])
               ,_sentenceType = Dir --, _object = Nothing
               ,_tmp = Nothing, _pol = Nothing, _subj = Nothing
               ,_nptype = Normal}



