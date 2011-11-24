import Translate hiding (main)
import System.Environment

main = do
  fls <- getArgs
  sequence [evaluations file ("EvalResultx"++show n) | (file,n) <- zip fls [0..]]

