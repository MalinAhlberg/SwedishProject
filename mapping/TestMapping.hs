import Translate hiding (main)
import System.Environment

main1 = do
  fls <- getArgs
  sequence [evaluations file ("EvalResult"++show n) | (file,n) <- zip fls [0..]]

main = main' tb 
