import Parsing
import System.Environment (getArgs)

main = do
  (file:arg) <- getArgs
  run file True "Res.txt"
  putStrLn "See result in Res.txt!"


