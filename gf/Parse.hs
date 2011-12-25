import PGF
import Data.Maybe

test :: String -> IO ()
test str = do
  pgf <- readPGF "BigTest.pgf"
  let typ = fromJust $ readType "S"
      tr = parse pgf (read "BigTestSwe") typ str 
  print tr
  return ()

