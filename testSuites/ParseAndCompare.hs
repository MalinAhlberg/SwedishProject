import Parser

main = do
  (file:arg) <- getArgs
  run file False "tmp.tmp"
  correct <- readFile "treebank.txt"
  have    <- readFile "tmp.tmp"
  compare correct have

compare = zipWith rate `on` parseResult
  where rate (i,n) (j,m) = undefined


