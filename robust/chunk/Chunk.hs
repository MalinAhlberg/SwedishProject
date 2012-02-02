

chunkparse tree = do
  -- return word numbers as well as new words?
  -- or lists of lists (each original word = one list)
  -- in that case, do not need to redo so many times
  -- all other times, just look at the numbers
  -- either connect all new words at tree, or simply
  -- just look at numbers.
  s <- compound+names $ getSentence tree
  c <- topCat tree
  --guard c ==same as now then skiplevel?
  res <- parse s c
  if null res then combine tree 
              else xs


combine tree = do
  c <- topCat tree
  ts <- getNextLevels tree
  childs <- mapM chunkparse ts
  applyRule c childs


topCat :: Tree String -> Type
topCat [Node pos [Node ws []] = 
topCat [Node cat ts]  = toGF cat
