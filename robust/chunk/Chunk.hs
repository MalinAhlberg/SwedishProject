import Data.Tree as T

chunkparse tree = do
  -- (return word numbers as well as new words?
  -- or lists of lists (each original word = one list)
  -- in that case, do not need to redo so many times
  -- all other times, just look at the numbers
  -- either connect all new words at tree, or simply
  -- just look at numbers.)
  -- new idea: make usual tree with strings, Tree String,
  -- strings are confade by compound+name, and the translation
  -- save in file (eller så), so Malin Ahlberg -> X0
  --                             gatusten      -> 1 &+ sten 
  --                          or gatusten      -> gata &+ sand
  -- TODO: use cat as much as possible!!! 
  s <- compound+names $ getSentence tree
  c <- topCat tree
  --guard c ==same as now then skiplevel?
  res <- parse s c
  if null res then combine tree 
              else xs


combine :: T.Tree String -> Tree
combine tree = do
  let c = topCat tree
  ts <- getNextLevels tree
  case ts of
       [t] -> if topCat tree == c
                 then combine t -- go one level deeper to get new info
                 else do                
                      childs <- mapM chunkparse ts
                      applyRule c childs


topCat :: T.Tree String -> Type
topCat [Node pos [Node w []] = translatePos pos
topCat (Node w [])           = parseAsWord w
topCat [Node cat ts]         = toGF cat
