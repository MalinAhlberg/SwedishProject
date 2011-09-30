module GraphTree where
import Data.Char
import Data.Tree
    
-- Tree helper function: generate a graphviz dot ﬁle depicting it
dotTree :: Tree String -> String -> String
dotTree t [] = dotTree t "T"
dotTree t name = header ++ body ++ footer where
   header = "digraph \"" ++ name ++ "\" {\n"
   body = dotTreeBody t 0
   footer = "\n}"
   -- Tree helper function: generate the body of the digraph
   dotTreeBody :: Tree String -> Int ->  String
   dotTreeBody (Node (n:ns) []) i =
        (toUpper n : ns) ++show i++ " []" --label=\"" ++ "\"]\n"
   dotTreeBody (Node name xs) i =
      mynode ++ links ++ bodies where
        mynode = name++show i ++ " [style=filled,color=lightgrey];\n"
        links = concat [name++show i ++ " -> " ++ n++show (i+1) ++ ";\n" | (Node n _) <- xs]
        bodies = concat [dotTreeBody (Node n ts) (i+1) | (Node n ts) <- xs ]
