module GraphTree where
import Data.Tree
    
-- Tree helper function: generate a graphviz dot ﬁle depicting it
dotTree :: Tree String -> String -> String
dotTree t [] = dotTree t "T"
dotTree t name = header ++ body ++ footer where
   header = "digraph \"" ++ name ++ "\" {\n"
   body = dotTreeBody t 
   footer = "\n}"
   -- Tree helper function: generate the body of the digraph
   dotTreeBody :: Tree String -> String
   dotTreeBody (Node n [Node x []]) =
        n ++ " [label=\"" ++ show x ++ "\"]\n"
   dotTreeBody (Node name xs) =
      mynode ++ links ++ bodies where
        mynode = name ++ " [style=filled,color=lightgrey];\n"
        links = concat [name ++ " -> " ++ n ++ ";\n" | (Node n _) <- xs]
        bodies = concat [dotTreeBody (Node n ts) | (Node n ts) <- xs ]
