module GraphTree where
import Data.Char
import Data.Tree
    
-- Tree helper function: generate a graphviz dot ﬁle depicting it
dotTree :: Tree String -> String -> String
dotTree t [] = dotTree t "T"
dotTree t name = header ++ body ++ footer where
   header = "graph \"" ++ "\" {\n"
   body = dotTreeBody t 0
   footer = "\n}"
   -- Tree helper function: generate the body of the digraph
   dotTreeBody :: Tree String -> Int ->  String
   dotTreeBody (Node ns []) i =
        name ns i ++ " [label = \""++ns++"\", style = \"solid\", shape = \"plaintext\"] \n" 
   dotTreeBody (Node n xs) i =
      mynode ++ links ++ bodies where
        mynode = name n i ++ "[label = \""++n++"\", style = \"solid\", shape = \"plaintext\"];\n"
        links = concat [name n i ++ " -- " ++ name n' (i+1) ++ "[ style = solid];\n" | (Node n' _) <- xs]
        bodies = concat [dotTreeBody (Node n ts) (i+1) | (Node n ts) <- xs ]
   name n i = map correct n ++ show i
   correct '?' = 'Q'
   correct ' ' = '_'
   correct x   = x
