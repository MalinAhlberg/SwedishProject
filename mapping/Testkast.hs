import PGF
import Data.Maybe

hej = do pgf <- readPGF "../gf/BigTest.pgf"
         putStrLn $ showType [] $ fromJust $ functionType pgf (fromJust $ readCId "it_Pron")



