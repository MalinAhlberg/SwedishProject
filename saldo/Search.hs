import SaldoXML
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.Function
import Data.List

findTags all = M.foldlWithKey f M.empty all
 where f mapp tag entry = M.insertWith (flip const) tag entry mapp

findTagsFast :: M.Map String a -> [(String,a)]
findTagsFast = nubBy ((==) `on` fst) . M.toList      

main = do 
  args <- getArgs 
  case args of 
       (file:_) -> mainF file (writeFile "tags.txt" . show . findTagsFast) --(M.keys . findTags)
       _        -> error "wrong inargs"


 
