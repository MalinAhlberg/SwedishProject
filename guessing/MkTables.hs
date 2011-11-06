module MkTables 
 (getTables, showTable, showSmallTable, getSmallForms, inf, Table) where
import PGF
import Data.List
import Data.Maybe
import System.Environment (getArgs)

type Table = (String,[[(Form,String)]])
type Form  = String

namelist :: [String]
namelist = ["giva","anfalla","angiva_V"]
main = do
  args <- getArgs 
  pgf <- readPGF pgfFile  
  let l = (read lang) :: CId
  res <- mapM (getTables pgf l) namelist
--  resFile <- mkResFile "Resultat" args
  putStrLn $ unlines $ map showTable res
--  writeFile resFile (unlines $ map showTable res)
  
getTables :: PGF -> CId -> String -> IO Table
getTables pgf langId s = do
  let mexpr = readExpr s    
  case mexpr of
    Just expr -> return $ (s,tabularLinearizes pgf langId expr)
    Nothing   -> putStrLn ("Failed: "++s) >> return (s,[])

pgfFile = "IrregSwe.pgf"
lang    = "IrregSwe"
showTable :: Table -> String
showTable t = showTableF t (const True)

showSmallTable :: Table -> String
showSmallTable t = showTableF t ((`elem` tableForms) . fst)

showTableF :: Table -> ((Form,String) -> Bool) ->  String
showTableF (s,t) shown = s++":\n"++table t
  where table = unlines . map (unlines . map showTuple . filter shown) 

showTuple :: (Form,String) -> String
showTuple (typ,string) = string ++"\t\t"++typ

             --(String,[[(String,String)]])
getSmallForms :: Table -> [String]
getSmallForms = map  snd . sortBy formOrd . filter ((`elem` tableForms3) . fst) . head . snd

getOneForm :: Form -> Table -> String
getOneForm f (s,t) = fromJust $ lookup f $ concat t

tableForms,tableForms3 :: [Form]
tableForms3 = [pret,supin,inf]
tableForms = [pres,pret,supin,inf]

pres,pret,supin,inf :: Form
pres = "s (VF (VPres Act))"
pret = "s (VF (VPret Act))"
supin = "s (VI (VSupin Act))"
inf  = "s (VI (VInfin Act))"

formOrd :: (Form,String) -> (Form,String) -> Ordering
formOrd (p,_) (s,_) = compare (ord p) (ord s)

ord "s (VF (VPres Act))" = 2
ord "s (VF (VPret Act))" = 3
ord "s (VI (VSupin Act))" = 4
ord "s (VI (VInfin Act))" = 1
ord _                     = 5
-- tabularLinearizes :: PGF -> CId -> Expr -> [[(String,String)]]


