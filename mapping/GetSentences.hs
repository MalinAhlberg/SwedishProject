import XMLHelp
import System.Environment

main = do 
  files <- getArgs
  ss <- mapM (flip mainF (return . map getSentence)) files
  writeFile "alla" $ unlines $ map (\(i,s) -> show i++"\t"++s) $ concat $ concat ss
