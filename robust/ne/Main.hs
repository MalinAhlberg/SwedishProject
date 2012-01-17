import XMLHelp hiding (words)
import Ne

import Control.Arrow

maini :: FilePath -> IO ()
maini out = do
  sent <- mainF tb (return . map getSentence')
  let output = map (second isName) $ concat sent
  writeFile out $ unlines $ map show output

tb = "../../Talbanken05_20060604/FPS/P.tiger.xml"
