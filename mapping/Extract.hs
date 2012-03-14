module Main where
--import LastTranslate
import Translate hiding (main)

main = do res <- main' tb --"TBSimplified.xml"
          writeFile "AllTBNewBig" $ unlines $ getRes res 
