module Main where
--import LastTranslate
import Translate hiding (main)

main = do res <- main' "../Talbanken05_20060604/FPS/P.tiger.xml"
          writeFile "AllTBNew" $ unlines $ getRes res 
