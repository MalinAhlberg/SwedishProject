module Main where

import Graphics.UI.Gtk
import ParseLex
import Control.Applicative
import Control.Monad

maxScrollerV scroller = do
  vadj <- get scroller scrolledWindowVAdjustment
  maxadj <- adjustmentGetLower vadj
  adjustmentSetValue vadj maxadj
  adjustmentValueChanged vadj
  adjustmentAdjChanged vadj
  putStrLn $ "Max scroller value is " ++ show maxadj

makeNewImageScroller :: FilePath -> IO ScrolledWindow
makeNewImageScroller file = do
  treeScroller <- scrolledWindowNew Nothing Nothing

  scrolledWindowSetPolicy treeScroller PolicyAutomatic PolicyAlways

  {-
  scrollerHAdj <- scrolledWindowGetHAdjustment treeScroller
  scrollerVAdj <- scrolledWindowGetVAdjustment treeScroller


-}
  treeImage <- imageNewFromFile file

  scrolledWindowAddWithViewport treeScroller treeImage
{-
  treeViewport <- viewportNew scrollerHAdj scrollerVAdj

  containerAdd treeViewport treeImage
  containerAdd treeScroller treeViewport
-}
  return treeScroller

main :: IO ()
main = do
  (maps,pgf) <- play'
  initGUI
  window <- windowNew
  bigbox  <- vBoxNew True 0

  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := bigbox, containerBorderWidth := 5]

  (resLabel,resFrame) <- myLabelWithFrameNew
  labelSetText resLabel ""
  frameSetLabel resFrame "Result"

  inputField <- entryNew
  entrySetText inputField "jag åt katt"
  parseButton <- buttonNewWithLabel "  Parse  "

  entryBox <- hBoxNew True 0
  boxSetHomogeneous entryBox False

  boxPackStart entryBox inputField  PackGrow    1
  boxPackStart entryBox parseButton PackNatural 1

  inputFrame <- frameNew
  containerAdd inputFrame entryBox
  frameSetShadowType inputFrame ShadowOut
  frameSetLabel inputFrame "Input"

  boxSetHomogeneous bigbox False


  notebook <- notebookNew
  notebookSetShowTabs notebook True
  notebookSetTabPos notebook PosTop

  boxPackStart bigbox notebook   PackGrow 0
  boxPackStart bigbox inputFrame PackNatural 2
  boxPackStart bigbox resFrame   PackNatural 2

  let parse = do
       txt <- entryGetText inputField
       mfile <- parseIt txt resLabel maps pgf
       case mfile of
           Just file -> do
                putStrLn $ "File is " ++ file
                scroller <- makeNewImageScroller file
                widgetShowAll scroller
                widgetQueueDraw scroller
                maxScrollerV scroller
                r <- notebookAppendPage notebook scroller txt
                putStrLn $ "Appended to notebook: " ++ show r
                notebookSetTabReorderable notebook scroller True
                widgetShowAll notebook
                notebookSetCurrentPage notebook r
                return ()
           Nothing -> return ()

  on notebook pageAdded $ \w i -> putStrLn $ "Page added: " ++ show i

  on inputField entryActivate parse
  onClicked parseButton parse


  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

myLabelWithFrameNew :: IO (Label,Frame)
myLabelWithFrameNew = do
  label <- labelNew Nothing
  frame <- frameNew
  containerAdd frame label
  frameSetShadowType frame ShadowOut
  return (label, frame)

parseIt txt resfld maps pgf = do
  putStrLn $ "You typed: " ++ txt
  treeImage <- processparse txt pgf (read "DictSwe") maps
  let (n,img) = case treeImage of
        Nothing      -> (0,Nothing)
        Just (pth,i) -> (i,Just pth)
  labelSetText resfld $ "Number of parse trees: " ++ show n
  return img


