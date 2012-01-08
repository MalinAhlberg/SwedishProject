module Main where

import Graphics.UI.Gtk
import ParseLex
import Control.Applicative
import Control.Monad
import Control.Concurrent

maxScrollerV scroller = do
  vadj <- get scroller scrolledWindowVAdjustment
  maxadj <- get vadj adjustmentUpper
  set vadj [ adjustmentValue := maxadj ]
  adjustmentValueChanged vadj
  scrolledWindowSetVAdjustment scroller vadj
  putStrLn $ "Max scroller value is " ++ show maxadj

makeNewImageScroller :: FilePath -> IO ScrolledWindow
makeNewImageScroller file = do
  treeScroller <- scrolledWindowNew Nothing Nothing

  scrolledWindowSetPolicy treeScroller PolicyAutomatic PolicyAlways

  treeImage <- imageNewFromFile file

  scrolledWindowAddWithViewport treeScroller treeImage

  maxScrollerV treeScroller

  return treeScroller

main :: IO ()
main = do
  (maps,pgf) <- play'
  initGUI
  window <- windowNew
  widgetModifyBg window StateNormal (Color 25000 25000 45555)
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
  notebookSetScrollable notebook True
  notebookSetPopup notebook True

  boxPackStart bigbox notebook   PackGrow 0
  boxPackStart bigbox inputFrame PackNatural 2
  boxPackStart bigbox resFrame   PackNatural 2

  let parse = do
       txt <- entryGetText inputField
       mfile <- parseIt txt resLabel maps pgf
       new <- case mfile of
           Just (fileP,fileA) -> do
                putStrLn $ "File is " ++ fileP ++ " and " ++ fileA
                scrollerP <- makeNewImageScroller fileP
                scrollerA <- makeNewImageScroller fileA
                rP <- notebookAppendPage notebook scrollerP (txt ++ " (parse)")
                rA <- notebookAppendPage notebook scrollerA (txt ++ " (abs)")
                putStrLn $ "Appended to notebook: " ++ show rP ++ " and " ++ show rA
                notebookSetTabReorderable notebook scrollerP True
                notebookSetTabReorderable notebook scrollerA True
                notebookSetCurrentPage notebook rP
                widgetShowAll notebook
                return [scrollerP,scrollerA]
           Nothing -> return []
       mapM_ maxScrollerV new

  on notebook pageAdded $ \w i -> do putStrLn $ "Page added: " ++ show i
--                                     maxScrollerV (castToScrolledWindow w)

--  on notebook switchPage $ \i -> do Just w <- notebookGetNthPage notebook i
--                                    maxScrollerV (castToScrolledWindow w)

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
  treeImage <- processparse txt pgf (read "DictbSwe") maps
  let (n,imgs) = case treeImage of
        Nothing      -> (0,Nothing)
        Just (pth,i) -> (i,Just pth)
  labelSetText resfld $ "Number of parse trees: " ++ show n
  return imgs


