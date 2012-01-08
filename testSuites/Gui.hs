import Graphics.UI.Gtk
import ParseLex

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  (maps,pgf) <- play'
  initGUI
  window <- windowNew
  bigbox  <- vBoxNew True 0

  treeScroller <- scrolledWindowNew Nothing Nothing

  scrollerHAdj <- scrolledWindowGetHAdjustment treeScroller
  scrollerVAdj <- scrolledWindowGetVAdjustment treeScroller

  treeImage <- imageNewFromFile "gfETreeImage.png"
  treeViewport <- viewportNew scrollerHAdj scrollerVAdj

  containerAdd treeViewport treeImage
  containerAdd treeScroller treeViewport

  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := bigbox, containerBorderWidth := 5]

  (resLabel,resFrame) <- myLabelWithFrameNew
  labelSetText resLabel ""
  frameSetLabel resFrame "Result"
--  labelSetPattern resLabel [3, 1, 3]

  inputField <- entryNew
  parseButton <- buttonNewWithLabel "  Parse  "

  entryBox <- hBoxNew True 0
  boxSetHomogeneous entryBox False

  boxPackStart entryBox inputField PackGrow 1
  boxPackStart entryBox parseButton PackNatural 1

  inputFrame <- frameNew
  containerAdd inputFrame entryBox
  frameSetShadowType inputFrame ShadowOut
  frameSetLabel inputFrame "Input"

  boxSetHomogeneous bigbox False

  boxPackStart bigbox treeScroller PackGrow 2
  boxPackStart bigbox inputFrame PackNatural 2
  boxPackStart bigbox resFrame PackNatural 2

  let parse = parseIt inputField treeImage resLabel maps pgf
  onEntryActivate inputField parse
  onClicked parseButton parse
--  onClicked button2 (entrySetText inputField "")

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

parseIt fld im resfld maps pgf = do
  txt <- entryGetText fld
  putStrLn $ "You typed: "++txt
  treeImage <- processparse txt pgf (read "DictSwe") maps
  i <- case treeImage of
        Nothing      -> return 0
        Just (pth,i) -> imageSetFromFile im pth
                        >> return i
  labelSetText resfld $ "Number of parse trees: "++show i


