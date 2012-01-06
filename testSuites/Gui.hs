import Graphics.UI.Gtk
import ParseLex

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  (maps,pgf) <- play'
  initGUI
  window <- windowNew
  bigbox  <- vBoxNew True 30 
  hbox    <- vBoxNew True 6
  ibox    <- vBoxNew True 20
  button1 <- buttonNewWithLabel "Parse"
  button2 <- buttonNewWithLabel "Delete"
  tree <- imageNewFromFile "gfETree.png"
--  boxPackStart button1 button1 PackRepel 0
--  boxPackStart button2 button2 PackRepel 0

  boxPackStart bigbox ibox PackNatural 0
  boxPackStart bigbox hbox PackNatural 0
  boxPackStart ibox tree PackNatural 0
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := bigbox, containerBorderWidth := 10]
  boxPackStart hbox button1 PackGrow 0
  boxPackStart hbox button2 PackGrow 0

  (label6,frame6) <- myLabelWithFrameNew
  boxPackStart hbox frame6 PackNatural 10
  labelSetText label6 ""
  frameSetLabel frame6 "Result:"
  labelSetPattern label6 [3, 1, 3]

  txtfield <- entryNew
  boxPackStart hbox txtfield PackNatural 5
  let parse = parseIt txtfield tree label6 maps pgf
  onEntryActivate txtfield parse 

  onClicked button1 parse
  onClicked button2 (entrySetText txtfield "")

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
  tree <- processparse txt pgf (read "BigTestSwe") maps
  i <- case tree of
        Nothing      -> return 0 
        Just (pth,i) -> imageSetFromFile im pth
                        >> return i
  labelSetText resfld $ "Number of parse trees"++show i


