import Graphics.UI.Gtk
import Parsing

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO ()
main = do
  (maps,pgf) <- play'
  initGUI
  window <- windowNew
  button <- buttonNew
  hbox    <- hBoxNew True 10
  button1 <- buttonNewWithLabel "Parse"
  button2 <- buttonNewWithLabel "Delete"
  tree <- imageNewFromFile "gfETree.png"
  boxPackStart hbox tree PackNatural 0
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerChild := hbox, containerBorderWidth := 10]
  boxPackStart hbox button1 PackGrow 0
  boxPackStart hbox button2 PackGrow 0
  txtfield <- entryNew
  boxPackStart hbox txtfield PackNatural 5
  let parse = parseIt txtfield tree maps pgf
  onEntryActivate txtfield parse 
  onClicked button1 parse
  onClicked button2 (entrySetText txtfield "")
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI


--parseIt :: Entry -> Image -> IO ()
parseIt fld im maps pgf = do
  txt <- entryGetText fld
  putStrLn $ "You typed: "++txt
  tree <- processparse txt pgf (read "BigTestSwe") maps
  case tree of
       Nothing  -> return ()
       Just pth -> imageSetFromFile im pth
