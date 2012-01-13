{-# LANGUAGE OverloadedStrings #-}
module MainE where
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.IO as S
import Text.XHtml
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Data.Maybe
import Control.Monad.IO.Class

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    gr <- return undefined --play
    run port (parseF gr)

--parseF :: MonadIO m => ParseData -> Request -> m Response
parseF gr req = do
  liftIO $ putStrLn "start"
  let mn = queryString req
  x <- liftIO $ findText gr req mn 
  liftIO $ putStrLn "have returned"
  return x

--findText :: ParseData -> Request -> Query -> IO Response
findText gr req mn 
  | (Just (Just im)) <- lookup "img" mn = 
      do
       putStrLn $ "want image"++BC.unpack im
       putStrLn $ "all:"++show mn
       return $
         ResponseFile status200 [("Content-Type", "image/png")] 
               ("images/"++BC.unpack im) Nothing
  | (Just (Just txt)) <- lookup "text" mn = do
      putStrLn "have text, will show it"
      parseIt gr txt
  | (Just (Just txt)) <- lookup "more" mn = do
      putStrLn "want more trees!!"
      parseMore txt
  | otherwise = do
      putStrLn "have nothing, will return textfield"
      return inputForm

inputForm :: Response
inputForm = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
              [BU.fromString $ show textInputField
              ,"<p>Note that very few words are annoted with valency!\n"
              ,BU.fromString verbInfo, "!</p>"]
              
textInputField :: Html
textInputField = form << 
      [paragraph << (textarea noHtml ! [strAttr "name" "text"])
      ,submit "" "Submit"]

parseIt :: (a,b) -> B.ByteString -> IO Response 
parseIt (maps,pgf) txt = do 
  (i,res) <- return (4,(Right $ ("tmptreep.png","tmptreea.png") :: Either [String] (S.FilePath,S.FilePath)))
  -- orginial code:
  --processparse (BC.unpack txt) pgf maps
  -- Try this line instead to see what happens when parsing fails
  --(i,res) <- return (0,Left ["svåra","ord"]) 
  case res of
    (Right (pt,at)) -> return $
        ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString $
           [ "<p>Parsed: \"",txt,results i, "!</p>"
           , "<p><a href='/?img=",BU.fromString pt,"'>Parse tree</a></p>\n"
           , "<p><a href='/?img=",BU.fromString at,"'>Abstract tree</a></p>\n"]++moreTrees i
    Left xs            -> return $ 
        ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
          [ "<p>Parsed: \"",txt,results 0, "!</p>"
          , "<p>Unknown words: ",BU.fromString $ show xs,"!</p>\n"
          , "<p><a href='/'>Back</a></p>\n"]
 where results :: Int -> B.ByteString
       results i = BU.fromString ("\" which resulted in "++show i++" trees")
       moreTrees :: Int -> [B.ByteString]
       moreTrees i | i>2 = ["<p><a href='/?more=",txt,"'>More trees</a></p>\n"]
                   | i<3 = []

parseMore :: B.ByteString -> IO Response
parseMore txt = do
  links <- return [("tmptreep.png","tmptreea.png"),("tmptreea8.png","tmptreep8.png")] --reparse (BC.unpack txt)
  putStrLn $ "have reparsed, got "++show (length links)++" trees"
  let html = "<p>All trees</p>\n" : concatMap (\(i,(pt,at)) -> 
              ["<p>",BU.fromString (show i),"<a href='/?img=",BU.fromString pt,"'>Parse tree</a>\n"
              , "<a href='/?img=",BU.fromString at,"'>Abstract tree</a></p>\n"])
                   (zip [0..] links)
  return $
    ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat 
         $ map copyByteString html


   
verbInfo :: String
verbInfo = concatMap (\(a,b) -> "Verbs of type "++a++":\n"++unlines b++"\n")
             [("V3",v3),("V2",v2),("VV",vv) ,("VA",va),("V2V",v2v),("V2A",v2a)]

v2 = ["tycka om", "raka", "fånga", "titta på"," se","ta","slå","akta sig för","äta","dricka"
      ,"finnas", "saknas", "fattas", "glömma", "läsa", "skriva", "bli", "ha"]
v3 = ["ta med sig till", "ge till", "ge", "erbjuda"]
vv = ["börja", "måste", "vill", "kan"]
va = ["bli"]
v2v = ["be"]
v2a = ["måla"]
