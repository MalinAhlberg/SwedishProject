{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.IO
import Text.XHtml
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Data.Maybe
import Control.Monad.IO.Class

import ParseLex
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    gr <- play
    run port (parseF gr True)

mainSmall = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    gr <- return (undefined,undefined)
    run port (parseF gr False)


parseF :: MonadIO m => ParseData -> Bool -> Request -> m Response
parseF gr b req = do
  liftIO $ putStrLn "start"
  let mn = queryString req
  x <- liftIO $ findText gr req mn b 
  liftIO $ putStrLn "have returned"
  return x

findText :: ParseData -> Request -> Query -> Bool -> IO Response
findText gr req mn b
  | (Just (Just im)) <- lookup "img" mn = 
      do
       putStrLn $ "want image"++BC.unpack im
       putStrLn $ "all:"++show mn
       return $
         ResponseFile status200 [("Content-Type", "image/png")] 
               ("images/"++BC.unpack im) Nothing
  | (Just (Just txt)) <- lookup "text" mn = do
      putStrLn "have text, will show it"
      parseIt gr txt b
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

parseIt :: ParseData -> B.ByteString -> Bool -> IO Response 
parseIt (maps,pgf) txt b = do 
  (i,res) <- if b then processparse (BC.unpack txt) pgf maps
                  else smallparse (BC.unpack txt)
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
  links <- reparse (BC.unpack txt)
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
