{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.IO
import System.FilePath
import System.TimeIt
import Text.XHtml
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Data.Maybe
import Data.List
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
  id <- return "theMap" --get userid somehow! mkDir theMap, images/theMap
  liftIO $ print ("pathinfo: "++show (pathInfo req))
  liftIO $ print ("all requst: "++show req)
  x <- liftIO $ findText id gr req mn b 
  liftIO $ putStrLn "have returned"
  return x


findText :: UserId -> ParseData -> Request -> Query -> Bool -> IO Response
findText id gr req mn b
  | (Just (Just im)) <- lookup "img" mn = do
     putStrLn $ "want image "++BC.unpack im
     putStrLn $ "all:"++show mn
     return $
       ResponseFile status200 [("Content-Type", "image/png")] 
             ("images/"++BC.unpack im) Nothing
  | (Just (Just txt)) <- lookup "text" mn = do
      putStrLn "have text, will show it"
      parseIt id gr txt b
  | (Just (Just txt)) <- lookup "more" mn = do
      putStrLn "want more trees!!"
      parseMore id txt
  | (Just _) <- lookup "firstform" mn = do
      putStrLn "have nothing, will return textfield"
      return inputForm
findText id gr req mn b = do
  let path = map (filter (/='\"')) $ map show $ pathInfo req
  liftIO $ print (pathInfo req)
  liftIO $ print path
  case path of
     ("static":x)   -> do
         let typ = takeExtension $ last path
         return $
           ResponseFile status200 [("Content-Type", getTyp typ)] 
               (intercalate "/" path) Nothing
     _              -> do
         return $
           ResponseFile status200 [("Content-Type", "text/html")] 
               ("static/index.html") Nothing
 where getTyp ".js"  = "application/javascript"
       getTyp ".css" = "text/css"
       getTyp ".html" = "text/html"
     

inputForm :: Response
inputForm = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
              [BU.fromString $ show textInputField
              ,"<p>Note that very few words are annoted with valency!\n"
              ,BU.fromString verbInfo, "!</p>"]
              
textInputField :: Html
textInputField = form << 
      [paragraph << (textarea noHtml ! [strAttr "name" "text"])
      ,submit "" "Submit"]

parseIt :: UserId -> ParseData -> B.ByteString -> Bool -> IO Response 
parseIt id (maps,pgf) txt b = do 
  (t,results) <- timeItT $ if b then processparse id (BC.unpack txt) pgf maps
                           else do print ("smallparse "++ BC.unpack txt) >> smallparse id (BC.unpack txt)
  putStrLn $ "It took "++show t++" seconds "
  let html = map getHtml results
  return $
        ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString $
        concat html
 where getHtml (s,n,res) = case res of
              Right (pt,at) ->
                     [ "<p>Parsed: ",pretty s,results n, "!</p>"
                     , "<p><a href='/?img=",BU.fromString pt,"'>Parse tree</a></p>\n"
                     , "<p><a href='/?img=",BU.fromString at,"'>Abstract tree</a></p>\n"]++moreTrees n (snd s)
              Left xs        -> 
                    [ "<p>Parsed: ",pretty s,results 0, "!</p>"
                    , "<p>Unknown words: ",BU.fromString $ show xs,"!</p>\n"
                    , "<p><a href='/'>Back</a></p>\n"]
       results :: Int -> B.ByteString
       results i = BU.fromString (" which resulted in "++show i++" trees")
       pretty :: (String,String) -> B.ByteString
       pretty (i,s) = BU.fromString ("Sentence "++i++" \""++s++"\"")
       moreTrees :: Int -> String -> [B.ByteString]
       moreTrees i s | i>1 = ["<p><a href='/?more=",BU.fromString s,"'>More trees</a></p>\n"]
                     | i<2 = []

parseMore :: UserId -> B.ByteString -> IO Response
parseMore id txt = do
  links <- reparse id (BC.unpack txt)
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
