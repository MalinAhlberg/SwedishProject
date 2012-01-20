{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Network.HTTP.Headers
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
--import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.IO
import System.FilePath
import System.TimeIt
import System.Process
import Text.XHtml
import Data.Monoid
import Data.Maybe
import Data.List
import Data.List.Utils
--import Control.Monad.IO.Class
import Control.Monad.State
import Control.Concurrent.MVar

import ParseLex
 
--type CState = StateT Cookies IO
--data Cookies = C {count ::  Int }
--
--main = do
--    let port = 3000
--    putStrLn $ "Listening on port " ++ show port
--    gr <- play
--    run port (parseF gr True newC)

main = do
    let port = 3001
    putStrLn $ "Listening on port " ++ show port
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    mvar <- newMVar 0
    gr <- return (undefined,undefined)
    run port (parseF gr False mvar)

parseF :: MonadIO m => ParseData -> Bool -> MVar Int -> Request -> m Response
parseF gr b i req = do
  liftIO $ putStrLn "start"
  let mn = queryString req
  liftIO $ print ("pathinfo: "++show (pathInfo req))
  liftIO $ print ("all requst: "++show req)
  x <- liftIO $ findText gr req mn b i
  liftIO $ putStrLn "have returned"
  return x


findText :: ParseData -> Request -> Query -> Bool -> MVar Int -> IO Response
findText gr req mn b i
  | (Just (Just im)) <- lookup "img" mn = do
     putStrLn $ "want image "++BU.toString im
     putStrLn $ "all:"++show mn
     return $
       ResponseFile status200 [("Content-Type", "image/svg+xml")
                              ,("Cache-Control","no-cache")] 
             ("images/"++BU.toString im) Nothing
  | (Just (Just txt)) <- lookup "text" mn = do
      putStrLn "have text, will show it"
      parseIt (getCookie req) gr txt b
  | (Just (Just txt)) <- lookup "more" mn = do
      putStrLn "want more trees!!"
      parseMore (getCookie req) txt
  | (Just _) <- lookup "firstform" mn = do
      putStrLn "have nothing, will return textfield"
      return inputForm
  | (Just (Just txt)) <- lookup "input" mn = do
       putStrLn $ "will complete "++ show txt
       autoComplete txt

findText gr req mn b mvar = do
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
         cokh <- case getCookie req of
              Just id -> do 
                          putStrLn "found cookie"
                          return []
              Nothing -> do 
                         i <- takeMVar mvar
                         putMVar mvar (i+1)
                         runCommand $ "mkdir "++usermap (show i)
                         runCommand $ "mkdir "++"images/"++usermap (show i)
                         putStrLn $ "have created "++"mkdir "++usermap (show i)
                         return 
                           [("Set-Cookie",BU.fromString ("gfswedish="++show i))]
         return  $
           ResponseFile status200 ([("Content-Type", "text/html")]++cokh)
               ("static/index.html") Nothing
 where getTyp ".js"  = "application/javascript"
       getTyp ".css" = "text/css"
       getTyp ".html" = "text/html"
       getTyp ".svg"  = "image/svg+xml"

usermap :: String -> String
usermap = ("usermap"++)
getCookie :: Request -> Maybe String
getCookie = maybe Nothing parseCookies . findCookie
  where findCookie = lookup "Cookie" . requestHeaders 
        parseCookies :: B.ByteString -> Maybe String
        parseCookies = listToMaybe . map (usermap . show . fst) . parseInt 
                     . drop 10 . dropWhileList (not . ("gfswedish" `isPrefixOf`))
                     . BU.toString 
          where parseInt :: String -> [(Int,String)]
                parseInt = reads
           

inputForm :: Response
inputForm = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat
             $ map copyByteString
              [BU.fromString $ show textInputField
              ,"<p>This is a grammar-driven parser for Swedish, using the grammar formalism GF. "
              ,"The parser and grammar is under development, and currently uses a very small lexicon.</p>"
              ,"<p><a href='http://www.grammaticalframework.org'>Grammatical Framework</a></p>"
              ,"<p><a href='https://github.com/MalinAhlberg/SwedishProject'>Source code</a></p>"
              ]
             -- ,BU.fromString verbInfo, "!</p>"]
              
textInputField :: Html
textInputField = form << 
      [paragraph << (input ! [strAttr "name" "text", strAttr "autocomplete" "off" ])
      ,paragraph  ! [strAttr "id" "completion"] << noHtml
      ,submit "" "Submit"]

parseIt :: Maybe String -> ParseData -> B.ByteString -> Bool -> IO Response 
parseIt id (maps,pgf) txt b = do 
  let dir = fromJust id
  (t,results) <- timeItT $ if b then processparse dir (BU.toString txt) pgf maps
                           else do print ("smallparse "++ BU.toString txt)
                                   smallparse dir (BU.toString txt)
  putStrLn $ "parse trees "++show results
  putStrLn $ "It took "++show t++" seconds "
  let html = map getHtml results
  return $
        ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat 
          $ map copyByteString $ concat html
 where getHtml (s,n,res) = case res of
              Right (pt,at) ->
                     [ "<p>Parsed: ",pretty s,results n, "!</p>"
                     , "<div><a href='?img=",BU.fromString pt,"'><img src='?img="
                     ,BU.fromString $ pt,"'> </a>"
                     , "<a href='?img=",BU.fromString at,"'><img src='?img="
                     ,BU.fromString at,"'> </a></div>"]++moreTrees n (snd s)
              Left xs        -> 
                    [ "<p>Parsed: ",pretty s,results 0, "!</p>"
                    , "<p>Unknown words: ",BU.fromString $ show xs,"!</p>" ]
       results :: Int -> B.ByteString
       results i = BU.fromString (" which resulted in "++show i++" trees")
       pretty :: (String,String) -> B.ByteString
       pretty (i,s) = BU.fromString ("Sentence "++i++" \""++s++"\"")
       moreTrees :: Int -> String -> [B.ByteString]
       moreTrees i s | i>1 = ["<p><a href='?more=",BU.fromString s
                             ,"'>Show more trees </a></p>\n"]
                     | i<2 = []

parseMore :: Maybe String -> B.ByteString -> IO Response
parseMore id txt = do
  let user = fromJust id --fail otherwis
  links <- reparse user (BU.toString txt)
  putStrLn $ "have reparsed, got "++show (length links)++" trees"
  let html = "<p>All trees</p>\n" : concatMap (\(pt,at) -> 
              ["<p>","<a href='?img=",BU.fromString pt
              ,"'><img src='?img=",BU.fromString pt,"'></a>\n"
              ,"<a href='?img=",BU.fromString at,"'><img src='?img="
              ,BU.fromString at,"'></a></p>\n"])
                     links
  return $
    ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat 
         $ map copyByteString html


autoComplete :: B.ByteString -> IO Response
autoComplete txt = do
  w <- getNextWord (BU.toString txt) 10
  let res  = if null w then [] 
                else ["<p>",BU.fromString "Some alternatives: "
                     ,BU.fromString $ unwords w,"</p>"]
  putStrLn $ "autoComplet returns" ++ unwords w
  return $ ResponseBuilder status200 [("Content-Type", "text/html")]
         $ mconcat $ map copyByteString $ res

   
--verbInfo :: String
--verbInfo = concatMap (\(a,b) -> "Verbs of type "++a++":\n"++unlines b++"\n")
--             [("V3",v3),("V2",v2),("VV",vv) ,("VA",va),("V2V",v2v),("V2A",v2a)]
--
--v2 = ["tycka om", "raka", "fånga", "titta på"," se","ta","slå","akta sig för","äta","dricka"
--      ,"finnas", "saknas", "fattas", "glömma", "läsa", "skriva", "bli", "ha"]
--v3 = ["ta med sig till", "ge till", "ge", "erbjuda"]
--vv = ["börja", "måste", "vill", "kan"]
--va = ["bli"]
--v2v = ["be"]
--v2a = ["måla"]
