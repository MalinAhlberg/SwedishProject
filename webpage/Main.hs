{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.Wai.Application.Static
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import System.Process
import System.IO
import Text.XHtml.Transitional
import Text.XHtml
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    --run port app
    run port parseF

--parseF :: Request -> IO Response
parseF req = do
  liftIO $ putStrLn "start"
  let mn = queryString req
  x <- liftIO $ findText req mn 
  liftIO $ putStrLn "have returned"
  return x

findText :: Request -> Query -> IO Response
findText req mn = 
         do let im = lookup "img" mn
            case im of
                Just (Just x) -> do
--                                 putStrLn "read file"
--                                 fil <- B.readFile  "images/quote_1.gif" --Process "cat" ["images/bg.jpg"] []
--                                 putStrLn $ show fil
--                                 putStrLn "read file -done"
                                 hSetEncoding stdin latin1
                                 return $
                                   ResponseFile status200 [("Content-Type", "image/gif")] 
                                         "/home/malin/Swedishproject/webpage/images/quote_1.gif" Nothing
                                   --ResponseBuilder status200 [("Content-Type", "image/gif")] 
                                   --    $ copyByteString $ fil 
                Nothing -> undefined
            let txt = lookup "txt" mn
            case im of
                Just (Just x) -> do
                                 fil <- readFile "images/bg.jpg"
                                 return $
                                   ResponseBuilder status200 [("Content-Type", "image/jpeg")] 
                                       $ copyByteString $ BU.fromString $ show fil 

  --          return $ staticApp defaultWebAppSettings req
--         `mplus`
--         do xm <- lookup "text" mn
--            return $ index xm 
--         `mplus`
--         return inputForm
  --x <- liftIO $ parseIt tools mn
--  x <- maybe (return inputForm) (liftIO . parseIt) mn
--  output . renderHtml $ page "Input example" x

inputForm :: Response
inputForm = ResponseBuilder status200 [("Content-Type", "text/html")] $ copyByteString
              (BU.fromString $ show $ hej)
              
hej :: Html
hej = form << [paragraph << (--"Parse bitte " 
                        ((textarea noHtml  )  ! ([strAttr "name" "text"]))),
                     submit "" "Submit"]

pic :: Html
pic = form << [(image )  ! ([strAttr "src" "http://localhost/cgi-bin/output/tmp.png"])]
 {-
app req = return $
    case pathInfo req of
        ["yay"] -> yay
        x -> index x
        -}
 
yay = ResponseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]
 
--index :: Show a => a -> Response
index x = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n", BU.fromString $ show $ pic]
