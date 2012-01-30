{-# LANGUAGE TemplateHaskell #-}
module Log where
import Control.Concurrent
import System.Process
import Data.Global

declareChan "ch"  [t| (FilePath,String) |]


logA :: String -> String -> IO ()
logA id str = writeChan ch (logFile,(id++": "++str))
  where logFile = "logs/userlog.log"

log :: String -> IO ()
log str = writeChan ch ("logs/log.log",str)

err :: String -> IO ()
err str = writeChan ch ("logs/err.log",str)

appendFileLn file str = do 
  d <- readProcess "date" [] []
  appendFile file (d++"\n"++str++"\n")
  
writeMsg :: IO ()
writeMsg = do
  (file,str) <- readChan ch 
  appendFileLn file str
  writeMsg


--forkIO_ x = forkIO x >> return ()
