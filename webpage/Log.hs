module Log where
import Control.Concurrent

logA :: String -> String -> IO ()
logA id str = forkWrite . appendFileLn logFile (id++": "++str)
  where logFile = "logs/userlog.log"

log :: String -> IO ()
log = forkWrite . appendFileLn "logs/log.log" 

err :: String -> IO ()
err = forkWrite . appendFileLn "logs/err.log"

appendFileLn file str = appendFile file (str++"\n")
forkIO_ x = forkIO x >> return ()
