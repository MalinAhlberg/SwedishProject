module Main where
import System.Timeout
import System.Environment (getArgs)
import Data.Maybe
import Data.Map hiding (map,null,filter)
import Data.Ord
import Data.Char
import Data.List
import Data.Function
import Control.Concurrent
import Control.Arrow
import Control.Applicative
import Control.Monad.State
import PGF

import Parsing
import XMLHelp hiding (words,parse)

-- use with tee -a outfile
main = do
  (file:arg) <- getArgs
  let strict = isStrict arg
  run file strict outFile


