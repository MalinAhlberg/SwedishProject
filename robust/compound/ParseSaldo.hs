module ParseSaldo where
import Data.Attoparsec.Text as A
import Data.Attoparsec  hiding (Parser) 
import Data.Text hiding (head,map,concat)
import Control.Applicative
import Control.Monad
import System.TimeIt
import Debug.Trace
import qualified Data.HashMap as M 

test = timeIt (do 
  s <-readFile "../../saldo/saldom.xml"
  let m = tryp parseSaldo s
  case m of
       Left e  -> putStrLn e
       Right p -> print $ M.lookup (pack "gatu") p)
   >>= print 

tryp p = A.parseOnly p . pack

getSaldo = do
  s <-readFile "../../saldo/saldom.xml"
  let m = tryp parseSaldo s
  case m of
       Left e  -> error "couldn't parse saldo"
       Right p -> return p

parseSaldo :: Parser (M.Map Text [(Text,Text)])
parseSaldo = do
   xs <- concat <$> many parseEntry
   return $ M.fromListWith (++) xs

readNextTag = do
  flushTill "<"
  t <- A.takeWhile (/='>')
  char '>'
  return t

parseEntry :: Parser [(Text,[(Text,Text)])]
parseEntry = do
  flushTill "<lem>"
  lem <- A.takeWhile (/='<')
  tb <- many parseTable 
  return $ map (\(f,w) -> (w,[(f,lem)])) tb

parseTable :: Parser (Text,Text)
parseTable = do
  t <- readNextTag 
  guard (t/=pack "/table")
  when (t/=pack "form") $ flushTill "<form>"
  string' "<param>"
  form <- A.takeWhile (/='<')
  flushTill "<wf>"
  w <- A.takeWhile (/='<')
  string' "</wf></form>" --eat the rest, to avoid confusion
  return (form,w)
   
flushTill :: String -> Parser ()
flushTill str = do
  A.takeWhile (/= head str)
  string' str <|> (anyChar >> flushTill str)

string' str = do
  A.string $ pack str
  return ()
