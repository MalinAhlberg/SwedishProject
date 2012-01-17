
import Data.Attoparsec.Char8 as A
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 as B 
import Data.ByteString.UTF8 as BU

parseStuff = parseOnly verbValency . fromString

verbValency = do
  xorA
  theWord
  v2
  
char' c = do
  skipSpace
  char c

string' = string . fromString 

subject = xorA

xorA = do
    char' 'A' <|> char' 'x' <|> char' 'y' <|> char' 'B'
    mone (char' '/' >> (char' 'A' <|> char' 'x'))

theWord = do
   char' '&' 

--          reflexive,preposition,particle
v2 :: Parser (Maybe ByteString,Maybe ByteString,Maybe ByteString)
v2 = do
  pa <- maybeP particle 
  pr <- maybeP (do mone (char' '(') 
                   skipSpace
                   A.takeWhile $ notInClass "yzxAB/)( ") -- no prepositions with yzx..
  rf <- maybeP sig 
  xorA
  mone (char' ')')
  return (rf,trimJust pr,pa)

  
sig = do
  char' '('
  s <- string' "sig"
  char ')'
  return s 

particle = do
  char' '('
  p <- A.takeWhile $ notInClass "xyzABC/)( "
  char ')'
  return p

mone p = (p >> return ()) <|> return ()

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = do
   (p >>= return . Just)
   <|>
   return Nothing

trimJust :: Maybe ByteString -> Maybe ByteString
trimJust (Just x) | B.null x = Nothing
trimJust x         = x

{-
    word <- str
    char' ','
    res <- liftM ((,) word) (list lemma)
    char' ')'
    return res
    -}


