module ParseLexin where
import Data.Attoparsec.Char8 as A
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 as B 
import Data.ByteString.UTF8 as BU

parseValency = tryp verbValency 
parseWords = tryp wlist
tryp p = parseOnly p . BU.fromString

verbValency = do
  xorA
  theWord
  v <- v2
  sepBy moreVal (char' '/')
  return v

moreVal = A.anyChar  -- to be implemented properly
subject = xorA

xorA = do
    char' 'A' <|> char' 'x' <|> char' 'y' <|> char' 'B' <|> (char' 'b' >> char ' ')
    mone (char' '/' >> (char' 'A' <|> char' 'x'))

theWord = do
   char' '&' 

type V2Arg = ([Argument],[Preposition])
type V3Arg = V2Arg
data Argument = Part ByteString | Refl -- | Prep ByteString
  deriving (Show)
type Preposition = ByteString

v2 :: Parser V2Arg
v2 = do
  xs <- A.many (particle True <|> sig) 
  pr <- preposition
  mone (char' '(')   -- verbs that can be used as both V and V2 will assigned V2
  xorA
  mone (char' ')')
  return (xs,[pr])

v3 :: Parser V3Arg
v3 = do
  xs <- A.many (particle True <|> sig)
  pr1 <- preposition
  xorA
  pr2 <- preposition
  xorA
  return (xs,[pr1,pr2])



preposition :: Parser Preposition
preposition = do
 mone (char' '(')   -- paranthesis to bind the preposition to the verb
 skipSpace
 pr <- A.takeWhile1 $ notInClass "yzxAB/)(, " -- no prepositions with yzx..
 skipMany alternative -- ignore alternatives (should be improved)
 return pr
 where alternative = do
        char '/'
        A.takeWhile1 $ notInClass "xyzABC/)( "
        maybeP $ string' " etc"

  
sig = do
  sig1 <|> sig2
  return Refl
 where sig1 =  do  --ignoring whether the reflexive pronoun may be left out
         char' '('
         string' "sig"
         char ')'
       sig2 = string' "sig" >> return 's'


particle par = do
  when par $ skip (char' '(')
  p <- A.takeWhile1 $ notInClass "xyzABC/)(, "
  skipMany alternative  -- ignore alternatives (should be improved)
  when par $ skip (char ')')
  return $ Part p
 where alternative = do
        char' '/'
        A.takeWhile1 $ notInClass "xyzABC/)( "
        maybeP $ string' " etc"


-- Parse word lists
wlist :: Parser [(ByteString,[Argument])]
wlist = sepBy entry (string' ", ")
 where entry = do
          wd <- A.takeWhile1 $ notInClass ", "  
          A.many (skipSpace >> digit)       -- ignore numbers (sår 2)
          ags <- A.many ((char ' '>> sig) <|> (char ' ' >> particle False))
          return (wd,ags)



-- help functions  
char' c = do
  skipSpace
  char c

string' = string . BU.fromString 

skip :: Parser a -> Parser ()
skip p = p >> return ()

mone p = (skip p) <|> return ()

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = do
   (p >>= return . Just)
   <|>
   return Nothing

trimJust :: Maybe ByteString -> Maybe ByteString
trimJust (Just x) | B.null x = Nothing
trimJust x         = x

trimArg :: ByteString -> (ByteString -> Argument) -> [Argument]
trimArg b p | B.null b  = []
            | otherwise = [p b]
