module ParseLexin where
import Data.Attoparsec.Char8 as A
import Control.Monad
import Control.Applicative
import Data.ByteString.Char8 as B 
import Data.ByteString.UTF8 as BU


-- need to use lexer? gottar sig (åt x/att + INF)  should be V2: gottar sig åt and VV : gottar sig åt att
parseValency = tryp verbValency 
parseWords = tryp wlist
tryp p = parseOnly p . BU.fromString

verbValency = do
  xorA
  theWord
  v <- sepBy (v2 <|> v3 <|> vv <|> moreVal) (char' '/')
  return v

moreVal = A.anyChar >> return (VT X [] []) -- to be implemented properly
subject = xorA

xorA = 
  skip (do char' 'A' <|> char' 'x' <|> char' 'y' <|> char' 'B' 
           mone (char' '/' >> (char' 'A' <|> char' 'x')))
  <|> skip littleb -- a small b is ok if it is not part of a word
 where littleb = string' "b/x" <|> string' "b " <|> string' "b)"
        

theWord = do
   char' '&' 

data VerbType = VT {vtype :: V , argument :: [Argument], preps :: [Preposition]}
  deriving (Show)
   -- X is unparseble type
data V = V2 | V3 | X | VV Bool --VS VV VQ VA V2V V2S V2Q V2A
  deriving (Show)
data Argument = Part ByteString  --particles
              | Refl             --is reflexive
              -- | Inf Bool         --infinitival verb, True if inifinitive marker is used
  deriving (Show)
type Preposition = Maybe ByteString

v2 :: Parser VerbType
v2 = do
  xs <- A.many (particle True <|> sig) 
  mone (char' '(')   -- verbs that can be used as both V and V2 will assigned V2
  pr <- preposition
  xorA
  mone (char' ')')
  return $ VT V2 xs [pr]

v3 :: Parser VerbType
v3 = do
  xs <- A.many (particle True <|> sig)
  pr1 <- preposition
  mone (char' '(')   -- verbs that can be used as both V2 and V3 will assigned V3
  xorA
  mone (char' ')')
  pr2 <- preposition
  xorA
  return $ VT V3 xs [pr1,pr2]

vv :: Parser VerbType
vv = do
  ps <- A.many (particle True <|> sig)
  b <- (mone (char' '(') >> string' "att" >> mone (char' ')') >> return True) -- ignores facts about whether infinitive marker can be left out. improve!
        <|> 
        return False
  char' '+'
  skipSpace
  string' "INF"
  return $ VT (VV b) ps []

preposition :: Parser Preposition
preposition = maybeP $ do
 --mone (char' '(')   -- paranthesis to bind the preposition to the verb
 skipSpace
 pr <- A.takeWhile1 $ notInClass nowords -- no prepositions with yzx..
 skipMany alternative -- ignore alternatives (should be improved)
 return pr
 where alternative = do
        char '/'
        A.takeWhile1 $ notInClass nowords
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
  p <- part
  skipMany alternative  -- ignore alternatives (should be improved)
  when par $ skip (char ')')
  return $ Part p
 where alternative = do
          char' '/'
          A.takeWhile1 $ notInClass nowords
          maybeP $ string' " etc"
       part = do  -- makes sure that we do not interpret 'b' as a particle
          skipSpace
          (x,xs) <- do a    <- satisfy (notInClass $ 'b':nowords)
                       rest <- A.takeWhile $ notInClass nowords
                       return (a,rest)
                    <|> 
                    do b <- char 'b'
                       rest <- A.takeWhile1 $ notInClass nowords 
                       return (b,rest)
          return (x `cons` xs)

nowords = "xyzABC/)(, "

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
