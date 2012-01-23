module ParseLexin where
import Prelude as Pr
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
  xs <- A.many (particle True <|> sig) --outside of the paranthesis, in order to avoid
                                       -- confusion
  mone $ char' '('  --- should make sure they match!!
  pr1 <- preposition
  vs <- sepBy (v2s <|> v3 <|> v2a <|> va <|> vv <|> vs <|> vq <|> v2 <|> v <|> moreVal) (char '/')
  mone $ char' ')' --- should make sure they match!!
  return $ Pr.map (\v -> VT (vtype v) (xs++argument v) (pr1:preps v)) vs

moreVal = A.anyChar >> return (VT X [] []) -- to be implemented properly
subject = xorA

xorA = 
  do char' 'A' <|> char' 'x' <|> char' 'y' <|> char' 'B' 
     mone (skip orlittleb <|> skip (char' '/' >> (char' 'A' <|> char' 'x' <|> char' 'c')))
  <|> 
  skip littleb -- a small b is ok if it is not part of a word
 where littleb = string' "b/x" <|> string' "b " <|> string' "b)" <|> string' "b/y"
       orlittleb = string' "/b"
        

theWord = do
   char' '&' 

data VerbType = VT {vtype :: V , argument :: [Argument], preps :: [Preposition]}
  deriving (Show)
   -- X is unparseble type
data V = V | V2 | V3 | X | VV Bool | VS | VQ | VA | V2S | V2Q | V2A -- V2V, very uncommon 
  deriving (Show)
data Argument = Part ByteString  --particles
              | Refl             --is reflexive
              -- | Inf Bool         --infinitival verb, True if inifinitive marker is used
  deriving (Show)
type Preposition = Maybe ByteString

v :: Parser VerbType
v = do
 xs <- A.many (particle True <|> sig) 
 return (VT V xs [])

v2 :: Parser VerbType
v2 = do
  xs <- A.many (particle True <|> sig) 
  mone (char' '(')   -- verbs that can be used as both V and V2 will assigned V2
  pr <- preposition
  xorA
  mone (char' ')')
  return $ VT V2 xs [pr]

{- Assigns V3 also two verbs with a counjunction as argument
   Eg. 'särar på x och y'
   RunProg could handle this in a clever way -}
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
  pp <- preposition
  skipSpace
  b <- (mone (char' '(') >> string' "att" >> mone (char' ')') >> return True) -- ignores facts about whether infinitive marker can be left out. improve!
        <|> 
        return False
  skipSpace
  char' '+'
  skipSpace
  string' "INF"
  mone $ char' ')'
  return $ VT (VV b) ps [pp]

vs :: Parser VerbType
vs = do
  ps <- A.many (particle True <|> sig)
  skipSpace
  mone $ char' '('
  --ignores which one, should be improved
  sepBy ((string' "att" <|> string' "hur" <|> string' "när") 
                                          >> mone (string' " etc")) (char' '/') 
  mone $ char' ')'
  char' '+'
  skipSpace
  skip (string' "SATS") <|> (string' "S" >> endOfInput)
  return $ VT VS ps []

vq :: Parser VerbType
vq = do
  ps <- A.many (particle True <|> sig)
  mone $ char' '+'
  skipSpace
  string' "FRÅGESATS"
  return $ VT VQ ps [] 

{- Accepts predicative verbs, which may not be VA
   Eg. 'avancerar som (till) PRED' -> when finding 'som' use as V2? 
        (to be done in RunProg)
   See SAG Verbfraser: Predikativ $ 23, Talbanken 4440,4438 -}
va :: Parser VerbType
va = do
  ps <- A.many (particle True <|> sig)
  mone $ char' '+'
  skipSpace
  string' "PRED"
  return $ VT VA ps [] 

v2s :: Parser VerbType
v2s = do
  mone $ char' '('
  v  <- v2
  mone $ char' ')'
  mone $ char' '('
  v' <- vs
  mone $ char' ')'
  return $ VT V2S  (argument v ++ argument v') (preps v ++ preps v')

v2a :: Parser VerbType
v2a = do
  mone $ char' '('
  v  <- v2
  mone $ char' ')'
  mone $ char' '('
  v' <- va
  mone $ char' ')'
  return $ VT V2A  (argument v ++ argument v') (preps v ++ preps v')

preposition :: Parser Preposition
preposition = maybeP $ do
 skipSpace
 pr <- getOneWord
 skipMany alternative -- ignore alternatives (should be improved)
 return pr
 where alternative = do
        char '/'
        getOneWord
        maybeP $ string' " etc"

  
sig = do
  sig1 <|> sig2
  return Refl
 where sig1 =  do  --ignoring whether the reflexive pronoun may be left out
         char' '('
         string' "sig"
         char ')'
       sig2 = skipSpace >> string' "sig" >> return 's'


particle par = do
  when par $ skip (char' '(')
  p <- getOneWord
  skipMany alternative  -- ignore alternatives (should be improved)
  when par $ skip (char ')')
  return $ Part p
 where alternative = do
          char' '/'
          getOneWord 
          maybeP $ string' " etc"

-- gets one word, not 'b' and not 'att' or others, as specified in notparticles
getOneWord = do  
  skipSpace
  part <- A.takeWhile1 $ notInClass nowords
  guard $ not $ part `Pr.elem` notparticles 
  return part

notparticles = Pr.map BU.fromString ["att","b","sig"]
nowords = "xyzABCFPS/)+(, "

-- Parse word lists
wlist :: Parser [(ByteString,[Argument])]
wlist = sepBy entry (string' ", ")
 where entry = do
          wd <- A.takeWhile1 $ notInClass ", "  
          A.many (skipSpace >> digit)       -- ignore numbers (sår 2)
          ags <- A.many ((char ' '>> sig) <|> (char ' ' >> particle False))
          return (wd,ags)



-- help functions  
(+++) :: B.ByteString -> B.ByteString -> B.ByteString 
(+++) = B.append


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
