module GenRulesSw where

import General
import List
import qualified Data.Set as Set

type Suffixes = [((String -> String), String)]

e :: String -> ((String -> String),String)
e s = (id,s) 

apply_suffixes :: String -> Suffixes -> [String]
apply_suffixes stem xs = [f stem ++ suff | (f,suff) <- xs]

lift :: [a] -> [[a]]
lift  [] = []
lift   s = [s]

keep :: Int -> (String -> String) -> String -> String
keep n f s = f pre ++ suf
 where
  (pre,suf) = case splitAt n (reverse s) of
               (rs,rp) -> (reverse rp, reverse rs)

drop_last_vowel = dv

dsuff :: String -> String -> String
dsuff suff s 
 | isPrefixOf (reverse suff) (reverse s) = tk (length suff) s
 | otherwise = s

dv :: String -> String
dv w = case find_stem_vowel w of
        (seg,e,l) -> seg ++ l


dvu :: String -> String
dvu w = case find_stem_vowel w of
          (seg,e,l) -> (ungeminate_m_n seg) ++ l

fr :: String -> String
fr s = case reverse s of
         ('f':v:_) -> s ++ "v"
         _ -> s

fv s = if (dp 2 s == "fv") then tk 1 s else s

dpe :: String -> String
dpe w = case find_stem_vowel w of
         (seg,e,l) | slast e == 'e' && length l < 2 -> seg ++ init e ++ l
         _ -> w

slast x@(_:_) = last x
slast _ = '_'

dpeu :: String -> String
dpeu w = case find_stem_vowel w of
           (seg,e,l) | slast e == 'e' && length l < 2 -> ungeminate_m_n seg ++ init e ++ l
           _ -> ungeminate_m_n w

dpl :: Char -> String -> String
dpl c s = case span (/= c) (reverse s) of
           (b,_:r) -> reverse (b ++ r)
           (b,[])  -> reverse b

drop_second_last :: String -> String
drop_second_last s = (tk 2 s) ++ (dp 1 s)

is_vowel :: Char -> Bool
is_vowel c = Set.member c vowel 

vowel = Set.fromList "aeiouyåäöAEIOUYÅÄÖ"

is_consonant :: Char -> Bool
is_consonant c = Set.member c consonant

consonant = Set.fromList "bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZ"

is_voiced :: Char -> Bool
is_voiced c = Set.member c voiced

voiced = Set.fromList "bdglmnrvjBDGLMNRVJ"

if_vowel :: Char -> String -> String -> String
if_vowel c d e = if is_vowel c then d else e

drop_final_e :: String -> String
drop_final_e = dropEndIf (=='e')

ds :: String -> String
ds s =  case reverse s of
        (x:y:_) | ((is_consonant x) && y == 's') || ((is_consonant x) && y == 'x') || [y,x] == "ch" || x=='x' || x =='z' || x =='s' -> s
        [] -> []
        _   -> s +? "s"

ds_drop :: String -> String
ds_drop s = ds $ if iw s then safe_init s else s
  where iw [] = False
        iw str = is_vowel (last str)

fl :: String -> String
fl s = case words s of
         [x,y] -> y +? x
         _     -> error "Unable to flip in \"" ++ s ++ "\""

-- dropEndIf (=='s')

compound_s :: String -> [String]
compound_s s = case reverse s of
                 (x:y:_) | ((is_consonant x) && y == 's') || ((is_consonant x) && y == 'x') || [y,x] == "ch" || x=='x' || x =='z' || x=='s' -> [s]
                 [] -> []
                 _   -> [s +? "s"]

insert_second_last :: String -> Char -> String
insert_second_last s c = safe_init s ++ [c] ++ (f s)
  where f [] = []
        f str = [last str]

geminate :: String -> String
geminate s = s ++ dp 1 s

ungeminate :: String -> String
ungeminate s = case reverse s of
  'm':'m':_ -> safe_init s
  _         -> s

ungeminate_m_n :: String -> String
ungeminate_m_n s = case reverse s of
  n:m:_ | n == m && elem n "nm" -> safe_init s
  _ -> s

safe_init :: String -> String
safe_init [] = []
safe_init xs = init xs

mmn :: String -> String
mmn s = case reverse s of
         ('n':'m':'m':xs)  -> (reverse ('n':'m':xs))
         _                 -> s

--3 Umlaut
--
-- Let's conclude with something that is not easy to do on this level of generality
-- with regular expressions: 
-- define first the *stem vowel* as the last vowel (or diphtong) in the stem:

find_stem_vowel :: String -> (String, String, String)
find_stem_vowel sprick = (reverse rps, reverse i, reverse kc) where
  (kc, irps) = break is_vowel $ reverse sprick
  (i,   rps) = span  is_vowel $ irps

-- vowel change
vc :: String -> String -> String
vc v sprick = 
  case find_stem_vowel sprick of
    (spr,i,ck) -> spr ++ v ++ ck

vct :: [(String,String)] -> String -> String
vct xs sprick =   case find_stem_vowel sprick of
                    (spr,i,ck) -> spr ++ (v i) ++ ck
 where v i = case lookup i xs of
               Just x -> x
               Nothing -> i



triplecons :: String -> Char -> String
triplecons s c = case reverse s of
                   (c1:c2:_) | c1 == c && c1 == c2 -> safe_init s
                   _  -> s

-- Although *umlaut* is not very very useful in Swedish, we are glad to
-- present a general rule for it:

umlaut :: String -> String
umlaut man = m ++ mkUm a ++ n where
  (m,a,n) = find_stem_vowel man
  mkUm v = case v of
    "a" -> "ä"
    "o" -> "ö"
    "å" -> "ä"
    "u" -> "y"
    _   -> v
