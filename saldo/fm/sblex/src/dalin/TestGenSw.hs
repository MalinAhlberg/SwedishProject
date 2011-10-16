module TestGenSw where

import List
import Frontend
import Char
import Maybe

is_a_word  = isJust . tword

not_a_word = not . isJust . tword

p_pos t = takeWhile (/='_') (tpara t)

p_annotation t =  
    case dropWhile (/='_') (tpara t) of
      (_:xs) -> takeWhile (/='_') xs

p_word t = case dropWhile (/='_') (tpara t) of
             (_:xs) -> case dropWhile (/='_') xs of
                         (_:ys) -> map f ys
 where f '_' = ' '
       f x   = x

id_pos   t = f (tid t)
 where f [] = error "invalid id: " ++ (tid t)
       f ('.':'.':'.':xs) = f ('.':'.':xs)
       f ('.':'.':xs)     = takeWhile (/='.') xs
       f (x:xs) = f xs 

id_lemma t = f (tid t)
 where f [] = error "invalid id: " ++ (tid t)
       f ('.':'.':'.':xs) = '.':f ('.':'.':xs)
       f ('.':'.':xs)     = []
       f (x:xs)           = x : f xs 

norm_id_lemma t = remove_number $ reverse $ i
  where i = id_lemma t
        remove_number (x:xs) 
         | isDigit x = case dropWhile isDigit xs of
                         ('_':zs@(_:_)) -> reverse zs
                         _              -> i
         | otherwise = i

head_to_lemma_id = add_underscore . symbols . unspace . thead
 where unspace s = [if (x == ' ') then '_' else x | x <- s]
       symbols = concat . map tr
       add_underscore (x:xs)
        | isDigit x || elem x "-·" = ('_':x:xs)
        | otherwise = (x:xs)
       tr c = maybe [c] id $ lookup c 
               [('&',"·26"),(':',"·3A"), ('\'',"·27"), ('/',"·2F"), ('+',"·2B"), 
                (',',"·2C"),('°',"·B0"), ('%',"·25"),('=',"·3D"),('?',"·3F")] 

is_multi_word t = length pos > 2 && last pos == 'm' 
 where pos = tcat t

is_abbr t = last (tcat t) == 'a'

is_vowel c = elem c "aeiouyåäöAEIOUYÅÄÖüÜàÀ"

is_consonant c = elem c "bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZ"

traverse :: String -> (String -> Bool) -> Bool
traverse [] f = False
traverse s@(x:xs) f
 | f s       = True
 | otherwise = traverse xs f

member_str :: String -> String -> Bool
member_str pre s = traverse s (isPrefixOf pre)

decl :: TestInput -> Char -> Bool
decl t c
 | elem (tcat t) ["nn","nnm","nna"] =  (head (p_annotation t)) == c 
 | otherwise                        = False

conj :: TestInput -> Char -> Bool
conj t c
 | elem (tcat t) ["vb","vbm","vba"] =  (head (p_annotation t)) == c 
 | otherwise                        = False

param :: TestInput -> [String] -> Bool
param t xs = and [elem x (tparam t) | x <- xs]

is_no_plural :: TestInput -> Bool
is_no_plural t 
 | elem (tcat t) ["nn","nnm","nna"] = case (p_annotation t) of
                                        ('0':_) -> True
                                        _       -> False
 | otherwise      = False

gender :: TestInput -> Char -> Bool
gender t c
 | elem (tcat t) ["nn","nnm","nna"] = case p_annotation t of
                                        (_:x:_) -> c == x
 | otherwise                        = False

is_no_komp :: TestInput -> Bool
is_no_komp t
 | elem (tcat t) ["av","avm","ava"] = case dropWhile (/='_') (tpara t) of
                      (_:'0':'_':_) -> True
                      _             -> False
 | otherwise      = False

is_komp :: TestInput -> Bool
is_komp t
 | elem (tcat t) ["av","avm","ava"] = 
     case dropWhile (/='_') (tpara t) of
       (_:x:_) | elem x "12v" -> True
       _                        -> False
 | otherwise      = False

is_no_pret_part :: TestInput -> Bool
is_no_pret_part t 
 | tcat t == "vb" = case p_annotation t of
                      (_:'m':_) -> True
                      _       -> False
 | otherwise      = False

is_verb_sd :: TestInput -> Bool
is_verb_sd t 
 | tcat t == "vb" = case p_annotation t of
                      (_:'s':_) -> True
                      (_:'d':_) -> True
                      _       -> False
 | otherwise      = False

is_pret_part :: TestInput -> Bool
is_pret_part t 
 | tcat t == "vb" = case p_annotation t of
                      (_:'a':_) -> True
                      _         -> False
 | otherwise      = False

