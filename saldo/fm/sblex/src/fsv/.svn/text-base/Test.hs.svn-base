module Test (tests) where

import List
import Frontend
import Char
import Maybe
import TestGen
import qualified Data.Set as Set

tests :: (PositiveTests, NegativeTests)
tests = (pos,neg)
 where 
 pos = pos_gen_rules
 neg = neg_gen_rules

-- General rules
pos_gen_rules = [contains_vowel, contains_consonant]

neg_gen_rules = [symbols]

-- every word, with some exceptions, should contain a vowel.
contains_vowel :: TestInput -> Result
contains_vowel t
  | or [is_vowel c | c <- w t] || null (w t)          = pass
  | otherwise                                         = message t "no vowel"

-- every word, with some exceptions, should contain a consonant.
contains_consonant :: TestInput -> Result
contains_consonant t
 | is_abbr t || (tcat t) == "nl"                = pass
 | or [is_consonant c | c <- w t] || null (w t) = pass
 | otherwise                                    = message t "no consonant"

-- no consecutive symbols.
symbols :: TestInput -> Result
symbols t
 | traverse (w t) 
   $ \s -> (case s of 
             (c:c1:_) | c == c1 && elem c "-:\"+." -> True
             _        -> False) = message t "consecutive symbols"
 | otherwise = pass

