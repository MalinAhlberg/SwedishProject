module Main where

import CommonMain
import Dict
import Commands
import Frontend
import Test
import PrintSw
import Dictionary(Entry,set_lemma_id)
import qualified Dict.Abs as Abs

main :: IO ()
main = commonMain Fornsvenska

data Fornsvenska = Fornsvenska
 deriving Show

instance Language Fornsvenska where
    morphology_header _ = "FM-FSV 1.0\n © L. Borin, M. Forsberg, 2010, under GNU LGPL 3.0 or CC-SA 2.5 Generic"
    name         _ = "fsv"
    lprinter     _ = print_table
    internDict   _ = dict
    paradigms    _ = foldr insertCommand emptyC commands
    termParser _ ts e = add_id ts e
    testBench _ = tests

add_id :: [Abs.Term] -> Entry -> Entry
add_id [Abs.TermC _ [(Abs.TermA (Abs.NStr s))]]  e = set_lemma_id s e
add_id _                                         e = e
