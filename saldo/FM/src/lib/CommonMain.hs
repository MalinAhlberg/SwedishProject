module CommonMain where

import DictToDictionary
import Print
import List(intersperse,isPrefixOf,sortBy,nub)
import System(getArgs, getEnv)
import GeneralIO
import General
import IO
import qualified Data.Set as Set
import System(getProgName)
import Dictionary
import Frontend
import Char
import Dict.ErrM
import Print
import Monad
import Command
import Util
import Combine
--import Net
import qualified Data.Map as Map

-- import Tokenize
import qualified CTrie

data AnaType = 
    Normal                |
    NoAnalysis            |
    AnaEval               |
    FilterLexiconNo       |
    FilterLexiconNoComp   |
    FilterLexiconComp
    
gfTypes :: Language a => a -> String
gfTypes l = "types." ++ name l ++ ".gf"

readDicts :: Language a => a -> [FilePath] -> (Bool,Bool,Bool) -> IO (Dictionary,Int)
readDicts l fs (undefcheck,argccheck,unusedcheck) = do output
                                                       readDicts' l fs
 where
  output 
   | length fs > 1 = prErr $ "\nprocessing dictionaries in files: " ++ (unwords fs) 
   | null fs       = prErr $ "\nno dictionary loaded"
   | otherwise     = prErr $ "\nprocessing dictionary in file " ++ (unwords fs)
  readDicts' l [] = return $ (internDict l, 0)
  readDicts' l (f:fs) = do (d,n) <- readDicts' l fs
                           res   <- parseDict l f (undefcheck,argccheck,unusedcheck)
                           case res of
                             Ok (d1,n1) -> return $ (unionDictionary d d1,n+n1)
                             Bad s      -> do prErr s
                                              return (d,n)

prStatistics :: Language a => a -> Int -> IO()
prStatistics l n =
 do let is = size (internDict l)
    prErr $ print_lang l ++ print_paradigms l ++ print_size (n+is,n,is) ++ "\n"

print_lang l = "language id: " ++ (name l) ++ "\n"

print_paradigms l = case paradigmCount l of
                     0 -> "no paradigms\n"
                     1 -> "1 paradigm\n"
                     n -> show n ++ " paradigms\n"

print_size (0,n,isz)  = "no/empty dictionary"
print_size (1,_,_)    = "1 entry"
print_size (sz,n,isz) = show nsz ++ "k entries [e: " ++ show n ++ ", i: " ++ show isz ++ "]"
  where 
        nsz = fromInteger $ round (((fromInteger (toInteger sz))::Double) / 1000)

uName :: Language a => a -> String
uName l = case name l of
	   [] -> []
	   (x:xs) -> toUpper x : xs

commonMain :: Language a => a -> IO ()
commonMain l = do
  xx <- getArgs
  res <- retrieve l xx
  case res of
    Left err -> do prErr $ err
    Right (flags, []) | dictionary_needed flags
    	-> do prg <- getProgName
              prErr $ welcome l
              prErr $ "dictionary file required"
              prErr $ "Usage: " ++ prg ++ " [OPTION...] dictionary_file(s)...\n"
    Right (flags, files) -> 
     if is_help flags then do s <- help l ; prText (welcome l) ; prText s else if is_version flags then prText (welcome l) else
      do prErr $ welcome l
         let undefcheck      = is_undef flags      || is_all flags
             unusedcheck     = is_unused flags     || is_all flags
             argccheck       = is_argc flags       || is_all flags
             dupcheck        = is_duplicated flags || is_all flags
             (compInf,cmode) = (is_compound flags, get_compound flags)
             tokS            = if (is_reduce flags && not (elem (get_tokenizer_name flags) ["lines","norm"])) then norm_tokenizer
                                   else (get_tokenizer (tokenizer l) flags)
         when (is_reduce flags && not (elem (get_tokenizer_name flags) ["lines","norm"])) $ prErr "Invalid tokenizer in reduce mode, changed to 'norm'"
         (d,n) <- if (dictionary_needed flags) then 
                      do (dict,n1) <- readDicts l files (undefcheck,argccheck,unusedcheck)
                         return (apply_encoding l flags (dictionary_postprocessing l dict),n1)
                   else 
                       return (emptyDict,0)
         case flags of
           fs | invalid_tokenizer fs -> prErr $ "unknown tokenizer: " ++ (get_tokenizer_name fs)
           fs | invalid_quality   fs -> prErr $ "unknown quality argument: " ++ (get_quality fs)
           fs | is_quality fs -> do when dupcheck   $ check_lemma_duplication d
                                    when (is_test fs || is_all fs) $ tester (testBench l) (testdata d)
                                    when (is_paradigm_test fs || is_all fs) $ 
                                         do prErr "Running paradigm duplication check..."
                                            let lenw = length . concat . map words
                                            print_duplicates [(f xs, [fun xs | (p1,(ls,fun)) <- Map.toList (paradigms l), p1 /= p, lenw ls == lenw xs]) 
                                                                                                  | (p,(xs,f)) <- Map.toList (paradigms l), not (Set.member p (paradigm_dup_exception l))] (dup_id_exceptions l)
--           fs | is_net fs       -> case get_port fs of
--                                     Nothing -> prErr "Invalid port"
--                                     Just n   -> do build_trie l fs d
--                                                    prStatistics l n
--                                                    server n (\s -> prAnalysis l Normal s (analysis cmode (composition l) (sandhi_rules l) s))
           fs | elem Infl     fs ->  do prErr $ "[ FM inflection mode ]"
                                        s <- imode l
                                        output_write fs s
           fs | elem Synth   fs  -> do prErr $ "[ FM synthesiser mode ]\n"
			               CTrie.buildTrieDictSynt d False
                                       prStatistics l n
                                       s <- synthesiser l
                                       output_write fs s
           fs | is_paradigm fs -> lookup_paradigm l
           fs | elem Tag fs -> do --CTrie.buildTrieDict (isComp l) d False dupcheck
                                  build_trie l fs d
                                  prStatistics l n
			          s <- posify l tokS (analysis l cmode (composition l) (sandhi_rules l))
                                  output_write fs s
           fs | is_reduce fs -> do build_trie l fs d
                                   prStatistics l n
			           s <- posify l tokS (analysis l cmode (composition l) (sandhi_rules l))
                                   s2 <- readFile (get_reduce_file fs)
                                   output_write fs $ Combine.combine s s2
           fs | is_mode fs  -> do --CTrie.buildTrieDict (isComp l) d False dupcheck
                                  build_trie l fs d
                                  prStatistics l n
			          run l tokS (analysis l cmode (composition l) (sandhi_rules l)) (get_mode fs)
           fs | is_printer fs -> 
                output_write fs $
                  case printer fs of
                   (Just p) | elem p (map fst (lprinter l)) -> case lookup p (lprinter l) of
                                                                 Just p -> p d
                   (Just "core")      -> unlines (paradigmNames l)
                   (Just "paradigms")         -> prDictionary $ apply_encoding l flags (dictionary [f xs | (p,(xs,f)) <- Map.toList (paradigms l)])
                   (Just "paradigms_compact") -> prParadigmsCompact $ apply_encoding l flags (dictionary [f xs | (p,(xs,f)) <- Map.toList (paradigms l)])
                   (Just "paradigms_latex") -> prLatex $ apply_encoding l flags (dictionary [f xs | (p,(xs,f)) <- Map.toList (paradigms l)])
                   (Just "tagset")    -> prTagset l    $ apply_encoding l flags (dictionary [f xs | (p,(xs,f)) <- Map.toList (paradigms l)])
                   (Just "paradigms_list") -> "[" ++ (concat (intersperse ", " ['\"' : p ++ "\"" | (p,_) <- Map.toList (paradigms l)])) ++ "]"
                   (Just "paradigms_plist") -> unlines ["# -*- coding: utf-8 -*-",
                                                        "paradigms = '[" ++ (concat (intersperse "," ['\"' : p ++ "\"" | (p,_) <- Map.toList (paradigms l)])) ++ "]'"]
                   (Just "pos_plist") -> unlines ["# -*- coding: utf-8 -*-",
                                                  "pos = '[" ++ (concat (intersperse "," (nub ['\"' : (get_pos (f xs)) ++ "\"" | (p,(xs,f)) <- Map.toList (paradigms l)]))) ++ "]'"]
                   (Just "extract")   -> prExtract l
                   (Just "compound")  -> prCompound l
                   (Just "newlex")    -> prNewDictionary d
                   --  (Just "json")    -> prJSON d
	           (Just "lex")     -> prJSON d --prFullFormLex (dict2fullform d False)
	           (Just "tabbedlex") -> prTabbedLex d
                   (Just "webservice") -> prWebService d
                   (Just "tagtab") -> prWordTaglist d
	           (Just "words")   -> prWordlist (dict2fullform d)
	           (Just "tables")  -> prDictionary d
	           (Just "gf")      -> "-- machine-generated GF file\n\n" ++
		                        "include " ++ (gfTypes l) ++ " ;\n\n" ++ 
		                         (prGF d)
--	           (Just "gfr")     ->  "-- machine-generated GF file\n\n" ++
--	           	                 "include " ++ (gfTypes l) ++ " ;\n\n" ++ 
--	            	                   prGFRes d
    	           --(Just "latex")      -> prLatex d
	           (Just "xml")        -> prXML d
                   (Just "clex")       -> prCLEX d
                   (Just "sfst")       -> prSFST d
                   (Just "sfstheader") -> prSFSTHEAD d
                   (Just "sfstlex")    -> prSFSTLEX d
	           (Just "lexc")       -> prLEXC d
	           (Just "xfst")       -> prXFST d
                   (Just "hundict")    -> prHunDict d
                   (Just "hunaffix")   -> prHunAffix d
	           (Just "sql")        -> prSQL d
                   (Just "lmf")        -> prLMF (name l) d
                   (Just "rdf")        -> prRDF (name l) d
                   (Just  x)           -> error $ "unknown printer: " ++ x
                   Nothing              -> error $ "Internal error. This is a bug."
           fs -> do build_trie l fs d
                    prStatistics l n
		    run l tokS (analysis l cmode (composition l) (sandhi_rules l)) []

build_trie :: Language l => l -> [Flag] -> Dictionary -> IO ()
build_trie l fs d = CTrie.buildTrieDict (isComp l) d False

data Stats = Stats {
		    totalWords :: Int,
		    coveredWords :: Int
		   }

initStats :: Stats
initStats = Stats { totalWords = 0, coveredWords = 0 }

posify :: Language a => a -> (String -> [Tok]) -> (String -> [[String]]) -> IO String
posify l lexer f = do 
  s' <- hGetContents stdin
  let ts  = lexer s'
  --    tss = get_sentences ts
  return $ (unlines (map anapos ts)) ++ "\n" 
 where 
  -- printResult [] = []
  --printResult xs = unlines xs -- "{" ++ (unwords xs) ++ "}\n"
  anapos t = 
   case t of
    BL     -> []
    (P s)  -> s ++ "\t" ++ s ++ ":spec" -- "(\"" ++ esc s ++ "\",spec)"
    (PD s) -> s ++ "\t" ++ s ++ ":num/spec" -- "(\"" ++ esc s ++ "\",num/spec)"
    (D s) ->  s ++ "\t" ++ s ++ ":num" --"(\"" ++ esc s ++ "\",num)"
    (W s) ->  case f s of
                [] -> s -- "(\"" ++ esc s ++ "\",)"
                xs -> s ++ "\t" ++ prResult xs -- "(\"" ++ esc s ++ "\"," ++ prResult xs ++ ")"
    (A (u,l)) -> case (f u) ++ (f l) of
                  [] -> u -- "(\"" ++ esc u ++ "\",)"
                  xs -> u ++ "\t" ++ prResult xs -- "(\"" ++ esc u ++ "\"," ++ prResult xs ++ ")"
    (AA (u,m,l)) -> case (f u) ++ (f m) ++ (f l) of
                     [] -> u -- "(\"" ++ esc u ++ "\",)"
                     xs -> u ++ "\t" ++ prResult xs -- "(\"" ++ esc u ++ "\"," ++ prResult xs ++ ")"
  prResult :: [[String]] -> String
  prResult xs  = concat $ intersperse "\t" $ filter (not.null) (map filter_analysis xs)
--  esc [] = []
--  esc ('\"':xs) = '\\':'\"':esc xs
--  esc (x:xs) = x:esc xs
  filter_analysis :: [String] -> String
  filter_analysis [s] = get_head s ++ ":" ++  (unwords $ filter (not.null) $ (get_pos s):(get_inhs s):[get_param s])
  filter_analysis _   = []
  get_head s@(x:xs)
   | isPrefixOf "id\":" s = case span (/= '\"') (drop 5 s) of
                              (r,_) -> r
   | otherwise             = get_head xs
  get_inhs s@(x:xs)
   | isPrefixOf "inhs\":" s = case span (/= ']') (drop 7 s) of
                               (r,_) -> case filter (\c -> c /= '"' && c /= ',') r of
                                          [] -> []
                                          s  | elem '*' s -> []
                                          s               -> s  
   | otherwise             = get_inhs xs
  get_pos s@(x:xs)
   | isPrefixOf "pos\":" s = case span (/= '\"') (drop 6 s) of
                               (r,_) | elem '*' r -> []
                               (r,_) -> r
   | otherwise             = get_pos xs
  get_param s@(x:xs)
   | isPrefixOf "param\":" s = case span (/= '\"') (drop 8 s) of
                               (r,_) | elem '*' r -> []
                               (r,_) -> r
   | otherwise             = get_param xs

get_sentences :: [Tok] -> [[Tok]]
get_sentences xs = gets xs []
   where gets [] s = [reverse s]
         gets (c:cs) s
          | isMajor c = (reverse (c:s)):gets cs []
          | otherwise = gets cs (c:s)

isMajor (P [c]) = elem c ".?!"
isMajor _     = False

run :: Language a => a -> (String -> [Tok]) -> (String -> [[String]]) -> String -> IO ()
run l t f "fail"      = run' l t f NoAnalysis        initStats >> return ()
run l t f "eval"      = run' l t f AnaEval initStats >> return ()
run l t f "lexfail"   = run' l t f FilterLexiconNo   initStats >> return ()
run l t f "nocomp"    = run' l t f FilterLexiconNoComp initStats >> return ()
run l t f "lexcomp"   = run' l t f FilterLexiconComp initStats >> return ()
run l t f _           = do
                          st <- run' l t f Normal initStats
		          prErr $ "Total words:   " ++ show (totalWords st)
		          prErr $ "Covered words: " ++ show (coveredWords st)

run' :: Language a => a -> (String -> [Tok]) -> (String -> [[String]]) -> AnaType -> Stats -> IO Stats
run' l tokenizer f a st = 
 do b <- hIsEOF stdin
    if b then return st 
     else do 
       s <- hGetLine stdin
       analyze l a f (tokenizer s) st >>= run' l tokenizer f a

word_tokens :: [Tok] -> [String]
word_tokens xs = [s | (W s) <- xs]

analyze :: Language a => a -> AnaType -> (String -> [[String]]) -> [Tok] -> Stats -> IO Stats
analyze _ _ _  [] st = return st
analyze l t f (s:ss) st =
   case s of
    BL    -> analyze l t f ss st
    (P s) ->   
        do case t of 
	    Normal    
             -> do prText $ "{\"" ++ s ++ "\":\"-Symb-\"}"
		   analyze l t f ss st
            _ -> analyze l t f ss st
    (PD s) ->   
        do case t of 
	    Normal
             -> do prText $ "{\"" ++ s ++ "\":\"-Num/Symb-\"}"
		   analyze l t f ss st
            _ -> analyze l t f ss st
    (D s) ->
        do case t of 
            Normal
             -> do prText $ "{\"" ++ s ++ "\":\"-Num-\"}"
		   analyze l t f ss st
	    _ -> analyze l t f ss st
    (W s) ->  
	case f s of
        [] -> do prText $ prAnalysis l t s []
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st 
			            })
        xs -> do prText $ prAnalysis l t s xs
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st + 1 
			            })
    (A (s,ls)) ->  
	case (f s) ++(f ls) of
        [] -> do prText $ prAnalysis l t s []
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st 
			            })
        xs -> do prText $ prAnalysis l t s xs
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st + 1 
			            })
    (AA (s,m,ls)) ->  
	case (f s) ++ (f m) ++(f ls) of
        [] -> do prText $ prAnalysis l t s []
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st 
			            })
        xs -> do prText $ prAnalysis l t s xs
	         analyze l t f ss (st {
			             totalWords = totalWords st + 1,
			                          coveredWords = coveredWords st + 1 
			            })

prText [] = return ()
prText s  = hPutStrLn stdout s

prAnalysis :: Language a => a -> AnaType -> String -> [[String]] -> String
prAnalysis l Normal   s   [] = case wordGuesser l s of
                                 (x:xs) ->  concat [concat ["{\"", s, "\":[\"-Guess-\",\n"],
                                                    prA l (map (:[]) (x:xs)),
                                                    "\n}"]
                                 []     -> concat ["{\"", s, "\":\"-Unknown-\"}"]
prAnalysis l Normal     s xs    = concat [concat ["{\"", s, "\":{\n"], prA l xs, "\n}}"]
prAnalysis _ NoAnalysis s []    = s
prAnalysis _ NoAnalysis s _       = []
-- force evaluation without printing. 
prAnalysis _ AnaEval s xs 
 | length (["" | x <- xs,"" <- x]) == 1  = s 
 | otherwise                             = []
prAnalysis _ FilterLexiconNo s xs = case [ x | [x] <- xs] of
                                      (_:_) -> []
                                      _     -> s
prAnalysis _ FilterLexiconNoComp s xs  = case [x | [x] <- xs] of
                                           ys | length xs == length ys -> s
                                           _  -> [] 
prAnalysis _ FilterLexiconComp s xs  = case [x | (x:y:_) <- xs] of
                                         ys | length xs == length ys -> s
                                         _  -> [] 

prA l xs =  concat $ intersperse ",\n" (map pr (annotate 1 1 (sort_length (affixes l) xs)))

annotate sn cn       [] = []
annotate sn cn ([]:xs)  =   (0,[]):annotate       sn cn xs
annotate sn cn ([x]:xs) = (sn,[x]):annotate (sn+1) cn xs
annotate sn cn (xs:ys)  =  (cn,xs):annotate sn (cn+1) ys

pr (_,[])  = []
pr (n,[x]) = "\"s_" ++ (show n) ++ "\":" ++ x
pr (n,xs) = str ++ (concat (intersperse (",\n" ++ pad) xs)) ++ "]"
   where str = "\"c_" ++ (show n) ++ "\":[" 
         pad   = take (length str) (repeat ' ')
    
welcome :: Language a => a -> String
welcome l = (morphology_header l) ++
            "\n includes FM 3.0 (M. Forsberg & A. Ranta, 2010, under GNU GPL)" 
