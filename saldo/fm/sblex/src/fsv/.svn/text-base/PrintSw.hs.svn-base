module PrintSw where

import List(intersperse,nub,inits)
import Util
import Dictionary
import General
import Char
import Data.Bits ((.&.))

print_table = [("multi",multi_printer),("rdf_sb",prRDF_sb),("flat_xml",prFlatXML)]

-- | Print JSON
multi_printer :: Dictionary -> String
multi_printer = concat . nub . concat . map prOne . filter is_multi. unDict
 where prOne  (id,stem,para,cat,inhs,tbl,_) = 
           [conc [p "word" x, p "head" stem, p "pos" cat, p "param" "frag", pl "inhs" inhs, p "id" id, p "p" para, p "attr" (show attr)] |  (a,(attr,s)) <- (merge tbl), str <- unStr s, length (words str) > 1, x <- map unwords $ gen (words str)]
       merge            [] = []
       merge           [x] = [x]
       merge (x@(param1,(a,s)):y@(param2,(a',s1)):xs) = 
           case (number param1, number param2) of
             ((v,n1,n2),(v',n1',n2')) | v == v' && n1' > n1 -> merge ((param2,(a',mkStr (unwords (unStr s ++ unStr s1)))):xs)
             _ -> x:merge (y:xs)
       number p = case map (map (read :: String -> Int) . words . map f) (filter (isDigit.head) (words p)) of
                    [[v1,n1,n2]] -> (v1,n1,n2)
       f x
         | elem x "-:" = ' ' 
         | otherwise = x
       gen :: [String] -> [[String]]
       gen []     =  []
       gen (x:xs) =  filter (\xs -> not (null xs || one xs)) (inits (x:xs)) ++ gen xs
       one [_] = True
       one _ = False
       pl s []   = quote s ++ ":" ++ "[]"
       pl s xs   = quote s ++ ":[" ++ (concat (intersperse "," (map quote xs))) ++"]"
       p s1 s2 = quote s1 ++ ":" ++ quote s2
       conc xs = '{':(concat (intersperse "," xs)) ++ "}\n"
       is_multi (_,stem,para,cat,_,_,_) = length (words stem) > 1

-- | Print RDF
prRDF_sb :: Dictionary -> String
prRDF_sb d =  unlines [
  "<?xml version='1.0' encoding='UTF-8'?>",
  "<rdf:RDF",
  "  xmlns=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
  "  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
  "  xmlns:sb=\"http://spraakbanken.gu.se/saldo#\">",
  (concat (map pr (unDict d))),
  "</rdf:RDF>"]
 where
 pr (id,stem,para,cat,inhs,tbl,extr) =  
     concat 
         [
          "   <rdf:Description rdf:about=\"http://spraakbanken.gu.se/saldo/lemma/" ++ id ++ "\">\n",
          "    <rdf:type rdf:resource=\"http://spraakbanken.gu.se/ns/saldo/lemma\"/>\n",
          "    <sb:cf>"  ++ amp stem ++ "</sb:cf>\n",
          "    <sb:p>" ++ para ++ "</sb:p>\n",
          "    <sb:pos>"   ++ cat ++ "</sb:pos>\n",
          concat ["    <sb:inhs>"  ++ i ++ "</sb:inhs>\n" | i <- inhs] ++ "   </rdf:Description>\n",
          concat [unlines ["   <rdf:Description rdf:about=\"http://spraakbanken.gu.se/saldo/lemma/" ++ id ++ "#" ++ (urlEncode p) ++ "\">",
                           "    <rdf:type rdf:resource=\"http://spraakbanken.gu.se/ns/saldo/msd\"/>",
          "    <sb:id rdf:resource=\"http://spraakbanken.gu.se/saldo/lemma/" ++ id ++ "\"/>",
          "    <sb:msd>" ++ p ++ "</sb:msd>",
          concat (intersperse "\n" ["    <sb:wf>" ++ (amp w) ++ "</sb:wf>" | w <- unStr ws]),
          "   </rdf:Description>"] | (p,(_,ws)) <- tbl, not (null (unStr ws))]
          ]


-- | Print RDF
prFlatXML :: Dictionary -> String
prFlatXML d =  unlines [
  "<?xml version='1.0' encoding='UTF-8'?>",
  "<!-- $Id$ -->",
  "<Lexicon>",
  (concat (map pr (unDict d))),
  "</Lexicon>"]
 where
 pr (id,stem,para,cat,inhs,tbl,extr) =  
     concat 
         [
          "   <LexicalEntry>\n",
          "    <eid>" ++ id ++ "</eid>\n",
          "    <gf>"  ++ amp stem ++ "</gf>\n",
          "    <p>" ++ para ++ "</p>\n",
          "    <pos>"   ++ cat ++ "</pos>\n",
          pr_inhs inhs,
          "    <table>\n",
          concat (intersperse "\n" ["     <form><param>" ++ p ++ "</param><wf>" ++ (amp w) ++ "</wf></form>" | (p,(_,ws)) <- tbl, w <- unStr ws]),
          "\n    </table>\n",
          "   </LexicalEntry>\n"
          ]
 pr_inhs [] = "    <inhs>-</inhs>\n"
 pr_inhs is = "    <inhs>" ++ (unwords is) ++ "</inhs>\n"

 -- copied from Module :  HTTP
 -- Copyright :  (c) Warrick Gray 2002
 -- License   :  BSD
amp = concat . map (\c -> if c == '&' then "&amp;" else [c])
urlEncode (h:t) =
     let str = if reserved (ord h) then escape h else [h]
     in str ++ urlEncode t
urlEncode [] = []
reserved x
     | x >= ord 'a' && x <= ord 'z' = False
     | x >= ord 'A' && x <= ord 'Z' = False
     | x >= ord '0' && x <= ord '9' = False
     | x <= 0x20 || x >= 0x7F = True
     | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                    ,'=','+',',','$','{','}'
                                    ,'|','\\','^','[',']','`'
                                    ,'<','>','#','%','"']
escape x = 
     let y = ord x 
      in [ '%', intToDigit ((y `div` 16) .&. 0xf), intToDigit (y .&. 0xf) ]

