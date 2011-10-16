module Combine (combine,untab) where

import System
import List

combine :: String -> String -> String
combine s1 s2 = unlines $ create_output (pp s1) (pp s2)
 where pp = filter (not.null) . map untab . lines
  
create_output :: [[String]] -> [[String]] -> [String]
create_output [] [] = []
create_output ([]:xss) ([]:yss) = "" : create_output xss yss
create_output (([x]):xss) ([_,tag]:yss) = tab [x,"#:" ++ tag] : create_output xss yss
create_output ((x:[y]):xss) ([x2,tag]:yss) = if (x == x2) then tab [x,y] : create_output xss yss else error $ "data out of sync: " ++ x ++ " " ++ x2
create_output ((x:xs):xss) ([x2,tag]:yss) =
 if (x==x2) then 
  let analyses = map split_at_colon xs in
   case reduce tag analyses of
     []        ->  pr_ambi x analyses   : create_output xss yss
     [(w,t)]   -> tab [x,w ++ ":" ++ t] : create_output xss yss
     analyses' -> pr_ambi x analyses'   : create_output xss yss
 else error $ "data out of sync: " ++ x ++ " " ++ x2
create_output x y = error $ "invalid format of input data." ++ (show x ++ show y)

reduce :: String -> [(String,String)] -> [(String,String)]
reduce tag xs = reduce' xs 0
 where reduce'  [] _ = []
       reduce' [x] _ = [x]
       reduce' xs n
        | length tag < n = xs
        | otherwise = case [ (x,t) | (x,t) <- xs, safe_index tag n == safe_index t n] of
                        [] -> xs
                        ys -> reduce' ys (n+1)
       safe_index s n = if (length s > n) then Just (s !! n) else Nothing

pr_ambi :: String -> [(String,String)] -> String
pr_ambi w xs = tab $ w : (map pr (grouping [(b,a) | (a,b) <- grouping [([a],[b]) | (a,b) <- sort xs]]))
 where pr (xs,as)      = (conc_char '+' as) ++ ":" ++ (conc_char '+' xs) 
       conc_char c ss = concat $ intersperse ([c]) ss
       
grouping :: (Eq a,Eq b) => [([a],[b])] -> [([a],[b])]
grouping xs = [(as,nub bs) | (as,bs) <- grouping' xs]
 where
   grouping' []  = []
   grouping' [x] = [x]
   grouping' ((i,t):(i2,t2):xs)
    | i == i2   = grouping' ((i,nub (t++t2)):xs)
    | otherwise = (i,t): grouping' ((i2,t2):xs)

split_at_colon :: String -> (String,String)
split_at_colon s = case span (/= ':') s of
          (w,(_:t)) -> (w,t)

tab :: [String] -> String
tab = concat . intersperse "\t"

untab :: String -> [String]
untab [] = []
untab xs = case span (/='\t') xs of
             (w,(_:ys)) -> w:untab ys
             (w,[]) -> [w]
