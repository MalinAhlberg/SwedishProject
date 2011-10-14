module Multiwords where

import Frontend
import Dictionary
import List
import General
import Attr

multiwords :: Dictionary -> Dictionary
multiwords = dictionary . map process_entry . unDict
 where process_entry (id,stem,para,cat,inhs,tbl,e) = 
           (id,stem,para,cat,inhs,
              if (length (words stem) > 1) then split_multiwords tbl else tbl,e) 
       split_multiwords tbl = concat [frag_seq param a pos mw | (param,(a,str)) <- tbl, 
                                                               (mw,pos) <- zip (unStr str) [1..]]
       hole = Nothing
       frag_seq param a var s =
           let ws  = words s
               wsl = length ws
               f n = case hole of
                       Nothing -> ""
                       Just position -> if n < position then "pre" else "post"
            in [(unwords $ filter (not.null) [param,f n, show var ++ ":" ++ show n ++ "-" ++ show wsl],
                 (w_attr,mkStr fragment)) | (n,fragment) <- zip [1..] ws]
