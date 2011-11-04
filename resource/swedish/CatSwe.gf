-- concrete CatSwe of Cat = CommonX -[Tense,Temp] 
-- -- ** CatScand 
-- with 
--   (ResScand = ResSwe)
--   ** 
--  open TenseS, (R = ParamX) in
--   {
--   lincat 
--  Temp  = {s : Str ; t : STense ; a : R.Anteriority} ;
--  Tense = {s : Str ; t : STense} ;
--  };

concrete CatSwe of Cat = CommonX -[ Tense,Temp] ** CatScand with
  (ResScand = ResSwe) ;
--  concrete FoodsEng of Foods = FoodsI - [Pizza] with 
--        (Syntax = SyntaxEng),
--              (LexFoods = LexFoodsEng) ** 
--                      open SyntaxEng, ParadigmsEng in {
--                        
--                              lin Pizza = mkCN (mkA "Italian") (mkN "pie") ;
--                                  }n
