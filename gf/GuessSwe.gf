resource GuessSwe =
 open
   Prelude in {

-- Needed for lexicon produced by the Verb Guesser

oper guess : Str -> Str = \v -> case v of {
  rit + ("ats"|"ades"|"ade"|"at")           => rit+"a" ;
  kok+"ande"                               => kok+"a" ;
  le+"ende"                                => le+"r" ; 
  lek + ("d"|"t") +("e"|"es")              => lek+"er" ;  
  ro + ("dde"|"tt")                        => ro+"r" ;
  ring +"t"                                => ring+"er" ;
  het +er@("er"|"ar")                      => het+er ;
  b+e@("ä"|"å"|"e"|"o"|"y") + ("r" | "rs") => v ; -- stör?
  bo                                       => bo
    };
}

  
