--# -path=.:../scandinavian:../abstract:../common

concrete CompatibilitySwe of Compatibility = CatSwe ** open Prelude, CommonSwe in {

-- from Noun 19/4/2008

lin
    NumInt n = {s = \\_ => n.s ; isDet = True ; n = Pl} ;
    OrdInt n = {s = n.s ++ ":e" ; isDet = True} ;

}
