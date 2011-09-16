--# -path=.:swedish/:scandinavian:common:abstract:prelude

concrete LexiconExtSwe of LexiconExt =  DictSwe **
   open  Prelude, CommonScand, ParadigmsSwe, GuessSwe, IrregSwe in {

flags 
  optimize=values ;

lin

  AA_PN = regPN "noname" ;
  NN_N  = mkN "nonoun""nonoun""nonoun""nonoun";
  VV_V = mkV "noverb" "noverb""noverb""noverb""noverb""noverb" ;
  AJ_A = mkA "noadjective" "noadjective""noadjective""noadjective""noadjective""noadjective""noadjective";

  do_V2 = dirV2 (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord" );
  seek_V2 = dirV2 (mk2V "söka" "sökte") ;
  aldrig_AdV = ss "aldrig" ;
  oavsett_Prep = ss "oavsett" ;

-- from Test Lexicon
  die_V = (mkV "dö" "dör" "dö" "dog" "dött" "dödd") ; ----
  fear_VS = mkVS (regV "fruktar") ;
  hear_V2 = dirV2 (mkV "höra" "hör" "hör" "hörde" "hört" "hörd") ;
  know_VQ = mkVQ (mkV "veta" "vet" "vet" "visste" "vetat" "visst") ;
  know_VS = mkVS (mkV "veta" "vet" "vet" "visste" "vetat" "visst") ;
  say_VS = mkVS (mkV "säga" "säger" "säg" "sade" "sagt" "sagd") ;
  sleep_V = (irregV "sova" "sov" "sovit") ;
  star_N = regGenN "stjärna" utrum ;
  watch_V2 = mkV2 (regV "titta") (mkPrep "på") ;
  ashes_N = mk2N "aska" "askor" ;
  blood_N = mk2N "blod" "blod" ;
  flower_N = mk2N "blomma" "blommor" ;
  fog_N = mk2N "dimma" "dimmor" ;
  heart_N = mkN "hjärta" "hjärtat" "hjärtan" "hjärtana" ;
  meat_N = regGenN "kött" neutrum ;
  nose_N = mk2N "näsa" "näsor" ;
  sand_N = mk2N "sand" "sander" ;
  snow_N = mkN "snö" "snön" "snöer" "snöerna" ;
  blow_V = mk2V "blåsa" "blåste" ;
  count_V2 = dirV2 (regV "räkna") ;
  dig_V = mk2V "gräva" "grävde" ;
  freeze_V = irregV "frysa" "frös" "frusit" ;
  hunt_V2 = dirV2 (regV "jaga") ;
  laugh_V = regV "skratta" ;
  lie_V = irregV "ligga" "låg" "legat";
  push_V2 = dirV2 (mk2V "trycka" "tryckte") ;
  sit_V = irregV "sitta" "satt" "suttit" ;
  spit_V = regV "spotta" ;
  stand_V = irregV "stå" "stod" "stått" ;
  think_V = mk2V "tänka" "tänkte" ;
  throw_V2 = dirV2 (regV "kasta") ;
  turn_V = irregV "vända" "vände" "vänt" ;
  vomit_V = mk2V "spy" "spydde" ;
  wash_V2 = dirV2 (regV "tvätta") ;
  left_Ord = {s = "vänstra" ; isDet = True} ;
  right_Ord = {s = "högra" ; isDet = True} ;

  breathe_V = depV (regV "anda") ;
  vaegra_VV = mkVV (regV (guess "vägrar"));
  anvaenda_V2 = dirV2 (regV (guess "används"));
  arbeta_V = regV (guess "arbetar");
  behoeva_V2 = dirV2 (regV (guess "behövs"));
  bo_V = regV "bor" ;
  boerja_VV = mkVV (regV (guess "börjar"));
  faa_V2    = dirV2 (irregV "få" "fick" "fått") ;
  faa_VV    = (irregV "få" "fick" "fått") ** {c2 = mkComplement []; lock_VV = <>};
  spela_V2 = dirV2 (regV (guess "spelar")) ;

-- from Irreg, with correct constructs..
  angiva_V2 = dirV2 angiva_V ;
  avhugga_V2 = dirV2 avhugga_V ;
  avsitta_V2 = dirV2 avsitta_V ;
  avstiga_V2 = dirV2 avstiga_V ;
  bedraga_V2 = dirV2 bedraga_V ;
  bidraga_V2 = mkV2 bidraga_V (mkPrep "till") ;
  bliva_V2 = dirV2 bliva_V ;
  draga_V2 = dirV2 draga_V ;
  drypa_V2 = mkV2 drypa_V (mkPrep "av") ;
  duga_V = IrregSwe.duga_V ;
  dyka_V = IrregSwe.dyka_V ;
  erfara_V2 = dirV2 erfara_V ; -- lägg till jag erfar att ..
  faanga_V2 = dirV2 fånga_V ;
  -- företaga_V = företa sig ngt
  giva_V3 = mkV3 giva_V (mkPrep "till");
  giva_dat_V3 = mkV3 giva_V; 
  graata_V = gråta_V ;
  iakttaga_V2 = dirV2 iakttaga_V ;
  intaga_V2 = dirV2 intaga_V ;
  kaenna_V2 = dirV2 känna_V ;
  laata_V2V = dirV2V låta_V noPrep [] ;
  laegga_V2 = dirV2 lägga_V ;
  oevergiva_V2 = dirV2 övergiva_V ;
  se_V = IrregSwe.se_V ;
  se_V2 = dirV2 IrregSwe.se_V ;
  skrika_V = IrregSwe.skrika_V ;
  slippa_VV = mkVV slippa_V ;
  saetta_V2 = dirV2 sätta_V ;
  uppgiva_V2 = dirV2 uppgiva_V ; -- lägg til jag uppgav att ..

 oper 
  dirV2V : V -> Prep -> Str -> V2V ;
  dirV2V v p1 p2 = mmkV2 v p1 ** {c3 = mkComplement p2 ; lock_V2V = <>} ;


} ;
