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

  do_V2 = dirV2 (mkV "g�ra" "g�r" "g�r" "gjorde" "gjort" "gjord" );
  seek_V2 = dirV2 (mk2V "s�ka" "s�kte") ;
  aldrig_AdV = ss "aldrig" ;
  oavsett_Prep = ss "oavsett" ;

-- from Test Lexicon
  die_V = (mkV "d�" "d�r" "d�" "dog" "d�tt" "d�dd") ; ----
  fear_VS = mkVS (regV "fruktar") ;
  hear_V2 = dirV2 (mkV "h�ra" "h�r" "h�r" "h�rde" "h�rt" "h�rd") ;
  know_VQ = mkVQ (mkV "veta" "vet" "vet" "visste" "vetat" "visst") ;
  know_VS = mkVS (mkV "veta" "vet" "vet" "visste" "vetat" "visst") ;
  say_VS = mkVS (mkV "s�ga" "s�ger" "s�g" "sade" "sagt" "sagd") ;
  sleep_V = (irregV "sova" "sov" "sovit") ;
  star_N = regGenN "stj�rna" utrum ;
  watch_V2 = mkV2 (regV "titta") (mkPrep "p�") ;
  ashes_N = mk2N "aska" "askor" ;
  blood_N = mk2N "blod" "blod" ;
  flower_N = mk2N "blomma" "blommor" ;
  fog_N = mk2N "dimma" "dimmor" ;
  heart_N = mkN "hj�rta" "hj�rtat" "hj�rtan" "hj�rtana" ;
  meat_N = regGenN "k�tt" neutrum ;
  nose_N = mk2N "n�sa" "n�sor" ;
  sand_N = mk2N "sand" "sander" ;
  snow_N = mkN "sn�" "sn�n" "sn�er" "sn�erna" ;
  blow_V = mk2V "bl�sa" "bl�ste" ;
  count_V2 = dirV2 (regV "r�kna") ;
  dig_V = mk2V "gr�va" "gr�vde" ;
  freeze_V = irregV "frysa" "fr�s" "frusit" ;
  hunt_V2 = dirV2 (regV "jaga") ;
  laugh_V = regV "skratta" ;
  lie_V = irregV "ligga" "l�g" "legat";
  push_V2 = dirV2 (mk2V "trycka" "tryckte") ;
  sit_V = irregV "sitta" "satt" "suttit" ;
  spit_V = regV "spotta" ;
  stand_V = irregV "st�" "stod" "st�tt" ;
  think_V = mk2V "t�nka" "t�nkte" ;
  throw_V2 = dirV2 (regV "kasta") ;
  turn_V = irregV "v�nda" "v�nde" "v�nt" ;
  vomit_V = mk2V "spy" "spydde" ;
  wash_V2 = dirV2 (regV "tv�tta") ;
  left_Ord = {s = "v�nstra" ; isDet = True} ;
  right_Ord = {s = "h�gra" ; isDet = True} ;

  breathe_V = depV (regV "anda") ;
  vaegra_VV = mkVV (regV (guess "v�grar"));
  anvaenda_V2 = dirV2 (regV (guess "anv�nds"));
  arbeta_V = regV (guess "arbetar");
  behoeva_V2 = dirV2 (regV (guess "beh�vs"));
  bo_V = regV "bor" ;
  boerja_VV = mkVV (regV (guess "b�rjar"));
  faa_V2    = dirV2 (irregV "f�" "fick" "f�tt") ;
  faa_VV    = (irregV "f�" "fick" "f�tt") ** {c2 = mkComplement []; lock_VV = <>};
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
  erfara_V2 = dirV2 erfara_V ; -- l�gg till jag erfar att ..
  faanga_V2 = dirV2 f�nga_V ;
  -- f�retaga_V = f�reta sig ngt
  giva_V3 = mkV3 giva_V (mkPrep "till");
  giva_dat_V3 = mkV3 giva_V; 
  graata_V = gr�ta_V ;
  iakttaga_V2 = dirV2 iakttaga_V ;
  intaga_V2 = dirV2 intaga_V ;
  kaenna_V2 = dirV2 k�nna_V ;
  laata_V2V = dirV2V l�ta_V noPrep [] ;
  laegga_V2 = dirV2 l�gga_V ;
  oevergiva_V2 = dirV2 �vergiva_V ;
  se_V = IrregSwe.se_V ;
  se_V2 = dirV2 IrregSwe.se_V ;
  skrika_V = IrregSwe.skrika_V ;
  slippa_VV = mkVV slippa_V ;
  saetta_V2 = dirV2 s�tta_V ;
  uppgiva_V2 = dirV2 uppgiva_V ; -- l�gg til jag uppgav att ..

 oper 
  dirV2V : V -> Prep -> Str -> V2V ;
  dirV2V v p1 p2 = mmkV2 v p1 ** {c3 = mkComplement p2 ; lock_V2V = <>} ;


} ;
