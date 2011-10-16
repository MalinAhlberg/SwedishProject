module DictSw where

import BuildSw
import RulesSw
import TypesSw
import General
import Dictionary

swedishDict :: Dictionary
swedishDict = dictionary []

-- closedLexicon

closedLexicon :: [Entry]
closedLexicon = concat 
		[
  		 prepositions,
                 adverbials,		 
   		 conjunctions,
   		 subjunctions,
   		 particles,
		 pronounsPN,
		 pronounsAdj,
		 [
		  entry artIndef,
		  entry artDef,
		  infMark "att"
		 ],
                 auxiliaryVerbs,
		 numerals 
		]

pronounsPN :: [Entry]
pronounsPN = 
    [
     entry pronRefl,
     pronJag "jag" "mig" "min" "mitt",
     pronJag "du"  "dig" "din" "ditt", 
     pronHan "han" "honom" "hans",     
     pronHan "hon" "henne" "hennes",   
     pronJag "vi"  "oss" "vår" "vårt", 
     pronJag "ni"  "er"  "er" "ert",
     pronHan "de"  "dem"  "deras", 
     pronHan "den" "den" "dess", 
     pronHan "det" "det" "dess",
     pronHan "vem" "vem" "vems"
    ]

pronounsAdj = 
    [
     pronDylik "all",
     pronDylik "dylik",
     pronDylik "hurdan",
     pronDylik "likadan",
     pronDylik "sådan",
     pronNagon "egen" "eget" "egna",
     pronNagon "denna" "detta" "dessa",
     pronNagon "annan" "annat" "andra",
     pronNagon "ingen" "inget" "inga",
     pronNagon "vilken" "vilket" "vilka",
     pronNagon "mången" "månget" "många",
     pronNagon "någon" "något" "några"
    ]


prepositions 
     = map prep
	[
       "alltsedan",
       "apropå",
       "av",
       "bakom",
       "bland",
       "bortom",
       "bortåt",
       "brevid",
       "efter",
       "emellan",
       "emot",
       "enligt",
       "framför",
       "från",
       "för",
       "förbi",
       "före",
       "förutom",
       "genom",
       "gentemot",
       "hos",
       "härom",
       "i",
       "ifrån",
       "igenom",
       "inemot",
       "inför",
       "inklusive",
       "innan",
       "innanför",
       "inom",
       "intill",
       "invid",
       "jämte",
       "kontra",
       "kring",
       "längs",
       "med",
       "medelst",
       "mellan",
       "mot",
       "nedanför",       
       "om",
       "omkring",
       "ovan",
       "ovanför",
       "ovanpå",
       "per",
       "på",
       "runt",
       "sedan",
       "sen",
       "till",
       "trots",
       "undan",
       "under",
       "ur",
       "utan",
       "utanför",
       "utanpå", 
       "utefter",
       "utför",
       "utifrån",
       "utmed",
       "utom",
       "utöver",
       "via",
       "vid",
       "visavi",
       "å", 
       "åt",
       "över"
       ]

conjunctions = map conj $ words	
  "och samt respektive liksom fast men utan eller ty för så både"

subjunctions = map subj $ words
	       "att förrän innan medan sedan tills eftersom allteftersom emedan för så bara blott ifall om såvida liksom som såsom än ehuru fast fastän oaktat"

particles = map part $ words
   "emot fast in ut upp ner fram åter" -- plus prepositions

auxiliaryVerbs = map (\ (x, y, z, u) -> entry (auxVerbGen x y z u)) [
  (mkStr "vilja", mkStr "vill", mkStr "ville",mkStr "velat"),
  (mkStr "kunna", mkStr "kan",  mkStr "kunde",mkStr "kunnat"),
  (nonExist,      mkStr "måste",mkStr "måste",mkStr "måst"),
  (mkStr "skola", strings ["ska","skall"], mkStr "skulle",nonExist),
  (mkStr "böra",  mkStr "bör", mkStr "borde",mkStr "bort")
  ]

-- -- not really closed
adverbials = map adverbInv $ words 
  "dessutom heller också även dock ändå alltså väl nere ute inne"

-- from numerals.Swe.gf
numerals = [] -- map numeralInv $ words "arton aderton elva ett fem femtio femton fjorton fyra fyrtio hundra nio nittio nitton sex sextio sexton sju sjuttio sjutton tio tjugo tolv tre trettio tretton tusen två åtta åttio"

