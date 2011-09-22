module Simplify where
transl =  
 [("++","och")
 ,("++EL","eller")
 ,("++KS","så")
 ,("++KU","så")
 ,("++MN","men")
 ,("++OC","och") -- så, för,dvs
 ,("AB","här")
 ,("ABDA","nu")
 ,("ABFA","hur")
 ,("ABJA","lika")
 ,("ABJU","desto") --finns ej i gf
 ,("ABKP","mera")
 ,("ABKS","alltså") -- finns ej i gf 
 ,("ABKU","alltså") -- finns ej i gf 
 ,("ABMN","än")
 ,("ABNA","inte")
 ,("ABOC","också")
 ,("ABRA","när")
 ,("ABSA","så")
 ,("ABSU","minst")
 ,("ABTA","fullt")
 ,("ABZA","mycket")
 ,("ABZK","slutligen") -- finns ej i gf 
 ,("AJ  HH","unga")
 ,("AJ  HHGG","ungas")
 ,("AJ  HS","unga")
 ,("AJ  SS","gul")
 ,("AJ","gul")
 ,("AJKP","gulare")
 ,("AJKPHH","yngre")
 ,("AJKPHHGG","yngres")
 ,("AJSU","gulast")
 ,("AJSUHH","yngst")
 ,("AJSUSS","yngst")
 ,("AN    GG","katts")
 ,("AN  HH","katter")
 ,("AN  SS","katt")
 ,("AN","katt")
 ,("ANDD  GG","kattens")
 ,("ANDD","katten")
 ,("ANDDSS","katten")
 ,("ANDDSSGG","kattens")
 ,("AV","vara")
 ,("AVIP","var")
 ,("AVIV","vara")
 ,("AVPK","vore")
 ,("AVPS","är")
 ,("AVPT","var")
 ,("AVSN","varit")
 ,("BVIV","bli")
 ,("BVPK","")
 ,("BVPS","blir")
 ,("BVPT","blev")
 ,("BVSN","blivit")
 ,("EN  HH","en")
 ,("EN  HHGG","ens")
 ,("EN","en")
 ,("FV","få")
 ,("FVIV","få")
 ,("FVPK","finge")
 ,("FVPS  PA","fås")
 ,("FVPS","får")
 ,("FVPT","fick")
 ,("FVSN","fått")
 ,("GVIP","gör")
 ,("GVIV  PA","göras")
 ,("GVIV","göra")
 ,("GVPS  PA","görs")
 ,("GVPS","gör")
 ,("GVPT  PA","gjordes")
 ,("GVPT","gjorde")
 ,("GVSN  PA","gjorts")
 ,("GVSN","gjort")
 ,("HV","ha")
 ,("HVIP","ha")
 ,("HVIV","ha")
 ,("HVPS","har")
 ,("HVPT","har")
 ,("HVSN","hade")
		     {- "I?">Question mark</value>
 ,("IC">Quotation mark</value>
 ,("ID    AA">Part of idiom (multi-word unit), object form</value>
 ,("ID">Part of idion (multi-word unit)</value>
 ,("IG">Other punctuation mark</value>
 ,("IG++  KR">Other punctuation mark, coordinating, correction</value>
 ,("IG++EL">Other punctuation mark, coordinating, disjunctive</value>
 ,("IG++ELKR">Other punctuation mark, coordinating, disjunctive, correction</value>
 ,("IGPR">Other punctuation mark, ?</value>
 ,("IK    KR">Comma, correction</value>
 ,("IK">Comma</value>
 ,("IK++  KR">Comma, coordinating, correction</value>
 ,("IK++">Comma, coordinating</value>
 ,("IK++EL">Comma, coordinating, disjunctive</value>
 ,("IK++ELKR">Comma, coordinating, disjunctive, correction</value>
 ,("IK++OC">Comma, coordinating, copulative</value>
 ,("IM">Infinitive marker</value>
 ,("IP">Period</value>
 ,("IPPR">Period, ?</value>
 ,("IPXX">Period, ?</value>
 ,("IQ">Colon</value>
 ,("IR">Parenthesis</value>
 ,("IS">Semicolon</value>
 ,("IS++">Semicolon, coordinating</value>
 ,("IS++ELKR">Semicolon, coordinating, disjunctive, correction</value>
 ,("IT">Dash</value>
 ,("IT++  KR">Dash, coordinating, correction</value>
 ,("IT++">Dash, coordinating</value>
 ,("IT++EL">Dash, coordinating, disjunctive</value>
 ,("IT++ELKR">Dash, coordinating, disjunctive, correction</value>
 ,("IT++OC">Dash, coordinating, copulative</value>
 ,("ITPR">Dash, ?</value>
 ,("IU">Exclamation mark</value> 
 ,("KV">The verb locution "komma att" (periphrastic future)</value>
 ,("KVIV">The verb locution "komma att" (periphrastic future), infinitive</value>
 ,("KVPS">The verb locution "komma att" (periphrastic future), present</value>
 ,("KVPT">The verb locution "komma att" (periphrastic future), preterite</value>
 ,("KVSN">The verb locution "komma att" (periphrastic future), supine</value>
                -}
 ,("MN    GG","kvinna")
 ,("MN  HH","kvinna")
 ,("MN  SS","kvinna")
 ,("MN","kvinna")
 ,("MNDD","kvinnan")
 ,("MNDDSS","kvinnan")
		{-		"MVPS">The verb "måste" (must), present</value>
 ,("MVPT">The verb "måste" (must), preterite</value> -}
 ,("NN    GG","kvinnas")
 ,("NN  HH","kvinna")
 ,("NN  HHGG","kvinnans")
 ,("NN  HS","kvinna")
 ,("NN  HSGG","kvinnas")
 ,("NN  SS","kvinna")
 ,("NN  SSGG","kvinnas")
 ,("NN","kvinna")
 ,("NNDD  GG","kvinnans")
 ,("NNDD","kvinnan")
 ,("NNDDHH","kvinnan")
 ,("NNDDHHGG","kvinnans")
 ,("NNDDHS","kvinnan")
 ,("NNDDHSGG","kvinnans")
 ,("NNDDSS","kvinnan")
 ,("NNDDSSGG","kvinnans")
 ,("NNDDTR","kvinnan")
 ,("PN    GG","Johans")
 ,("PN  HH","Johan")
 ,("PN  HHGG","Johans")
 ,("PN  SS","Johan")
 ,("PN  SSGG","Johans")
 ,("PN","Johan")
 ,("PNDD  GG","Johans")
 ,("PNDD","Johan")
 ,("PNDDHH","Johan")
 ,("PNDDSS","Johan")
 ,("PNDDSSGG","Johans")
 ,("PO","han")
 ,("POCP","varandra") --not in gf
 ,("POCPHH","varandra") --not in gf
 ,("POCPHHAA","varandra") --not in gf 
 ,("POCPHHGG","varandras") --not in gf
 ,("PODP  AA","den")
 ,("PODP  GG","dess")
 ,("PODP","den")
 ,("PODPHH","han")
 ,("PODPHHAA","han")
 ,("PODPHHGG","hans")
 ,("POFP","vem")
 ,("POFPHH","vem")
 ,("POFPHHAA","vem")
 ,("POFPHHGG","vems")
 ,("POKP","sådan") -- ?
 ,("POKPHH","som")
 ,("POKPHHGG","andras") --not in gf
 ,("PONP","ingen")
 ,("PONPHH","ingen")
 ,("POOP","det")
 ,("POOPHH","det")
 ,("POPP  AA","oss")
 ,("POPP","jag")
 ,("POPPHH","jag")
 ,("POPPHHAA","oss")
 ,("POPPHHGG","vårt")
 ,("PORP","som")
 ,("PORPHH","som")
 ,("PORPHHAA","som")
 ,("PORPHHGG","vars")
 ,("POSU","flesta") -- ?
 ,("POSUHH","flesta")
 ,("POSUHHGG","flestas")
 ,("POTP","allt")
 ,("POTPHH","allt")
 ,("POTPHHAA","allt")
 ,("POTPHHGG","allts")
 ,("POXP  AA","sig")
 ,("POXP  GG","sin")
 ,("POXP","sig")
 ,("POXPHH","sin")
 ,("POXPHHAA","sig")
 ,("POXPHHGG","sin")
 ,("POXX","sig")
 ,("POZP","jag")
 ,("POZPHH","jag")
 ,("POZPHHAA","mig")
 ,("POZPHHGG","min")
 ,("PR","på")
 ,("PRKP","närmare")
 ,("PRSU","närmaste") -- ?
			{-	"QV">The verb "kunna" (can)</value>
 ,("QVIV">The verb "kunna" (can), infinitive</value>
 ,("QVPS">The verb "kunna" (can), present</value>
 ,("QVPT">The verb "kunna" (can), preterite</value>
 ,("QVSN">The verb "kunna" (can), supine</value> 
 ,("RO  HH">Numeral other than "en", "ett" (one), person</value>
 ,("RO">Numeral other than "en", "ett" (one)</value>
 ,("ROOT">Numeral other than "en", "ett" (one), ordinal</value>
 ,("ROOTHH">Numeral other than "en", "ett" (one), ordinal, person</value> -}
 ,("SP  HH","jagande")
 ,("SP  HHGG","jagandes")
 ,("SP  HM","jagande")
 ,("SP  HMGG","jagandes")
 ,("SP  SM","jagandes")
 ,("SP","jagande")
 ,("SV","ska")
 ,("SVIV","ska")
 ,("SVPS","ska")
 ,("SVPT","skulle")
 ,("TP    PA","ätits")
 ,("TP  HH","äten")
 ,("TP  HHGG","ätens")
 ,("TP  HHPA","äten")
 ,("TP  HM","äten")
 ,("TP  HMGG","äten")
 ,("TP  HMPA","äten")
 ,("TP  SM","äten")
 ,("TP  SMPA","äten")
 ,("TP","äten")
 ,("UK","så")
 ,("UKAT","att")
 ,("UKFI","så")
 ,("UKKC","så")
 ,("UKKD","så")
 ,("UKKM","så")
 ,("UKKS","så")
 ,("UKKU","så")
 ,("UKOM","om")
 ,("UKTE","så")
 ,("VN    GG","katts")
 ,("VN  HH","katt")
 ,("VN  HS","katt")
 ,("VN  SS","katt")
 ,("VN","katt")
 ,("VNDD  GG","kattens")
 ,("VNDD","katten")
 ,("VNDDHH","katten")
 ,("VNDDHS","katten")
 ,("VNDDSS","katten")
 ,("VNDDSSGG","kattens")
 ,("VV    PA","äts")
 ,("VV  SM","tänka")
 ,("VV  SMPA","äts")
 ,("VV","tänka")
 ,("VVIP","tänk")
 ,("VVIPSM","tänk")
 ,("VVIV  PA","ätas")
 ,("VVIV","tänka")
 ,("VVIVSM","tänka")
 ,("VVIVSMPA","ätas")
 ,("VVOP","kunde")
 ,("VVPS  PA","äts")
 ,("VVPS","tänker")
 ,("VVPSSM","tänker")
 ,("VVPSSMPA","äts")
 ,("VVPT  PA","åts")
 ,("VVPT","tänkte")
 ,("VVPTSM","tänkte")
 ,("VVPTSMPA","åts")
 ,("VVSN  PA","ätits")
 ,("VVSN","tänkt")
 ,("VVSNSM","tänkt")
 ,("VVSNSMPA","ätits")
 {-				"WVIV">The verb "vilja" (want), infinitive</value>
 ,("WVPK">The verb "vilja" (want), preterite subjunctive</value>
 ,("WVPS">The verb "vilja" (want), present</value>
 ,("WVPT">The verb "vilja" (want), preterite</value>
 ,("WVSN">The verb "vilja" (want), supine</value> -}
 ,("XX","katt")
 ,("YY","ja")]

