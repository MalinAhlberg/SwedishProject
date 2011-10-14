
module CommandsSw where

import BuildSw
import Frontend
import TypesSw
import GenRulesSw
import General
import Dictionary
import Attr
import Maybe(catMaybes)
import qualified Data.Map as Map

commands :: [(String, [String], [String] -> Entry)]
commands = sal ++ sal_tail

suffix :: Int -> Entry -> String ->Entry
suffix n e s = map_wordforms ((tk n s)++) e

unDEFINED :: String -> Entry
unDEFINED _ = emptyEntry

noc :: (String -> Entry) -> (String -> Entry)
noc f = \s -> remove_param "sms" $ remove_param "ci" $ remove_param "cm" $ f s

sal_tail :: [(String, [String], [String] -> Entry)]
sal_tail =
 [
  paradigm_h "pm_fph_kleopatra"              ["Kleopatra"]            $ pm_f "ph",
  paradigm_h "pm_uls_storgatan"              ["Storgatan"]            $ pm_u "ls",
  paradigm_h "pm_ulf_centralen"              ["Centralen"]            $ pm_u "lf",
  paradigm_h "pm_noe_harvard"                ["Harvard"]              $ pm_n "oe",
  paradigm_h "pm_utz_bambara"                ["Bambara"]              $ pm_u "tz",
  paradigm_h "pm_vlg_nordsjön"               ["Nordsjön"]             $ pm_v "lg",
  paradigm_h "pm_uoc_operan"                 ["Operan"]               $ pm_u "oc",
  paradigm_h "pm_ueh_upplysningen"           ["Upplysningen"]         $ pm_u "eh",
  paradigm_h "pm_uaa_viggen"                 ["Viggen"]               $ pm_u "aa",
  paradigm_h "pm_naw_titanic"                ["Titanic"]              $ pm_n "aw",
  paradigm_h "pm_upm_audhumbla"              ["Audhumbla"]            $ pm_u "pm",
  paradigm_h "pm_uog_polisen"                ["Polisen"]              $ pm_u "og",
  paradigm_h "pm_uaa_camel"                  ["Camel"]                $ pm_u "aa",
  paradigm_h "pm_poc_hepstars"               ["Hepstars"]             $ pm_p "oc",
  -- paradigm_h "pm_naa_keso"                   ["Keso"]                 $ pm_n "aa",
  paradigm_h "pm_nac_saldo"                  ["SALDO"]                $ pm_n "ac",
  paradigm_h "pm_upa_brunte"                 ["Brunte"]               $ pm_u "pa",
  paradigm_h "pm_uop_landsorganisationen"    ["Landsorganisationen"]  $ pm_u "op",
  paradigm_h "pm_uoe_kursverksamheten"       ["Kursverksamheten"]     $ pm_u "oe",
  paradigm_h "pm_uap_vasaorden"              ["Vasaorden"]            $ pm_u "ap",
  paradigm_h "pm_nwm_aktuellt"               ["Aktuellt"]             $ pm_n "wm",
  paradigm_h "pm_nop_efta"                   ["EFTA"]                 $ pm_n "op",
  paradigm_h "pm_nog_skatteverket"           ["Skatteverket"]         $ pm_n "og",
  paradigm_h "pm_nog_knesset"                ["Knesset"]              $ pm_n "og",
  paradigm_h "pm_nog_interpol"               ["Interpol"]             $ pm_n "og",
  paradigm_h "pm_noc_musikforum"             ["Musikforum"]           $ pm_n "oc",
  paradigm_h "pm_nla_solsystemet"            ["Solsystemet"]          $ pm_n "la",
  paradigm_h "pm_hph_af"                     ["AF"]                   $ pm_h "ph",
  paradigm_h "pm_fph_barbro"                 ["Barbro"]               $ pm_f "ph",
  paradigm_h "pm_uwa_monalisa"               ["Monalisa"]             $ pm_u "wa",
  paradigm_h "pm_upc_ttaps-gruppen"          ["TTAPS-gruppen"]        $ pm_u "pc",
  paradigm_h "pm_uop_atlantpakten"           ["Atlantpakten"]         $ pm_u "op",
  paradigm_h "pm_uae_keso"                   ["Keso"]                 $ pm_u "ae",
  paradigm_h "pm_nwp_charta77"               ["Charta77"]             $ pm_n "wp",
  paradigm_h "pm_nos_gais"                   ["Gais"]                 $ pm_n "os",
  paradigm_h "pm_noa_finnair"                ["Finnair"]              $ pm_n "oa",
  paradigm_h "pm_nes_vasaloppet"             ["Vasaloppet"]           $ pm_n "es",
  paradigm_h "pm_nap_nobelpriset"            ["Nobelpriset"]          $ pm_n "ap",
  -- paradigm_h "pm_naa_camel"                  ["Camel"]                $ pm_n "aa",
  paradigm_h "pm_fpm_maria"                  ["Maria"]                $ 
   set_inhs ["f","pm"] . set_pos "pm" . noun_f 1 Utr  ([e "a"], [e "an"], [e "or"], [e "orna"]),   
  paradigm_h "pma_woc_od"                    ["OD"]                   $ pma_w "oc",
  paradigm_h "pma_nwb_blm"                   ["FoF"]                  $ pma_n "wb",
  paradigm_h "pma_nom_svt"                   ["BBC"]                  $ pma_n "om",
  paradigm_h "pma_nos_gsk"                   ["GSK"]                  $ pma_n "os",
  paradigm_h "pma_nog_ab"                    ["AB"]                   $ pma_n "og",
  paradigm_h "pma_noa_sas"                   ["SAS"]                  $ pma_n "oa",
  paradigm_h "pma_nam_thx"                   ["THX"]                  $ pma_n "am",
  paradigm_h "pma_naf_jas"                   ["JAS"]                  $ pma_n "af",
  paradigm_h "pma_naa_lep"                   ["LEP"]                  $ pma_n "aa",
  paradigm_h "pma_mph_jr"                    ["Jr"]                   $ pma_m "ph",
  paradigm_h "pma_hph_nn"                    ["N.N."]                 $ pma_h "ph",
  paradigm_h "pma_noe_gu"                    ["GU"]                   $ pma_n "oe",
  paradigm_h "pma_nlp_eu"                    ["EU"]                   $ pma_n "lp",
  paradigm_h "pma_uwn_dn"                    ["DN"]                   $ pma_u "wn",
  paradigm_h "pma_ntm_cp"                    ["cp"]                   $ pma_n "tm",
  paradigm_h "pma_mpm_st"                    ["St"]                   $ pma_m "pm",
  paradigm_h "pmm_n0eh_andra_världskriget"   ["Andra världskriget"]   $ pmm_n "eh",
  paradigm_h "pmm_u0wc_fröken_julie"         ["Fröken Julie"]         $ pmm_u "wc",
  paradigm_h "pmm_u0la_stora_björnen"        ["Stora björnen"]        $ pmm_u "la",
  paradigm_h "pmm_n0op_grön_ungdom"          ["Grön ungdom"]          $ pmm_n "op",
  paradigm_h "pmm_u0tb_betula_alba"          ["betula alba"]          $ pmm_u "tb",
  paradigm_h "pmm_m0ph_birger_jarl"          ["Birger Jarl"]          $ pmm_m "ph",
  paradigm_h "pmm_u0oe_svenska_institutionen" ["Svenska institutionen"] $ pmm_u "oe",
  paradigm_h "pmm_u0aa_koh_i_noor"           ["Koh i noor"]           $ pmm_u "aa",
  paradigm_h "pmm_m0ph_per_olov"             ["Per Olov"]             $ pmm_m "ph",
  paradigm_h "pmm_m0ph_karl_den_tolfte"      ["Karl den tolfte"]      $ pmm_m "ph",
  paradigm_h "pmm_m0ph_el_greco"             ["el greco"]             $ pmm_m "ph",
  paradigm_h "pmm_f0pm_jungfru_maria"        ["jungfru Maria"]        $ pmm_f "pm",
  paradigm_h "pmm_n0lf_vita_huset"           ["Vita huset"]           $ pmm_n "lf",
  paradigm_h "pmm_m0ph_karl_xii"             ["Karl xii"]             $ pmam_m "ph",
  paradigm_h "pmm_h0ph_jonsson_lind"         ["Jonsson Lind"]         $ pmm_h "ph",
  paradigm_h "pmm_u0tm_parkinsons_sjukdom"   ["Parkinsons sjukdom"]   $ pmm_u "tm",
  paradigm_h "pmm_u0op_nysvenska_rörelsen"   ["Nysvenska rörelsen"]   $ pmm_u "op",
  paradigm_h "pmm_u0ls_lilla_nygatan"        ["Lilla Nygatan"]        $ pmm_u "ls",
  paradigm_h "pmm_u0en_big_bang"             ["Big Bang"]             $ pmm_u "en",
  paradigm_h "pmm_u0aw_cutty_sark"           ["Cutty Sark"]           $ pmm_u "aw",
  paradigm_h "pmm_n0oc_ebba_grön"            ["Ebba Grön"]            $ pmm_n "oc",
  paradigm_h "pmm_m0pm_john_blund"           ["John Blund"]           $ pmm_m "pm",
  paradigm_h "pmm_m0ph_adam_av_bremen"       ["Adam av Bremen"]       $ pmm_m "ph",
  paradigm_h "pmm_v0lf_notre_dame"           ["Notre Dame"]           $ pmm_v "lf",
  paradigm_h "pmm_u0wn_dagens_nyheter"       ["Dagens nyheter"]       $ pmm_u "wn",
  paradigm_h "pmm_u0es_davis_cup"            ["Davis cup"]              $ pmm_u "es",
  paradigm_h "pmm_u0er_marie_bebådelse"      ["Marie bebådelse"]        $ pmm_u "er",
  paradigm_h "pmm_u0eh_franska_revolutionen" ["Franska Revolutionen"]   $ pmm_u "eh",
  paradigm_h "pmm_u0ag_rolls_royce"          ["Rolls Royce"]            $ pmm_u "ag",
  paradigm_h "pmm_p0ph_bröderna_grimm"         ["Bröderna Grimm"]         $ pmm_p "ph",
  paradigm_h "pmm_m0ph_plinius_d_y"           ["Plinius d y"]           $ pmam_m "pa",
  paradigm_h "pmm_m0pa_pelle_svanslös"        ["Pelle Svanslös"]        $ pmm_m "pa",
  paradigm_h "pmm_f0ph_eva_ek"                ["Eva Ek"]                $ pmm_f "ph",
  paradigm_h "pmm_uatm_multipel_skleros"      ["multipel skleros"]      $ pmm_u "tm",
  paradigm_h "pmm_uatm_cerebral_pares"        ["cerebral pares"]        $ pmm_u "tm",
  paradigm_h "pmm_pcpm_hugin_och_munin"       ["Hugin och Munin"]       $ pmm_p "pm",
  paradigm_h "pmm_f1pm_jungfrun_från_orleans" ["Jungfrun från Orleans"] $ pmm_f "pm",
  paradigm_h "pmm_nu0wn_svenska_dagbladet"    ["Svenska Dagbladet"]     $ pmm_n "wn",
  paradigm_h "pmm_nu0wn_svenskt_associationslexikon"    ["Svenskt associationslexikon"]     $ pmm_n "wn",
  paradigm_h "ab_2_bra"                  ["bra"] $ suffix 3 (ab_bra ["bra"] ["bättre"] ["bäst"]),
  paradigm_h "ab_2_nära"                 ["nära"] $ 
   suffix 4 (ab_bra ["nära"] ["närmare", "närmre"] ["närmast", "närmst"]),
  paradigm_h "ab_2_mycket"               ["mycket"] $ suffix 6 (ab_bra ["mycket"] ["mer","mera"] ["mest"]),
  paradigm_h "ab_2_länge"                ["länge"] $ suffix 5 (ab_bra ["länge"] ["längre"] ["längst"]),
  paradigm_h "ab_2_illa"                 ["illa"] $ 
    suffix 4 (ab_bra ["illa"] ["sämre","värre"] ["sämst","värst"]),
  paradigm_h "ab_2_gärna"                ["gärna"] $ suffix 5 (ab_bra ["gärna"] ["hellre"] ["helst"]),
  paradigm_h "ab_2_föga"                 ["föga"]  $ suffix 4 (ab_bra ["föga"] ["mindre"] ["minst"]),
  paradigm_h "ab_ik_vidare"              ["vidare"] $
    suffix 1 (ab_comp [] ["e"] [] ["e"]),
  paradigm_h "ab_2_lite"              ["lite"] $
     suffix 4 (ab_bra ["lite"] ["mindre"] ["minst"]),
  paradigm_h "ie_i_att"                  ["att"]            inf_mark,
  paradigm_h "sn_i_om"                   ["om"]             subj,
  paradigm_h "abh_i_ledes" ["ledes"] $ 
   replace_attr w_attr s_attr . set_pos "abh" . ab_bort,  
  paradigm_h "avh_1_aktig" ["aktig"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_1_blek,
  paradigm_h "avh_1_bent" ["bent"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_1_akut,
  paradigm_h "avh_0_artad" ["artad"] $ 
   replace_attr w_attr s_attr . set_pos "avh" . av_0_konstlad,
  paradigm_h "avh_0_buren" ["buren"] $ 
   replace_attr w_attr s_attr . set_pos "avh" .
    adj 2 ([e "en"], [e "et"], [e "na"], [e "na"], [], [], []),
  paradigm_h "nnh_4u_bo" ["bo"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . nn4,
  paradigm_h "nnh_2u_siding" ["siding"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_compound 0 Utr ([e ""], [e "en"], [e "ar"], [e "arna"], [(ds, "")],[(ds, "")]),
  paradigm_h "nnh_6u_tonnare" ["tonnare"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" .    
    noun_compound 0 Utr ([e ""], [e "n"], [e ""], [(tk 1, "na")],[(tk 1, "")],[(ds_drop,"")]), 

  paradigm_h "nnh_dn_snåret" ["snåret"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_f 0 Neutr  ([], [e ""], [], []),

  paradigm_h "nnh_du_årsåldern" ["årsåldern"] $ 
   replace_attr w_attr s_attr . set_pos "nnh" . 
    noun_f 0 Utr  ([], [e ""], [], []),

  paradigm_h "nn_3n_parti"     ["parti"]       nn3_parti,
  paradigm_h "nn_3u_fiber"     ["fiber"] $
   noun_compound 0 Utr ([e ""], [e "n"], [(dvu,"er")], [(dvu,"erna")],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_3u_tand"      ["tand"]        nn3_tand,
  paradigm_h "nn_3u_film"      ["film"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e ""],[e "", (ds,"")]),   

  paradigm_h "nn_3u_bygd"      ["bygd"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e "",e "e"],[e "e", (ds,"")]),   

  paradigm_h "nn_3u_plan"      ["plan"] $
             noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"],[e ""],[e "e",e "", (ds,"")]),   

  paradigm_h "nn_3u_akademi"   ["akademi"] $
   noun_compound 0 Utr ([e ""], [e "n",e "en"], [e "er"], [e "erna"],[e ""],[e ""]),   

  paradigm_h "nn_dn_rubbet"   ["rubbet"] $
   noun_compound 0 Neutr  ([], [e ""], [], [],[],[]),   

  paradigm_h "nn_dp_tropikerna"   ["tropikerna"] $
   noun_compound 0 GPl ([], [], [], [e ""],[],[]),   

  paradigm_h "nn_du_stampen"   ["stampen"] $
   noun_compound 0 Utr ([], [e ""], [], [],[],[]),   

  paradigm_h "nn_np_ordalag"   ["ordalag"] $
   noun_f 0 GPl ([], [], [e ""], [e "en"]),   

  paradigm_h "nn_rp_benvärmare"   ["benvärmare"] $
   noun_f 0 GPl ([], [], [e ""], [(tk 1, "na")]),   

  paradigm_h "nn_rp_griller"   ["griller"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[],[]),   

  paradigm_h "nn_rp_inälvor"   ["inälvor"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . tk 2,"")],[(ds . tk 2,"")]),   

  paradigm_h "nn_rp_johannesnycklar"   ["johannesnycklar"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 3,"el"),(tk 3,"els")],[(tk 3,"el"),(tk 3,"els")]),   

  paradigm_h "nn_rp_kläder"   ["kläder"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,""),(ds . tk 2,""),(tk 2,"es")],[(tk 2,""),(ds . tk 2,""),(tk 2,"es")]),   
  paradigm_h "nn_rp_kråkfötter"   ["kråkfötter"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . vc "o" . tk 3,"")], [(ds . vc "o" . tk 3,"")]),   

  paradigm_h "nn_rp_paltor"   ["paltor"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"")],[(tk 2,""),(ds . tk 2,"")]),   

  paradigm_h "nn_rp_specerier"   ["specerier"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"")],[(tk 2,"")]),   

  paradigm_h "nn_rp_stadgar"   ["stadgar"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 2,"e")],[(tk 2,"e")]),   

  paradigm_h "nn_rp_svear"   ["svear"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(tk 1,"")],[(tk 1,"")]),   

  paradigm_h "nn_rp_underkläder"   ["underkläder"] $
   noun_compound 0 GPl ([], [], [e ""], [e "na"],[(ds . tk 2,""),(tk 2,"es")],[(ds . tk 2,""),(tk 2,"es")]),   

  paradigm_h "pn_o_sån" ["sån"] $ 
    pn_nagon ([e "",(tk 1,"dan")],[e "t",(tk 1,"dant")],[e "a",(tk 1,"dana")]),

  paradigm_h "pn_o_all" ["all"] $ 
    pn_nagon ([e ""],[e "t"],[e "a"]),

  paradigm_h "pn_o_varsin" ["varsin"] $ 
    pn_nagon ([e ""],[(tk 1,"tt")],[e "a"]),

  paradigm_h "pn_o_vem" ["vem"] $ 
    pn_vem ([e ""],[e ""],[e "s"]),

  paradigm_h "pn_o_varandra" ["varandra"] $ 
    pn_vem ([],[e ""],[e "s"]),

  paradigm_h "pn_o_man" ["man"] $ 
   pn_han (P3,Sg) ([e ""],[(tk 3,"en")],[(tk 3, "ens")]),
    
  paradigm_h "pn_i_vars" ["vars"] $ pn_inv,

  paradigm_h "pn_o_sig" ["sig"] $
    pn_jag (P3,Sg) ([],[e "",(tk 2,"ej")],[(tk 1,"n")],[(tk 1,"tt")],[(tk 1,"na")]),

  paradigm_h "pn_o_ingen"             ["ingen"] $
   pn_nagon ([e ""],[(tk 1,"t")],[(tk 2,"a")]),

  paradigm_h "pn_o_den"               ["den"]   $
   pn_o_den, 
  paradigm_h "pn_o_någon"             ["någon"] $
   pn_nagon ([e "", (tk 3,"n")],[(tk 1,"t"), (tk 3,"t")],[(tk 2,"ra")]),
 paradigm_h "pn_o_ingendera"        ["ingendera"] $
    pn_nagon ([e ""],[(tk 5,"tdera")],[(tk 6,"adera")]),
  paradigm_h "pn_o_vi"                ["vi"] $
    pn_jag  (P1,Pl) ([e ""],[(tk 2, "oss")],[(tk 2,"vår"),(tk 2,"våran")],[(tk 2,"vårt"),(tk 2,"vårat")],[(tk 2,"våra")]),
  paradigm_h "pn_o_de"                ["de"] $
   pn_han (P3,Pl) ([e "",(vc "o","m")],[(id,"m"),(vc "o","m")],[e "ras"]),
  paradigm_h "pn_o_varenda"           ["varenda"] $
   pn_nagon ([e "",(tk 1,"e")], [(tk 4, "tenda")],[]),
  paradigm_h "pn_o_vardera"           ["vardera"] $
   pn_nagon ([e ""], [(tk 4, "tdera")],[]),
  paradigm_h "pn_o_varannan"          ["varannan"] $
   pn_nagon ([e ""],[(tk 5,"tannat")],[]),
  paradigm_h "pn_o_var"               ["var"] $
   pn_nagon ([e ""],[e "t"],[]),
  paradigm_h "pn_o_samma"             ["samma"] $
   pn_nagon ([e "",(tk 1,"e")],[e ""],[e ""]),
  paradigm_h "pn_o_ni"                ["ni"] $
    pn_jag  (P2,Pl) ([e ""],[(tk 2, "er")],[(tk 2,"er"),(tk 2, "eran")],[(tk 2,"ert"),(tk 2,"erat")],[(tk 2,"era")]),
  paradigm_h "pn_o_jag"               ["jag"] $
    pn_jag  (P1,Sg) ([e ""],[(tk 3, "mig"),(tk 3,"mej")],[(tk 3,"min")],[(tk 3,"mitt")],[(tk 3,"mina")]),
  paradigm_h "pn_o_högstdensamme"     ["högstdensamme"] $
   pn_nagon ([e ""],[(tk 6,"tsamma")],[(tk 6,"samma")]),
  paradigm_h "pn_o_hon"               ["hon"] $
   pn_han (P3,Sg) ([e ""],[(vc "e","ne")],[(tk 2,"ennes")]),
  paradigm_h "pn_o_han"               ["han"] $
   pn_han (P3,Sg) ([e ""], [(vc "o","om")], [e "s"]),
  paradigm_h "pn_o_endera"            ["endera"] $
   pn_nagon ([e ""],[(tk 5,"ttdera")],[]),
  paradigm_h "pn_o_du"                ["du"] $
    pn_jag  (P2,Sg) ([e ""],[(tk 1, "ig"),(tk 1,"ej")],[(tk 1,"in")],[(tk 1,"itt")],[(tk 1,"ina")]),
  paradigm_h "pn_o_densamma"          ["densamma"] $
    pn_nagon ([e "",(tk 1,"e")],[(tk 6,"tsamma")],[(tk 6,"samma")]),
  paradigm_h "pn_o_denna"             ["denna"] $
   pn_nagon ([e "",(tk 1,"e")],[(tk 3,"tta")],[(tk 3,"ssa")]),
  paradigm_h "pn_o_annan"             ["annan"] $
   pn_nagon ([e ""],[(tk 1,"t")],[(tk 3,"dra")]),
  -- paradigm_h "al_o_den"               ["den"]               al_o_den,
  paradigm_h "nn_vu_mixer" ["mixer"]  $         
   noun_f 0 Utr  ([e ""], [e "n"], [(id,""), (dv,"ar"),(id,"s")], [(id,"na"), (dv,"arna"),e "sarna"]),   

  paradigm_h "nn_vu_latte" ["latte"]  $         
   noun_compound 0 Utr  ([e ""], [e "n"], [e "", (tk 1,"ar"),e "s"], [e "na", (tk 1,"arna"),e "sarna"],[e ""],[e ""]),   

  paradigm_h "nn_vn_medium" ["medium"]  $     
   noun_compound 0 Neutr  ([e ""], [(tk 2,"et")], [(tk 2,"er"),(tk 2,"a")], [(tk 2,"erna"),(tk 2,"ana")],[(tk 2,"e"),e ""],[(tk 2,"e"),e ""]),

  paradigm_h "nn_ou_bekant" ["bekant"]   $     
    noun Utr [""] ["en"] ["er","a"] ["erna"],

  paradigm_h "nn_on_memorandum" ["memorandum"]  $  
   noun_f 0 Neutr ([e ""], [e "", e "et"], [(tk 2, "a")], [(tk 2,"ana")]),

  paradigm_h "nn_1u_åder" ["åder"]    $       
   noun_compound 0 Utr ([e "",(dv,"a")], [e "n",(dv,"an")], [(dv,"or")], [(dv,"orna")],[e ""],[(dv,"e"), e ""]),

  paradigm_h "nn_1u_gata" ["gata"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1,""),(ungeminate.tk 1,"u")],[(ungeminate.tk 1,""),(ungeminate.tk 1,"u")]),

  paradigm_h "nn_1u_mamma" ["mamma"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1,"a")],[(tk 1,"e"),(tk 1,"a")]),

  -- paradigm_h "nn_1u_dagmamma" ["dagmamma"]    $       
  -- noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1,"e"),(tk 1,"a")],[(tk 1,"e"),(tk 1,"a")]),

  paradigm_h "nn_1u_människa" ["människa"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1,"o")],[(ungeminate.tk 1,"o")]),

  paradigm_h "nn_1u_kamera" ["kamera"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [e ""],[e ""]),

  paradigm_h "nn_1u_olja" ["olja"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e")],[(tk 1, "e")]),

  paradigm_h "nn_1u_baksida" ["baksida"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e"),(tk 1,"es")],[(tk 1, "e"),(tk 1,"es")]),

  paradigm_h "nn_1u_sida" ["sida"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(ungeminate.tk 1, ""),(tk 1,"o")],[(tk 1, "e"),(tk 1,"es")]),

  paradigm_h "nn_1u_folksaga" ["folksaga"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")], [(tk 1, "e"),(tk 1,"o")],[(tk 1, "e"),(tk 1,"o")]),

  paradigm_h "nn_1u_kyrka" ["kyrka"]    $       
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [(ungeminate,""),e "o"],[(ungeminate,""),e "o",(ds, ""),e "e"]),

  paradigm_h "nn_2u_mening" ["mening"]    $       
   noun_compound 0 Utr ([e ""], [e "en"], [e "ar"], [e "arna"], [(ds, "")],[(ds, "")]),

  paradigm_h "nn_3u_salong" ["salong"]    $       
   noun_compound 0 Utr ([e ""], [e "en"], [e "er"], [e "erna"], [(ds, "")],[(ds, "")]),

  paradigm_h "nn_5n_dike" ["dike"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [(ds, "")],[(ds, "")]),

  paradigm_h "nn_5n_hjärta" ["hjärta"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [(tk 1, "")],[(tk 1, ""),(ds.tk 1,"")]),

  paradigm_h "nn_5n_saldo" ["saldo"]    $       
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"], [e ""],[e ""]),

  paradigm_h "nn_6n_departement" ["departement"]    $       
   noun_compound 0 Neutr ([e ""], [e "et"], [e "en"], [e "ena"], [(ds, "")],[(ds, "")]),

  paradigm_h "nn_0u_svenska" ["svenska"]    $       
   noun_compound 0 Utr ([e ""], [e "n"], [], [], [(tk 1,"a"),(tk 1,"")],[(tk 1,"a"),(tk 1,"")]),

  paradigm_h "nn_vu_kart" ["kart"]   $       
   noun Utr [""] ["en"] ["ar",""] ["arna","en"],

  paradigm_h "nn_vn_mirakel" ["mirakel"]   $      
   noun_f 0 Neutr ([e ""], [(dv,"et")], [(id,""),(dv,"er")], 
                          [(dv,"en"), (dv,"erna")]),

  paradigm_h "nn_0v_trim" ["trim"]  $  
   noun_f 0 Pend  ([e ""], [(geminate,"en"), (geminate,"et")], [], []),

  paradigm_h "nn_0v_blod" ["blod"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[e "",(ds,"")],[e "",(ds,"")]),

 paradigm_h "nn_0v_saffran" ["saffran"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[(ds,"")],[(ds,"")]),

  paradigm_h "nn_0v_gin" ["gin"]  $  
   noun_compound 0 Pend  ([e ""], [e "en", e "et"], [], [],[e ""],[e "",(ds,"")]),

  paradigm_h "nn_0v_tö" ["tö"]  $  
   noun_f 0 Pend  ([e ""], [e "et", e "n", e "t"], [], []),

  paradigm_h "nn_0n_bitumen" ["bitumen"]  $
   noun_f 0 Neutr ([e ""], [(id, ""),(vc "i", "et")], [], []),

  paradigm_h "nn_vn_lexikon" ["lexikon"]  $      
   noun_f 0 Neutr ([e ""], [e "et"], [(id,""),(tk 2,"a")], [e "en"]),

  paradigm_h "nn_2u_slarver" ["slarver"]  $    
   noun_f 0 Utr ([e ""], [e "n"], [(dvu,"ar"),(tk 2,"ar")], [(dvu,"arna"), (tk 2,"arna")]),

  paradigm_h "nn_2u_bräken" ["bräken"]  $    
   noun_f 0 Utr ([e ""], [e "", (dv, "en")], [(dv,"ar")], [(dv,"arna")]),

  paradigm_h "nn_2u_himmel" ["himmel"]  $     
   noun_f 0 Utr ([e ""], [(tk 3,"len"), (id,"en"),(id,"n")], [(tk 3, "lar")], [(tk 3, "larna")]),

  paradigm_h "nn_4n_fängelse" ["fängelse"]  $    
   noun Neutr [""] ["t"] ["r"] ["rna"],

  paradigm_h "nn_2u_dag" ["dag"]  $      
   noun_f 0 Utr ([e ""], [(id,"en"),(tk 1,"n")], [(id,"ar"),(tk 1,"r")], [(id,"arna"),(tk 1,"rna")]),

  paradigm_h "nn_vu_ponny" ["ponny"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1, "ier"),(tk 1,"ies"),(id,"er"), e "sar"], [(tk 1,"ierna"), e "erna",(tk 1,"iesarna"), e "sarna"]),

  paradigm_h "nn_vu_kollega" ["kollega"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1,"er"),(tk 1,"or")], [(tk 1,"erna"),(tk 1,"orna")]),

  paradigm_h "nn_vn_kolli" ["kolli"]  $       
   noun Neutr [""] ["t"] ["","n"] ["na"],

  paradigm_h "nn_6u_tum" ["tum"]  $        
   noun Utr [""] ["men"] [""] ["men"],

  paradigm_h "nn_6n_universum" ["universum"]  $    
   noun Neutr [""] ["", "et"] [""] ["en"],

  paradigm_h "nn_0n_gluten" ["gluten"]  $ 
   noun_compound 0 Neutr ([e ""], [e "et",e ""],[],[],[e ""],[e ""]),

  paradigm_h "nn_vu_yard" ["yard"]  $        
   noun Utr [""] ["en"] ["","s"] ["en","sen"],

  paradigm_h "nn_vu_svan" ["svan"]  $         
   noun Utr [""] ["en"] ["ar","or"] ["arna","orna"],

  paradigm_h "nn_vn_tema" ["tema"]  $          
   noun Neutr [""] ["t"] ["n","ta"] ["na","tana"],

  -- paradigm_h "nn_vn_perfektum" ["perfektum"]  $     
  -- noun_f 0 Neutr ([e ""],[e "et"],[e "", (tk 2,"er")], [e "en",(tk 2,"erna")]),

  paradigm_h "nn_vn_maximum" ["maximum"]  $      
   noun_f 0 Neutr ([e ""], [e "",e "et",(tk 2, "et")], [(id,""),(tk 2,"a")],[(tk 2,"ana"),(id,"en")]),

  paradigm_h "nn_vn_frö" ["frö"]  $         
   noun Neutr [""] ["et","t"] ["er","n"] ["erna","na","en"],

  paradigm_h "nn_3u_materia" ["materia"]  $      
   noun_f 0 Utr ([e ""], [e "n"], [(tk 1,"er")], [(tk 1,"erna")]),

  paradigm_h "nn_0n_delirium" ["delirium"]  $
   noun_f 0 Neutr ([e ""],[(tk 2, "et")],[],[]),

  -- paradigm_h "nn_vv_fossil" ["fossil"]  $
  -- noun Pend [""] ["en","et"] ["er",""] ["erna","en"],

  paradigm_h "nn_vv_libretto" ["libretto"]  $
   noun Pend [""] ["n","t"] ["r","n"] ["rna","na"],

  paradigm_h "nn_vu_safari" ["safari"]  $
   noun Utr [""] ["n"] ["er","s"] ["erna","sarna"],

  paradigm_h "nn_vu_bungalow" ["bungalow"]  $
   noun Utr [""] ["en"] ["er","s"] ["erna"],

   paradigm_h "nn_ip_honoratiores" ["honoratiores"]  $
   noun_no_genitive GPl ([],[],[e ""],[]),

  paradigm_h "nn_7u_lady" ["lady"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"ies")],[(tk 1,"ies"),(tk 1,"iesarna")]),

  paradigm_h "nn_6v_kvitten" ["kvitten"]  $
   noun Pend [""] [""] [""] ["a"],

  paradigm_h "nn_vv_franska" ["franska"]  $
   noun_f 0 Pend ([e ""],[e "n",e "t"],[(id,""),(tk 1,"or")], [e "na",(tk 1,"orna")]),

  paradigm_h "nn_4v_folie" ["folie"]  $
   noun Pend [""] ["n","t"] ["r"] ["rna"],

  paradigm_h "nn_3u_donjuan" ["donjuan"]  $
   noun Utr [""] [""] ["er"] ["erna"],

  paradigm_h "nn_2v_finger" ["finger"]  $
   noun_f 0 Pend ([e ""], [(dv,"et"),(id,"n")], [(dv,"ar")],[(dv,"arna")]),

  paradigm_h "nn_2u_biceps" ["biceps"]  $
   noun Utr [""] ["", "en"] ["ar"] ["arna"],

  paradigm_h "nn_1u_ultima" ["ultima"]  $
   noun_f 0 Utr ([e ""], [e "", e "n"], [(tk 1,"or")], [(tk 1,"orna")]),           

  paradigm_h "nn_0n_opium" ["opium"]  $
   noun_compound 0 Neutr ([e ""], [(tk 2,"et"),(id,"et")],[],[],[e ""],[e ""]),

  paradigm_h "nn_vv_skogsrå" ["skogsrå"]  $
   noun Pend [""] ["et","t","n"] ["r","n"] ["rna","na"],

  paradigm_h "nn_vv_prisma" ["prisma"]  $
   noun_f 0 Pend ([e ""], [e "n",e "t"], [(tk 1, "or"),(tk 1,"er")], [(tk 1,"orna"),(tk 1,"erna")]),

  paradigm_h "nn_vv_hult" ["hult"]   $
   noun Pend [""] ["et","en"] ["","ar","er"] ["en","arna","erna"],

  paradigm_h "nn_vu_spaniel" ["spaniel"]   $   
   noun Utr [""] ["n"] ["ar","s"] ["arna"],

  paradigm_h "nn_vu_litteraturkanon" ["litteraturkanon"]  $
   noun Utr [""] ["en",""] ["er",""] ["erna"],

  paradigm_h "nn_vu_igloo" ["igloo"]  $
   noun_compound 0 Utr ([e ""], [e "n"], [e "r",e "er",e "s", e "sar"],[e "rna",e "erna",e "sarna"],[e ""],[e ""]),

  paradigm_h "nn_vn_alfa_io" ["i"]  $
   set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "t", e "et"],[e "ts", e "ets"],[e "n", e ""],[e "ns",e "s"],[e "na",e "en"],[e "nas",e "ens"]),

  paradigm_h "nn_6n_deponens" ["deponens"]  $
   noun Neutr [""] [""] [""] ["en"],

  paradigm_h "nn_6n_andeväsen" ["andeväsen"]  $
   noun Neutr [""] ["det"] [""] ["a"],

  paradigm_h "nn_5n_altare" ["altare"]  $
   noun_compound 1 Neutr ([e "e"], [e "et"], [e "en"], [e "na"],[e ""],[e ""]),

  paradigm_h "nn_3u_geranium" ["geranium"]  $
   noun_f 0 Utr ([e ""],[(tk 2,"en")],[(tk 2,"er")],[(tk 2,"erna")]),

  -- paradigm_h "nn_1u_toffel" ["toffel"]  $
  -- noun_compound 0 Utr ([e ""], [e "n"],[(dvu,"or")],[(dvu,"orna")],[e ""],[e ""]),

  paradigm_h "nn_1u_tavla" ["tavla"]  $
   noun_compound 0 Utr ([e ""], [e "n"],[(dv,"or")],[(dv,"orna")],[((\s -> insert_second_last s 'e') . tk 1,"")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")]),

  paradigm_h "nn_1u_stamtavla" ["stamtavla"]  $
   noun_compound 0 Utr ([e ""], [e "n"],[(dv,"or")],[(dv,"orna")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")],[((\s -> insert_second_last s 'e') . tk 1,""),(tk 1,"e")]),

  paradigm_h "nn_0v_dregel" ["dregel"]  $
   noun_compound 0 Pend ([e ""],[(id,"n"), (dvu,"et")], [], [],[e ""],[e ""]),

--  paradigm_h "nn_0u_koppar" ["koppar"]  $
--   noun_compound 0 Utr ([e ""], [e "en", e "n"],[],[],[e ""],[e ""]),

  paradigm_h "nn_vv_paraply" ["paraply"]  $
   noun Pend [""] ["et","t","n"] ["er","n"] ["erna","na"],

  paradigm_h "nn_vv_etage" ["etage"]  $
   noun Pend [""] ["t","n"] ["r",""] ["rna","n"],

  paradigm_h "nn_vv_chiffer" ["chiffer"]  $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")],[(id,""),(dv,"er")],[(dv,"en"),(dv,"erna")]),

  paradigm_h "nn_vv_bolster" ["bolster"]  $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")],[(id,""),(dv,"ar")],[(dv,"arna"), (dv,"en"),(id,"na")]),

  paradigm_h "nn_vu_teve" ["teve"]  $
   noun Utr [""] ["n"] ["","ar"] ["na","arna"],

  paradigm_h "nn_vu_rhododendron" ["rhododendron"]  $
   noun_f 0 Utr ([e ""], [e "en"], [e "",(tk 2, "er")], [(tk 2, "erna")]),

  paradigm_h "nn_vu_kofot" ["kofot"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(umlaut . geminate,"er")],[(id,"arna"),(umlaut . geminate,"erna")]),

  paradigm_h "nn_vu_jourhavande" ["jourhavande"]  $
   noun Utr [""] ["n"] ["","n"] ["na"],

  paradigm_h "nn_vu_jockey" ["jockey"]  $
   noun_compound 0 Utr ([e ""],[e "n",e "en"],[e "ar",e "er",e "s",e "sar"],[e "arna",e "erna",e "sarna"],[e ""],[e ""]),

  paradigm_h "nn_vu_grej" ["grej"]  $
   noun Utr [""] ["en"] ["er","or"] ["erna","orna"],

  paradigm_h "nn_vu_drive" ["drive"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"ar"),(id,"s")],[(tk 1,"arna")]),

  paradigm_h "nn_vu_cello" ["cello"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"r"),(tk 1,"i")],[(tk 1,"orna")]),

  paradigm_h "nn_vn_trauma" ["trauma"]  $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"er")],[e "na",(tk 1,"erna")]),

  paradigm_h "nn_vn_stall" ["stall"]  $
   noun Neutr [""] ["et"] ["","ar"] ["en","arna"],

  paradigm_h "nn_vn_pi" ["pi"]  $
   noun Neutr [""] ["et","t"] ["","n"] ["na","en"],

  paradigm_h "nn_vn_paper" ["paper"]  $
   noun Neutr [""] ["et"] ["s",""] ["en"],

  paradigm_h "nn_vn_panorama" ["panorama"]  $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"or")],[e "na",(tk 1,"orna")]),

  paradigm_h "nn_vn_logi" ["logi"]  $
   noun Neutr [""] ["et","t"] ["er","n"] ["erna","na"],

  paradigm_h "nn_ou_medikus" ["medikus"]  $
   noun_f 0 Utr ([e ""], [e "",e "en"],[(tk 3,"ci")], [(tk 3,"cina")]),

  paradigm_h "nn_6v_årder" ["årder"]  $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")], [e ""],[(dv,"en"),(id,"na")]),

  paradigm_h "nn_6v_hästskosöm" ["hästskosöm"]  $
   noun_f 0 Pend ([e ""], [(geminate,"et"),(geminate,"en")], [e ""], [(geminate,"en")]),

  paradigm_h "nn_6u_bror" ["bror"]   $
   noun_compound 0 Utr ([e ""],[(tk 1,"dern"),(id,"n")], [(umlaut . tk 1, "der")], [(umlaut . tk 1, "derna")],[e "",(ds,"")],[e "",(ds,"")]),

  paradigm_h "nn_6n_pansar" ["pansar"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e ""],[e "na",e "en"],[e ""],[e ""]),

  paradigm_h "nn_3n_center" ["center"]  $
   noun_f 0 Neutr ([e ""],[(dv,"et")],[(dv,"er")],[(dv,"erna")]),

  paradigm_h "nn_6n_ankare" ["ankare"]  $
   noun_compound 0 Neutr ([e ""], [e "t"], [e "",e "n"],[(tk 1,"na")],[(tk 1,"")],[(tk 1,"")]),

  paradigm_h "nn_3v_plasma" ["plasma"]  $
   noun_f 0 Pend ([e ""], [e "t", e "n"],[(tk 1,"er")],[(tk 1,"erna")]),

  paradigm_h "nn_3u_papyrus" ["papyrus"]  $
   noun_f 0 Utr ([e ""],[(tk 2,"en"), (id,"en")],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3n_alluvium" ["alluvium"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2, "et")],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3n_alkali" ["alkali"]  $
   noun Neutr [""] ["t"] ["er"] ["erna"],

  paradigm_h "nn_3n_gift" ["gift"] $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[e ""],[e "",(ds,"")]),

  paradigm_h "nn_3n_histamin" ["histamin"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[e ""],[e ""]),

  paradigm_h "nn_3n_portvin" ["portvin"]  $
   noun_compound 0 Neutr ([e ""],[e "et"],[e "er"],[e "erna"],[(ds,"")],[(ds,"")]),

  paradigm_h "nn_3u_tid" ["tid"]  $
   noun_compound 0 Utr ([e ""],[e "en"],[e "er"],[e "erna"],[e "",(ds,"")],[e "",(ds,"")]),

  paradigm_h "nn_2v_skit" ["skit"]  $
   noun Pend [""] ["en","et"] ["ar"] ["arna"],

  paradigm_h "nn_2u_toddy" ["toddy"]  $
   noun_f 0 Utr ([e ""],[e "n"],[e "ar", (tk 1, "ar")],[e "arna", (tk 1,"arna")]),

  paradigm_h "nn_0u_praxis" ["praxis"]  $
   noun Utr [""] ["en",""] [] [],

  paradigm_h "nn_vv_stimulus" ["stimulus"]  $
   noun_f 0 Pend ([e ""], [e "", e "en", e "et"],[(id ,""),(tk 2,"i")], [e "en",(tk 2, "ina")]),

  paradigm_h "nn_vv_rå_gång" ["rå"]  $
   noun Pend [""] ["n","et","t"] ["r"] ["rna"],

  paradigm_h "nn_vv_ringfinger" ["ringfinger"]  $
   noun_f 0 Pend ([e ""],[(dv,"et"),(id,"n")], [(dv,"ar"),(id,"")], [(dv,"arna")]),

  paradigm_h "nn_vv_ordal" ["ordal"]  $
   noun Pend [""] ["et","en"] ["","ier"] ["ierna"],

  paradigm_h "nn_vv_halvankare" ["halvankare"]  $
   noun_f 0 Pend ([e ""], [e "t",e "n"], [e "",e "n"],[(tk 1, "na")]),

  paradigm_h "nn_vu_western" ["western"]  $     
   noun_f 0 Utr ([e ""],[e ""],[e "",e "s"],[e "a"]),

  paradigm_h "nn_vu_torso" ["torso"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"er"),(id,"er"),(id,"r")], [(tk 1,"erna"),(id,"erna"),(id,"rna")]),

  paradigm_h "nn_vu_spång" ["spång"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(vc "ä","er")], [(id,"arna"),(vc "ä","erna")]),

  -- paradigm_h "nn_vu_spann" ["spann"]  $
  -- noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(vc "ä","er")],[(id,"arna"),(vc "ä", "erna")]),

  paradigm_h "nn_vu_scarf" ["scarf"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,"ar"),(tk 1,"ves")],[(id,"arna"),(tk 1,"vesen")]),

  paradigm_h "nn_vu_rubel" ["rubel"]  $
   noun_f 0 Utr ([e ""], [e "n"],[e "",(dvu,"er")],[(dvu,"erna")]),

  paradigm_h "nn_vu_ro" ["ro"]   $
   noun Utr [""] ["n"] ["n","r"] ["na","rna"],

  paradigm_h "nn_vu_promovend" ["promovend"]  $
   noun Utr [""] ["en"] ["er","i"] ["erna","ina"],

  paradigm_h "nn_vu_preses" ["preses"]  $
   noun_f 0 Utr ([e ""], [e ""],[(tk 2, "ides"),(id,"ar")], [e "arna"]),

  paradigm_h "nn_vu_paria" ["paria"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"s"),(tk 1,"or")],[(tk 1,"orna")]),

  paradigm_h "nn_vu_mikron" ["mikron"]  $
   noun Utr [""] ["en"] ["er",""] ["erna","en"],

  paradigm_h "nn_vu_lama" ["lama"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"or"),(id,"er")],[(tk 1,"orna"),(id,"erna")]),

  paradigm_h "nn_vu_glass" ["glass"]  $
   noun Utr [""] ["en"] ["ar","er",""] ["arna","erna"],

  paradigm_h "nn_vu_gladiolus" ["gladiolus"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(id,""),(tk 2,"er"),(id,"ar")], [(tk 2,"erna"),(id,"arna")]),

  paradigm_h "nn_vu_baby" ["baby"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,"ar"),(id,"er"),(tk 1,"ies")], [e "arna",e "erna",(tk 1,"iesarna")]),

  paradigm_h "nn_vu_albino" ["albino"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"er"),(id,"s")],[(tk 1,"erna"),(id,"sarna")]),

  paradigm_h "nn_vn_stånd" ["stånd"]  $
   noun_f 0 Neutr ([e ""], [e "et"],[(id,""),(vc "ä","er")],[(id,"en"),(vc "ä","erna")]),

  paradigm_h "nn_vn_solo" ["solo"]  $
   noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"i")], [e "na"]),

  paradigm_h "nn_vn_serum" ["serum"]  $
   noun_f 0 Neutr ([e ""], [e "",e "et"],[(id,""),(tk 2,"a")], [(tk 2,"ana"),e "en"]),

  -- paradigm_h "nn_vn_scenario" ["scenario"]  $
  -- noun_f 0 Neutr ([e ""], [e "t"],[(id,"n"),(tk 1,"er")],[(id,"na"),(tk 1,"erna")]),

  paradigm_h "nn_vn_rö" ["rö"]  $
   noun Neutr [""] ["t","et"] ["","n"] ["na"],

  paradigm_h "nn_vn_rekviem" ["rekviem"]  $
   noun_f 0 Neutr ([e ""],[(id,""),(tk 1,"t"),(tk 1,"met")],[(id,""),(tk 1,"r")],[(id,"en"),(tk 1,"rna")]),

  paradigm_h "nn_vn_omen" ["omen"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2,"inet")],[(id,""),(tk 2,"ina")],[(id,"en")]),

  paradigm_h "nn_vn_mineral" ["mineral"]  $
   noun Neutr [""] ["et"] ["er", "ier"] ["erna","ierna"],

  paradigm_h "nn_vn_lim" ["lim"]  $
   noun Neutr [""] ["met"] ["", "mer"] ["men","merna"],

  paradigm_h "nn_vn_kompositum" ["kompositum"]  $
   noun_f 0 Neutr ([e ""],[(id,""),(tk 2,"et")],[(tk 2,"a"), (tk 2,"er")], [(tk 2,"erna")]),

  paradigm_h "nn_vn_ja" ["ja"]  $
   noun Neutr [""] ["et","t"] ["n", ""] ["na"],

  paradigm_h "nn_vn_härad" ["härad"]  $
   noun Neutr [""] ["et"] ["", "er","en"] ["ena","erna"],

  paradigm_h "nn_vn_gag" ["gag"]  $
   (noun Neutr [""] ["et"] ["s", ""] ["sen","en"]),

  paradigm_h "nn_vn_gage" ["gage"]  $
   noun_compound 0 Neutr ([e ""],[e "t"],[e "",e "r"],[e "n",e "rna"],[e ""],[e ""]),

  paradigm_h "nn_vn_apropå" ["apropå"]   $       
   noun Neutr [""] ["t"] ["n","er"] ["na","erna"],

  paradigm_h "nn_vn_alfa_z" ["z"]  $
   set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],[e "n", e ""],[e "ns",e "s"],[e "na"],[e "nas"]),

  paradigm_h "nn_vn_ackordion" ["ackordion"]  $
   noun_f 0 Neutr ([e ""],[(id,"et"),(tk 2,"et")],[(id,""),(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_ov_styck" ["styck"]  $
   noun_compound 0 Pend ([e "",e "en",e "na"],[e "en",e "et"],[e "", e "en",e "na"],[e "ena"],[e ""],[e ""]),

  paradigm_h "nn_ov_diktamen" ["diktamen"]  $
   noun_f 0 Pend ([e ""], [e ""],[(tk 2,"ina")], [(tk 2,"ina")]),

  paradigm_h "nn_ou_putto" ["putto"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 1,"i")], [(tk 1,"ina")]),

  paradigm_h "nn_ou_penny" ["penny"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(tk 2,"ce"),(tk 1,"ies")], [(tk 2,"cen"),(tk 1,"iesarna")]),

  paradigm_h "nn_ou_mekanikus" ["mekanikus"]  $
   noun_f 0 Utr ([e ""], [e "en"],[(tk 3,"ci")], [(tk 3,"cina")]),

  -- paradigm_h "nn_on_slusshuvud" ["slusshuvud"]  $
  -- noun Neutr [""] ["et"] ["en",""] ["ena"],

  paradigm_h "nn_on_gravamen" ["gravamen"]  $
   noun_f 0 Neutr ([e ""], [e "et"],[(tk 2,"ina")], []),

  paradigm_h "nn_7u_slogan" ["slogan"]  $
   noun Utr [""] ["","en"] ["s"] ["sen"],

  paradigm_h "nn_vu_romkom" ["romkom"]  $
   noun_compound 0 Utr ([e ""],[e "en",e "men"],[e "s", e "mar"],[e "sen",e "marna"],[e ""],[e ""]),

  paradigm_h "nn_vu_tortilla" ["tortilla"]  $
   noun_compound 1 Utr ([e "a"],[e "an"],[e "or", e "as"],[e "orna",e "asen",e "asena",e "asarna"],[e "a"],[e "a"]),

  paradigm_h "nn_7u_cashew" ["cashew"]  $
   noun_compound 0 Utr ([e ""],[e "en",e "n"],[e "s"],[e "sen"],[e ""],[e ""]),

  paradigm_h "nn_7n_skinhead" ["skinhead"]  $
   noun Neutr [""] ["et"] ["s"] ["sen"],

  paradigm_h "nn_6v_modus" ["modus"]  $
   noun Pend [""] ["","et"] [""] ["en"],

  paradigm_h "nn_6v_data" ["data"]  $
   noun_compound 0 Pend ([e ""],[e "n",e "t"],[e ""],[e "na"],[e ""],[e ""]),

  paradigm_h "nn_6u_man" ["man"]   $
   noun_f 0 Utr ([e ""], [e "nen"], [(umlaut,""),(id,""),(id,"nar")],[(umlaut,"nen"),(id,"narna")]),

  paradigm_h "nn_6u_iktus" ["iktus"]  $
   noun Utr [""] ["en", ""] [""] ["en"],

  paradigm_h "nn_6n_interregnum" ["interregnum"]  $
   noun_f 0 Neutr ([e ""], [e "et",e ""], [e ""], [e "en"]),

  paradigm_h "nn_5n_ri" ["ri"]  $
   noun Neutr [""] ["et"] ["n"] ["en","na"],

  paradigm_h "nn_3v_gelé" ["gelé"]  $
   noun_compound 0 Pend ([e ""],[e "n",e "t",e "et"],[e "er"],[e "erna"],[e ""],[e ""]),

  paradigm_h "nn_3u_fotnot" ["fotnot"]  $
   noun_f 0 Utr ([e ""], [e "en"], [(id,"er"),(vc "ö","ter")],[(id,"erna"),(vc "ö","terna")]),

  paradigm_h "nn_3u_farao" ["farao"]  $
   noun Utr [""] ["","n"] ["ner"] ["nerna"],

  paradigm_h "nn_3u_eforus" ["eforus"]  $
   noun_f 0 Utr ([e ""], [e ""],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3n_seminarium" ["seminarium"]  $
   noun_f 0 Neutr ([e ""], [e "et",(tk 2, "et")],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3n_futurum" ["futurum"]  $
   noun_f 0 Neutr ([e ""], [e "",e "et"],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3n_dominion" ["dominion"]  $
   noun_f 0 Neutr ([e ""], [e ""],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_3v_aktivum" ["aktivum"]  $
   noun_f 0 Pend ([(tk 2,""), e ""],[(tk 2,"et"),(tk 2,"en"),(id,"et"),(id,"")],[(tk 2,"er")],[(tk 2,"erna")]),

  paradigm_h "nn_2u_stygger" ["stygger"]  $
   noun_f 0 Utr ([e ""],[(id,"n"),(tk 1,"n")],[(tk 2,"ar")],[(tk 2,"arna")]),

  paradigm_h "nn_2u_förmiddag" ["förmiddag"]  $ 
   noun_f 0 Utr ([e ""],[(id,"en"),(tk 1,"n")],[(id,"ar")],[(id,"arna")]),

  paradigm_h "nn_2u_andur" ["andur"]  $
   noun_f 0 Utr ([e ""], [e "en",e "n"],[(dvu,"ar")],[(dvu,"arna")]),

  paradigm_h "nn_1v_antibiotika" ["antibiotika"]  $
   noun_f 0 Pend ([e ""], [e "n",e "t"],[(tk 1,"or")],[(tk 1,"orna")]),

  paradigm_h "nn_0v_status" ["status"]  $
   noun Pend [""] ["","en"] [] [],

  paradigm_h "nn_0v_hysteri" ["hysteri"]  $
   noun Pend [""] ["n","en","et","t"] [] [],

  paradigm_h "nn_0v_facit" ["facit"]  $
   noun_f 0 Pend ([e ""],[e ""],[],[]),

  paradigm_h "nn_0u_makadam" ["makadam"]  $
   noun_f 0 Utr ([e ""], [(geminate,"en"),(id,"en")], [], []),

  paradigm_h "nn_0u_aorta" ["aorta"]  $
   noun Utr [""] ["","n"] [] [],

  paradigm_h "nn_0n_karborundum" ["karborundum"]  $
   noun Neutr [""] ["","et"] [] [],

  paradigm_h "nn_0n_kammarkollegium" ["kammarkollegium"]  $
   noun_f 0 Neutr ([e ""],[e "",(tk 2,"et")],[],[]),

  paradigm_h "nn_0n_gehenna" ["gehenna"]   $
   noun Neutr [""] ["","t"] [] [],

  paradigm_h "nn_2u_bövel" ["bövel"]  $
   noun_f 0 Utr ([e ""],[e "n",e "en"],[(dvu,"ar")],[(dvu,"arna")]),

  paradigm_h "av_0s_ypperst" ["ypperst"]  $ 
   adj 0 ([], [], [], [], [], [e ""], [e "a"]), 

  paradigm_h "av_0_korkad"            ["korkad"]         av_0_konstlad,
  paradigm_h "av_1_enkel"             ["enkel"]          av_1_vacker,
  paradigm_h "av_in_lurt"             ["lurt"]          $ 
             replace_param [("invar","pos indef sg n nom")] . av_i_diverse,
  paradigm_h "av_id_norra"            ["norra"] $ 
             replace_param [("invar","pos def sg no_masc nom")] . av_i_diverse,
  paradigm_h "av_im_bemälde"          ["bemälde"] $
             replace_param [("invar","pos def sg masc nom")] . av_i_diverse,
  paradigm_h "av_ik_smärre"           ["smärre"] 
   $ adj 0 ([], [], [], [], [e ""], [], []), 
  paradigm_h "avm_io0_diverse"        ["bevänt med"]   $ noc $ avm_i,
  paradigm_h "avm_ix0_diverse"        ["allena saliggörande"]   $ noc $ avm_i,
  paradigm_h "avm_ia0_diverse"        ["före detta"]   $ noc $ avm_i,
  paradigm_h "avm_ip0_diverse"        ["idel öra"]     $ noc $ avm_i,
  paradigm_h "avm_is20_utom_sig"        ["utom sig"]     $ noc $ avm_i,
  -- paradigm_h "av_0_skriftlärd"        ["skriftlärd"]      av_0_konstlad,

  paradigm_h "av_0_höger" ["höger"] 
   (adj 2 ([e "er"], [e "er"], [e "ra"], [e "ra"], [], [], [])), 

  -- paradigm_h "av_0_världsbäst" ["världsbäst"]  
  -- (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 

  paradigm_h "av_0_vareviga" ["vareviga"] 
   (adj 0 ([(tk 1, "")], [(tk 5, "teviga")], [], [],[], [], [])), 

  paradigm_h "av_0_sankt" ["sankt"] 
   (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 

  paradigm_h "av_0_pytteliten" ["pytteliten"] 
   (adjc 5 ([e "liten"], [e "litet"], [e "lilla"], [e "små"], [], [], [],[e "små"])), 

  -- paradigm_h "av_0_blott" ["blott"]   
  -- (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 

  paradigm_h "av_0s_innersta" ["innersta"] $ 
   (adj 0 ([], [], [], [], [], [e ""], [])), 

  -- paradigm_h "av_0p_samtliga" ["samtliga"] 
  -- (adj 0 ([], [], [], [e ""], [], [], [])), 

  paradigm_h "av_0d_nästa" ["nästa"] 
   (adj 0 ([], [], [e ""], [], [], [], [])), 

  paradigm_h "av_0d_enda" ["enda"] 
   (adj 0 ([], [], [e ""], [], [], [], [])), 

  paradigm_h "av_v_ond" ["ond"]  
   (adj 0 ([e ""],[(tk 1,"t")], [e "a"], [e "a"], [(id,"are"), (tk 3, "värre")], [(id,"ast"), (tk 3, "värst")], [(id,"aste"),(tk 3, "värsta")])), 

  paradigm_h "av_v_god" ["god"] 
   (adj 0 ([e ""],[(tk 1,"tt")], [e "a"], [e "a"],[(id,"are"),(tk 3, "bättre")], [(id,"ast"),(tk 3,"bäst")],[(id,"aste"),(tk 3, "bästa")])), 

  paradigm_h "av_2_gammal" ["gammal"] 
   (adj 6 ([e "gammal"], [e "gammalt"], [e "gamla"], [e "gamla"], [e "äldre"], [e "äldst"], [e "äldsta"])), 

   paradigm_h "av_2_bra" ["bra"] 
   (adj 2 ([e "ra"], [e "ra"], [e "ra"], [e "ra"], [e "ättre"], [e "äst"], [e "ästa"])), 

  paradigm_h "av_v_nära" ["nära"]  
   (adj 1 ([e "a"], [e "a"], [e "a"], [e "a"], [e "mare",e "mre"], [e "mast",e "mst"], [e "masta",e "msta"])), 

  paradigm_h "av_v_förnäm" ["förnäm"] 
    (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [e "are"], [e "st",e "ast"], [e "sta",e "asta"])), 

  paradigm_h "av_v_dålig" ["dålig"] 
    (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [(tk 5,"sämre"), e "are"], [(tk 5,"sämst"),e "ast"], [(tk 5,"sämsta"), e "aste"])), 

  paradigm_h "av_2k_bakre" ["bakre"] 
    (adj 2 ([], [], [], [], [e "re"], [e "ersta"], [e "ersta"])), 

  paradigm_h "pn_o_mycken" ["mycken"] $ set_pos "pn" . 
     (adj 5 ([e "ycken"], [e "ycket"], [e "yckna"], [e "yckna"], [e "er",e "era"], [e "est"], [e "esta"])), 

  paradigm_h "pn_o_mången" ["mången"] $ set_pos "pn" . 
    (adj_no_masc 0 ([e ""], [(tk 2, "t"),(tk 1, "t")], [(tk 2,"a")], [(tk 2,"a")], [(tk 6,"fler"),(tk 6,"flera")], [(tk 6, "flest")], [(tk 6,"flesta")])), 

  paradigm_h "av_2_liten" ["liten"]  
    (adjc 5 ([e "liten"], [e "litet"], [e "lilla"], [e "små"], [e "mindre"], [e "minst"], [e "minsta"],[e "små"])), 

  paradigm_h "av_2_få" ["få"] 
    (adj 0 ([e ""], [e ""], [e ""], [e ""],[(umlaut, "rre")], [(umlaut,"rst")], [(umlaut,"rsta")])), 

  paradigm_h "av_1_orange" ["orange"] 
    (adj 1 ([e "e"], [e "e", e "t", e "et"], [e "a", e "e", e "ea"], [e "a", e "e", e "ea"], [e "are", e "eare"], [e "ast",e "east"], [e "aste", e "easte"])), 

  paradigm_h "av_1_gratis" ["gratis"] 
    (adj 0 ([e ""], [e "", e "t"], [e "", e "a"], [e "", e "a"], [e "are"], [e "ast"], [e "aste"])), 

  paradigm_h "av_1_knall" ["knall"] 
    (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"])), 

  paradigm_h "av_1_camp" ["camp"]  
    (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"])), 

  paradigm_h "av_1_beige" ["beige"] 
    (adj 0 ([e ""], [e "t"], [e "",e "a"], [e "",e "a"], 
            [e "are"], [e "ast"], [e "aste"])), 

  paradigm_h "av_1_ball" ["ball"]  $
    adj 0 ([e ""], [e "",e "t"], [e "a"], [e "a"], [e "are"], [e "ast"], [e "aste"]), 

  paradigm_h "nnm_vv0_libretto" ["allt i allo"]  $
   noc $ 
    last_mw "nnm" ((noun Pend [""] ["n","t"] ["r","n"] ["rna","na"])),

 paradigm_h "nnm_vu0_kaffe_latte" ["kaffe latte"]  $
   noc $ 
    last_mw "nnm" (noun_compound 0 Utr  ([e ""], [e "n"], [e "", (tk 1,"ar"),e "s", e "r"], [e "na", (tk 1,"arna"),e "sarna", e "rna"],[e ""],[e ""])),

  paradigm_h "nnm_vv0_hult" ["bäst före-datum"]   $
   noc $ last_mw "nnm" (noun Pend [""] ["et","en"] ["","ar","er"] ["en","arna","erna"]),

  paradigm_h "nnm_0v_manna" ["lingua franca"] $
   noc $ last_mw "nnm" (noun_f 0 Pend ([e ""],[e "n", e "t"],[],[])),

  paradigm_h "nnm_du0_stampen" ["gordiska knuten"] $
   noc $ last_mw "nnm" (noun_f 0 Utr ([],[e ""],[],[])),

  paradigm_h "nnm_vu0_mikron" ["grand danois"] $
   noc $ last_mw "nnm" (noun Utr [""] ["en"] ["er",""] ["erna"]),

  paradigm_h "nnm_vu0_trio" ["femme fatale"] $
   noc $ last_mw "nnm" (noun Utr [""] ["n"] ["r","s"] ["rna"]),

  paradigm_h "nnm_vu0_bungalow" ["spin off"] $
   noc $ last_mw "nnm" (noun Utr [""] ["en"] ["er","s"] ["erna","sen"]),

  paradigm_h "nnm_vv0_pain_riche" ["pain riche"] $
   noc $ last_mw "nnm" (noun Pend [""] ["n","t"] ["r",""] ["rna","na"]),

  paradigm_h "nnm_vv0_deja_vu" ["déjá vu"]   $
   noc $ last_mw "nnm" (noun Pend [""] ["n","t"] ["r","n"] ["rna","na"]),

  paradigm_h "nnm_rp0_griller" ["scampi fritti"] $
   noc $ last_mw "nnm" (noun_f 0 GPl ([], [], [e ""], [e "na"])),   

  paradigm_h "nnm_vn0_alfa_z" ["ettstrukna c"]  $
   noc $ last_mw "nnm" (nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],[],[],[],[])),

  paradigm_h "nnm_su0_pojke" ["dödens lammunge"] $
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""], [], [(drop_final_e,"ar")], [])),

  paradigm_h "nnm_su0_tro" ["janssons frestelse"] $
   noc $ last_mw "nnm" (noun Utr [""] ["n"] [] []),

   paradigm_h "nnm_7u0_hit" ["negro spiritual"]  $
   noc $ last_mw "nnm" (noun Utr [""] ["en"] ["s"] ["sen"]),  

  paradigm_h "nnm_7n0_skinhead" ["practical joke"]  $
   noc $ last_mw "nnm" (noun Neutr [""] ["t"] ["s"] ["sen"]),  

  paradigm_h "nnm_6u0_yen" ["pol mag"]  $ 
   noc $ last_mw "nnm" (noun Utr [""] ["en"] [""] ["en"]),  

  paradigm_h "nnm_6n0_blad" ["flygande tefat"]  $
   noc $ last_mw "nnm" (noun Neutr [""] ["et"] [""] ["en"]),  

  paradigm_h "nnm_6n1_blad" ["äss i rockärmen"]  $
   noc $ first_mw "nnm" (noun Neutr [""] ["et"] [""] ["en"]),  

  paradigm_h "nnm_5n0_ansikte" ["da capo"]  $
   noc $ last_mw "nnm" (noun Neutr [""] ["t"] ["n"] ["na"]),  

  paradigm_h "nnm_ip0_honoratiores" ["lika goda kålsupare"]  $
   noc $ last_mw "nnm" (noun_no_genitive GPl ([],[],[e ""], [])),  

  paradigm_h "nnm_3u0_film" ["medicine kandidat"]  $
   noc $ last_mw "nnm" (noun Utr [""] ["en"] ["er"] ["erna"]),  

  paradigm_h "nnm_3n0_parti" ["12 V-batteri"]  $
   noc $ last_mw "nnm" (noun Neutr [""] ["et"] ["er"] ["erna"]),  

 paradigm_h "nnm_3u1_film" ["dans på rosor"]  $
   noc $ first_mw "nnm" (noun Utr [""] ["en"] ["er"] ["erna"]),  

  paradigm_h "nnm_2u0_stol" ["vinst- och förlusträkning"]  $ 
   noc $ last_mw "nnm" (noun Utr [""] ["en"] ["ar"] ["arna"]),  

  paradigm_h "nnm_2u0_dag" ["bäst före-dag"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""], [(id,"en"),(tk 1,"n")], [(id,"ar"),(tk 1,"r")], [(id,"arna"),(tk 1,"rna")])),

  paradigm_h "nnm_2u0_nyckel" ["golden retriever"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(dvu,"ar")], [(dvu,"arna")])),  

  paradigm_h "nnm_rp1_vägnar" ["fjärilar i magen"]  $ 
   noc $ first_mw "nnm" (noun_compound 0 GPl ([],[], [e ""], [e "na"],[],[])),  

  paradigm_h "nnm_1u0_flicka" ["mul- och klövsjuka"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(tk 1,"or")], [(tk 1,"orna")])),  

  paradigm_h "nnm_0u0_hin" ["hin håle"]  $
    noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[], [], [])),  

  paradigm_h "nnm_0u0_frid" ["rhode islandsås"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "en"], [], [])),  

  paradigm_h "nnm_np0_ordalag" ["nordiska språk"]  $ 
   noc $ last_mw "nnm" (noun_f 0 GPl ([],[], [e ""], [e "en"])),  

  paradigm_h "nnm_4u0_linje" ["eau de cologne"]  $
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [e "r"], [e "rna"])),  

  paradigm_h "nnm_2u1_stol" ["ulv i fårakläder"]  $ 
   noc $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "en"], [e "ar"], [e "arna"],[],[])),  

  paradigm_h "nnm_2u0_pojke" ["vandrande pinne"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [(drop_final_e,"ar")], [(drop_final_e,"arna")])),  

  paradigm_h "nnm_rp0_kalla_kårar" ["kalla kårar"]  $
   noc $ last_mw "nnm" (noun_f 0 GPl ([],[], [e ""], [e "na"])),

  paradigm_h "nnm_1u1_flicka" ["fnurra på tråden"]  $ 
   noc $ first_mw "nnm" (noun_compound 0 Utr ([e ""],[e "n"], [(tk 1,"or")], [(tk 1,"orna")],[],[])),  
  paradigm_h "nnm_0u0_tro" ["cherry brandy"]  $ 
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n"], [], [])),

  paradigm_h "nnm_0u0_antimateria" ["idé- och lärdomshistoria"]  $
   noc $ last_mw "nnm" (noun_f 0 Utr ([e ""],[e "n", (tk 1,"en")], [], [])),

  paradigm_h "nnm_in0_vaj" ["berått mod"]  $
   noc $ last_mw "nnm" (noun_no_genitive Neutr ([e ""],[], [], [])),

  paradigm_h "nnm_iu0_vift" ["gilla gång"]  $
   noc $ last_mw "nnm" (noun_no_genitive Utr ([e ""],[], [], [])),

  paradigm_h "nnm_dn0_rubbet" ["rubbet"] $
   noc $ last_mw "nnm" (noun_compound 0 Neutr ([],[e ""], [], [],[],[])),

  paradigm_h "nna_0v_pcb"  ["pcb"]  $ 
   nna Pend ([e ""],[e "s"], [e "n", e "en",e "t",e "et"],[e "ns", e "ens",e "ts", e "ets"],[],[],[],[]),

  paradigm_h "nna_vu_dvd"  ["dvd"]  $ 
   nnac Utr ([e ""],[e "s"], [e "n"],[e "ns"],[e "ar",e "er"],[e "ars",e "ers"],[e "arna",e "erna"],[e "arnas",e "ernas"]),

  paradigm_h "nna_0n_hk"  ["hk"]  $ 
   nna Neutr ([e ""],[e "s"], [],[],[],[],[],[]),

  paradigm_h "nna_0u_jo"  ["jo"]  $ 
   nna Utr ([e ""],[e "s"], [],[],[],[],[],[]),

  paradigm_h "nna_vv_dna"  ["dna"]  $ 
   nna Pend ([e ""],[e "s"], 
             [e "n", e "t"],[e "ns",e "ts"],
             [e "",e "er"],[e "s",e "ers"],
             [e "na",e "erna"],[e "nas",e "ernas"]
            ),
  paradigm_h "nna_6n_kg"   ["kg"]  $ 
   nna Neutr ([e ""],[e "s"], [],[], [e ""], [e "s"], [], []),
  paradigm_h "nna_6u_lp"   ["lp"]  $ 
   nna Utr ([e ""],[e "s"], [e "n"],[e "ns"],
            [e ""],[e "s"],[e "na"],[e "nas"]),
   paradigm_h "nna_in_ex"   ["ex"]  $ 
   nna Neutr ([e ""],[],[],[],[],[],[],[]),
   paradigm_h "nna_6n_ekg"  ["ekg"]  $
   nna Neutr ([e ""],[e "s"], [e "t"],[e "ts"],
            [e "",e "n"],[e "s",e "ns"],[e "na"],[e "nas"]),
  paradigm_h "nna_vn_wc"   ["wc"]  $ 
   nna Neutr ([e ""],[e "s"], 
             [e "t"],[e "ts"],
             [e "",e "n"],[e "s",e "ns"],
             [e "en",e "na"],[e "ens",e "nas"]
            ),
  paradigm_h "nna_6v_pm"   ["pm"]  $ 
   nna Pend ([e ""],[e "s"], 
             [e "en", e "et"],[e "ens",e "ets"],
             [e ""],[e "s"],
             [e "na"],[e "nas"]
            ),
  paradigm_h "nna_2u_bh"   ["bh"]  $ 
   nna Utr ([e ""],[e "s"], 
            [e "en",e "n"],[e "ens", e "ns"],
            [e "ar"],[e "ars"],
            [e "arna"],[e "arnas"]
           ),

  paradigm_h "avm_0p0_gul" ["rangen stridig"]  $ 
     noc $ last_mw "avm" $ av_1_blek_ng,
  paradigm_h "avm_0a0_diverse" ["ute efter"] $ 
     noc $ last_mw "avm" av_i_diverse,
  paradigm_h "avm_0a0_korkad" ["så kallad"]  $ 
     noc $ last_mw "avm" (adj 0 ([e ""], [(tk 1,"t")], [e "e"], [e "e"], [], [], [])),
  paradigm_h "avm_0x0_bred" ["naggande god"]  $ 
     noc $ last_mw "avm" (adj 0 ([e ""], [(tk 1,"tt")], [e "e"], [e "e"], [], [], [])),
  paradigm_h "avm_0x0_gul" ["Gudi behaglig"]  $ 
     noc $ last_mw "avm" (adj 0 ([e ""], [e "t"], [e "e"], [e "e"], [], [], [])),
  paradigm_h "avm_0x0_korkad" ["fly förbannad"]  $ 
     noc $ last_mw "avm" (adj 0 ([e ""], [(tk 1, "t")], [e "e"], [e "e"], [], [], [])),
  paradigm_h "avm_0x0_utbrunnen" ["inte oäven"]  $ 
     noc $ last_mw "avm" (adj 0 ([e ""], [(tk 1, "t")], [(tk 1,"e")], [(tk 1, "e")], [], [], [])),

  paradigm_h "avm_0p1_gul" ["hal som en ål"]  $ 
     noc $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e "t"], [], [], [], [], [],[])),
  paradigm_h "avm_0p1_brydd" ["stadd i"]  $ 
     noc $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 2,"tt")], [], [], [], [], [],[])),
  paradigm_h "avm_0p0_diverse" ["ute efter"]  $ 
     noc $ last_mw "avm" av_i_diverse,
  paradigm_h "avm_1p1_bred" ["glad i"]  $ 
     noc $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [(tk 1,"tt")], [], [], [e "are"], [e "ast"], [e "aste"],[])),

  paradigm_h "avm_1p1_akut" ["fäst vid"]  $ 
   noc $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e ""], [], [], [e "are"], [e "ast"], [e "aste"],[])),


  paradigm_h "avm_1x0_akut" ["politiskt korrekt"]  $ 
   noc $ last_mw "avm" (adj 0 ([e ""], [e ""], [e "e"], [e "e"], [], [], [])),

  paradigm_h "avm_1x0_gul" ["gudi behaglig"]  $ 
   noc $ last_mw "avm" (adj 0 ([e ""], [e "t"], [e "e"], [e "e"], [], [], [])),

  paradigm_h "avm_1p1_gul" ["lös i magen"]  $ 
   noc $ first_mw "avm" (adj_no_genitive_c 0 ([e ""], [e ""], [], [], [e "are"], [e "ast"], [e "aste"],[])),

  paradigm_h "al_o_en" ["en"] $ al_o_en,
  paradigm_h "nn_0n_hindi" ["hindi"]  $
   noun_compound 0 Neutr ([e ""], [],[],[],[e ""],[e ""]),
  paradigm_h "nn_0n_kol-14" ["kol-14"] $ 
   nn_kol_14,
  paradigm_h "nn_0u_hin" ["hin"]    $
   noun_f 0 Utr ([e ""], [],[],[]),
  paradigm_h "nn_0u_boskap" ["boskap"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[(ds,"")],[(ds,"")]),

  paradigm_h "nn_0u_brödsäd" ["brödsäd"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[(ds,""),e "es"],[(ds,""), e "es"]),

  -- paradigm_h "nn_0u_filmjölk" ["filmjölk"]    $
  --  noun_compound 0 Utr ([e ""], [e "en"],[],[],[e "",(ds,"")],[e "", (ds,"")]),

  paradigm_h "nn_0u_månsing" ["månsing"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e ""],[e ""]),

  paradigm_h "nn_0u_kärnkraft" ["kärnkraft"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e "",(ds,"")],[e "",(ds,"")]),

  paradigm_h "nn_6u_mat" ["mat"]    $
   noun_compound_ng 0 Utr ([e ""], [],[e ""],[],[],[]),

  paradigm_h "nn_0u_mjölk" ["mjölk"]    $
    noun_compound 0 Utr ([e ""], [e "en"],[],[],[e ""],[e "",(ds,"")]),

  paradigm_h "nn_0u_säd" ["säd"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[],[],[e "", e "es"],[e "es",(ds,"")]),

  paradigm_h "nn_2u_sten" ["sten"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[e "ar"],[e "arna"],[e ""],[e "",(ds,"")]),

  paradigm_h "nn_2u_herde" ["herde"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[e "e",e "a"],[e "a",e "e",e "es"]),

  paradigm_h "nn_2u_hjälte" ["hjälte"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[e "e"],[e "e", e "es"]),

  paradigm_h "nn_2u_pojke" ["pojke"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[(ungeminate,"")],[(ds . ungeminate,"")]),

  paradigm_h "nn_2u_ormbunke" ["ormbunke"]    $
   noun_compound 1 Utr ([e "e"], [e "en"],[e "ar"],[e "arna"],[(ungeminate,""),(ds . ungeminate,""),e "e",e "es"],[(ungeminate,""),(ds . ungeminate,""),e "e",e "es"]),

  paradigm_h "nn_2u_ängel" ["ängel"]    $
   noun_compound 0 Utr ([e ""], [e "n"],[(dvu, "ar")],[(dvu,"arna")],[e "",(dv,"a")],[e "",(ds,""),(dv,"a")]),

  paradigm_h "nn_3u_karbid" ["karbid"]    $
   noun_compound 0 Utr ([e ""], [e "en"],[e "er"],[e "erna"],[e ""],[e ""]),

  paradigm_h "nn_ou_deputerad" ["deputerad"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "e"],[e "ena"]),
  paradigm_h "nn_6u_yen" ["yen"]    $ 
   noun_f 0 Utr ([e ""], [e "en"],[e ""],[e "en"]),
  paradigm_h "nn_vu_bagis" ["bagis"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "",e "ar"],[e "en",e "arna"]),
  paradigm_h "nn_vu_order" ["order"]  $
   noun_f 0 Utr ([e ""], [e "n"],[(id,""),(mmr.dv,"ar")],[e "na", (mmr.dv,"arna")]), 
  paradigm_h "nn_vu_minut" ["minut"]  $ 
   noun_f 0 Utr ([e ""], [e "en"],[e "er",e "rar"],[e "erna",e "rarna"]),
  paradigm_h "av_0_uppsutten" ["uppsutten"] $ 
   (adj 2 ([e "en"], [e "et"], [e "na"], [e "na"], [], [], [])), 
  paradigm_h "av_0_uppvikt" ["uppvikt"] $ 
   (adj 0 ([e ""], [e ""], [e "a"], [e "a"], [], [], [])), 
  paradigm_h "av_v_trång" ["trång"] $ 
   (adj 0 ([e ""], [e "t"], [e "a"], [e "a"], [(vc "ä","re"),(id,"are")], 
           [(vc "ä","st"),(id,"ast")], [(vc "ä","ste"),(id,"aste")])), 
  paradigm_h "pnm_x1_inte_ett_dugg" ["inte småpotatis"]  $ pnm_inv,
  paradigm_h "pnm_x1_vad_än" ["vad än"]  $ pnm_inv,
  paradigm_h "pnm_i_ditt_och_datt" ["ditt och datt"]  $ pnm_inv,
  paradigm_h "pnm_o_vem_som_helst" ["vem som helst"]  $ pnm_gen,
  paradigm_h "pnm_o_en_annan" ["en annan"]  $ pnm_gen2,

  paradigm_h "pnm_o_den_här" ["den här"] pnm_o_den_här,

  paradigm_h "nnm_6na_segel" ["kort varsel"] $ noc $ nnm_6na_kort_varsel,
  paradigm_h "nnm_6na_glykemiskt_index" ["glykemiskt index"] $ noc $ nnm_6na_glykemiskt_index,
  paradigm_h "nnm_6ua_gås"   ["oplockad gås"]  $ noc $ nnm_6ua_oplockad_gås,
  paradigm_h "nnm_6ua_kikare" ["svensk mästare"] $ noc $ nnm_6ua_svensk_mästare,
  paradigm_h "nnm_6na_välsmort_munläder" ["välsmort munläder"] $ noc $ nnm_6na_välsmort_munläder,

  paradigm_h "nnm_0ua_god_ton"           ["god ton"]  $ noc $ nnm_0ua_frid,
  paradigm_h "nnm_3ua_fransysk_visit" ["fransysk visit "]  $ noc $ nnm_3ua_film,
  paradigm_h "nnm_3ua_enarmad_bandit" ["enarmad bandit"]  $ noc $ nnm_3ua_enarmad_bandit,
  paradigm_h "nnm_1ua_svart_låda" ["svart låda"]  $ noc $ nnm_1ua_svart_låda,
  paradigm_h "nnm_3ua_rolig_historia" ["rolig historia"]  $ noc $ nnm_3ua_rolig_historia,
  paradigm_h "nnm_2ua_pojke" ["finsk pinne"] $ noc $ nnm_2ua_pojke,
  paradigm_h "nnm_2ua_naken_blankning" ["naken blankning"] $ noc $ nnm_2ua_naken_blankning,
  paradigm_h "nnm_dpc_göranden_och_låtanden" ["göranden och låtanden"]  $ noc $ nnm_5pc_göranden_och_låtanden,
  paradigm_h "nnm_0n0_fait_accompli" ["fait accompli"] $
    noc $ last_mw "nnm" (noun_f 0 Neutr ([e ""], [],[],[])),
  paradigm_h "nnm_0na_syre"     ["fritt vivre"]     $ noc $ nnm_0na_fritt_vivre,
  paradigm_h "nnm_2ua_stol"            ["varm korv"]       $ noc $ nnm_2ua_stol,
  paradigm_h "nnm_npc_kreti_och_pleti" ["kreti och pleti"] $ noc $ nnm_gpc_kreti_och_pleti,
  paradigm_h "ava_i_kungl"         ["Kungl."]        $ invar "ava",
  paradigm_h "vba_ia_jfr"          ["jfr"]           $ invar "vba",
  paradigm_h "ppa_i_pga"           ["pga"]           $ invar "ppa",
  paradigm_h "ppm_i_a_la"          ["a la"]          $ noc $ invar "ppm",
  paradigm_h "ppm_x1_för_skull"    ["för vidkommande"] $ noc $ invar "ppm",
  paradigm_h "kna_i_o"             ["o"]             $ invar "kna",
  paradigm_h "snm_i_efter_det_att" ["efter det att"] $ noc $ invar "snm",
  paradigm_h "ssm_d2_svinhugg_går_igen" ["svinhugg går igen"] $ noc $ invar "ssm",
  paradigm_h "ssm_i1_märk_väl"          ["märk väl"]          $ noc $ invar "ssm",
  paradigm_h "ssm_d2_saken_är_biff"     ["saken är biff"]     $ noc $ invar "ssm",
  paradigm_h "nlm_gi_tusen_sinom_tusen" ["tusen sinom tusen"] $ noc $ invar "nlm"
 ]

sal :: [(String, [String], [String] -> Entry)]
sal =
 [
  paradigm_h "mxc_i_dygnetrunt"   ["dygnetrunt"]      (set_pos "mxc" . compound),
  paradigm_h "sxc_i_justitie"      ["justitie"]        compound,
  paradigm_h "ab_1_fort"          ["fort"]            ab_1_fort,
  paradigm_h "ab_i_bort"          ["bort"]            ab_bort,

  paradigm_h "ab_i_aldrig"        ["aldrig"]          ab_i_aldrig,
  paradigm_h "ab_is_främst"       ["främst"]          ab_främst,

  paradigm_h "aba_i_dvs"          ["dvs"]             aba_i_dvs,
  paradigm_h "abm_i_till_exempel" ["till exempel"]    abm_i_till_exempel,
  paradigm_h "abm_u2_bakom_någons_rygg" ["bakom någons rygg"]    abm_i_till_exempel,
  paradigm_h "abm_x1_var_än"      ["hur än"]         abm_i_till_exempel,
  paradigm_h "av_0_lastgammal"    ["lastgammal"]      av_0_lastgammal,
  paradigm_h "av_0_medelstor"     ["medelstor"]       av_0_medelstor,
  paradigm_h "av_1_akut"          ["akut"]            av_1_akut,
  paradigm_h "av_1_blå"           ["blå"]   $
   (adj 0 ([e ""], [e "tt"], [e "a",e ""], [e "a",e ""], [e "are"], [e "ast"], [e "aste"])), 

  paradigm_h "av_1_bred"          ["bred"]            av_1_glad,
  paradigm_h "av_1_brydd"         ["brydd"]           av_1_högljudd,
  -- paradigm_h "av_1_grann"         ["grann"]           av_1_tunn,
  paradigm_h "av_1_gul"           ["gul"]             av_1_blek,        
  paradigm_h "av_1_lat"           ["lat"]             av_1_lat,
  paradigm_h "av_1_ny"            ["ny"]              av_1_fri,
  paradigm_h "av_1_rund"          ["rund"]            av_1_hård,
  paradigm_h "av_1_stum"          ["stum"]            av_1_ensam,
  paradigm_h "av_1_utbrunnen"     ["utbrunnen"]       av_1_angelägen,
  paradigm_h "av_i_diverse"       ["diverse"]         av_i_diverse,
  paradigm_h "in_i_aj"            ["aj"]              interj,
  paradigm_h "inm_i_aja_baja"     ["aja baja"]        interjm,
  paradigm_h "kn_i_och"           ["och"]             conj,
  paradigm_h "knm_x_ju_ju" ["både och"] $ set_pos "knm" . conj,
  
  paradigm_h "nl_g_halvannan"     ["halvannan"] $
   number ([e ""],[(tk 5,"tannat")],[],[],[e ""]),
  paradigm_h "nl_g_tu"            ["tu"] $
   number ([e ""],[e ""],[],[],[e ""]),
  paradigm_h "nl_g_tvenne"            ["tvenne"] $
   number ([e ""],[e ""],[],[],[]),
  paradigm_h "nl_i_i"             ["i"] $
   number_ng ([e ""],[e ""],[],[],[]),
  paradigm_h "nl_n_1"             ["1"] nl_n_1, 
  paradigm_h "nl_n_elva"          ["elva"] $
   number ([e ""],[e ""],[(tk 2, "fte")],[(tk 2, "fte")],[e "",(tk 2, "fte")]),
  paradigm_h "nl_n_en"            ["en"] $
   number ([e ""],[(tk 1, "tt")],[(tk 2, "första")],[(tk 2, "förste")],[e "", (tk 1, "tt"),(tk 2, "första"),(tk 2, "förste")]),
  paradigm_h "nl_n_fem"           ["fem"] $
   number ([e ""],[e ""],[e "te"],[e "te"],[e "",e "te"]),
  paradigm_h "nl_n_fyra"          ["fyra"] $
   number ([e ""],[e ""],[(tk 3, "järde")],[(tk 3,"järde")],[e "",(tk 3,"järde")]),
  paradigm_h "nl_n_hundra"        ["hundra"] $
   number ([e ""],[e ""],[e "de"],[e "de"],[e "",e "de"]),
  paradigm_h "nl_n_sex"           ["sex"] $
    number ([e ""],[e ""],[(tk 2, "jätte")],[(tk 2,"jätte")],[e "", (tk 2,"jätte")]),
  paradigm_h "nl_n_tio"           ["tio"] $
   number ([e ""],[e ""],[e "nde"],[e "nde"],[e "", e "nde"]),
  paradigm_h "nl_n_tolv"          ["tolv"] $
   number ([e ""],[e ""],[(tk 1, "fte")],[(tk 1, "fte")],[e "", (tk 1,"fte")]),
  paradigm_h "nl_n_tre"           ["tre"] $
   number ([e ""],[e ""],[e "dje"],[e "dje"],[e "", e "dje"]),
  paradigm_h "nl_n_två"           ["två"] $
   number ([e ""],[e ""],[(tk 3, "andra")],[(tk 3, "andre")],[e "",(tk 3, "andra"),(tk 3,"andre")]),
  paradigm_h "nl_n_åtta"          ["åtta"] $
   number ([e ""],[e ""],[(tk 1, "onde")],[(tk 1, "onde")],[e "",(tk 1, "onde")]),
  paradigm_h "nn_0u_tro"          ["tro"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(ds,"")],[(ds,"")]),  
  paradigm_h "nn_0u_radar" ["radar"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[e ""],[e ""]),  

  paradigm_h "nn_0u_hemsjuka" ["hemsjuka"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e"),(ds,"")],[(tk 1,"e"),(ds,"")]),

  paradigm_h "nn_1u_skyltdocka" ["skyltdocka"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"],[e "",e "e",(ds,"")],[e "",e "e",(ds,"")]),

  paradigm_h "nn_0u_saltsyra" ["saltsyra"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e"),e ""],[(tk 1,"e"),e ""]),
  
  paradigm_h "nn_1u_aminosyra" ["aminosyra"] $
   noun_compound 0 Utr ([e ""], [e "n"], [(tk 1,"or")], [(tk 1,"orna")],[(tk 1, "e"),e ""],[(tk 1,"e"),e ""]),

  paradigm_h "nn_0u_tjockolja" ["tjockolja"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "e")],[(tk 1,"e")]),

  paradigm_h "nn_0n_babbel"       ["babbel"] $
   noun_compound 0 Neutr ([e ""], [(dvu,"et")], [], [],[e ""],[e ""]),  
   paradigm_h "nn_0n_cesium"       ["cesium"] $
   noun_compound 0 Neutr ([e ""], [(tk 2,"et"),e "et", e ""], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0n_dalt"         ["dalt"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e ""],[e "", (ds, "")]),
  paradigm_h "nn_0n_koksalt" ["koksalt"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e "",(ds,"")],[e "",(ds,"")]),

  -- paradigm_h "nn_0n_landt" ["land"] $
  -- noun_compound 0 Neutr ([e ""], [e "et"], [], [],[(tk 1, "t")],[(tk 1, "t")]),

  paradigm_h "nn_0n_latin" ["latin"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[e ""],[e ""]),

  paradigm_h "nn_0n_ansvar" ["ansvar"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [], [],[(ds,"")],[(ds, "")]),

  paradigm_h "nn_0u_hälsa" ["hälsa"] $
   noun_compound 0 Utr ([e ""], [e "n"], [], [],[(tk 1, "o")],[(tk 1, "o"), (tk 1,"e")]),

  paradigm_h "nn_0n_oväsen" ["oväsen"] nn_0n_oväsen,
  paradigm_h "nn_0n_toapapper" ["toapapper"] $
   noun_f 0 Neutr ([e ""],[(dv, "et"),e "et"], [], []),  
  paradigm_h "nn_0n_raseri"       ["raseri"] $
   noun_compound 0 Neutr ([e ""],[e "et", e "t"], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0n_skum"         ["skum"]          nn_0n_skum,
  paradigm_h "nn_0n_syre"         ["syre"]          nn_0n_kaffe,
  paradigm_h "nn_0u_akribi"       ["akribi"] $
 noun_compound 0 Utr ([e ""],[e "n", e "en"], [], [],[e ""],[e ""]),  
  paradigm_h "nn_0u_antimateria"  ["antimateria"] $
   noun_f 0 Utr ([e ""], [e "n",(tk 1, "en")], [], []),  
  paradigm_h "nn_0u_samverkan"    ["samverkan"]   $
   noun_compound 0 Utr ([e ""], [e ""], [], [],[(ds, "")],[(ds,"")]),

  paradigm_h "nn_0u_skam"         ["skam"]          nn_0u_skam,
  paradigm_h "nn_0v_bikarbonat"   ["bikarbonat"] $
   noun_compound 0 Pend ([e ""], [e "en", e "et"], [], [],[e ""],[e ""]),
  paradigm_h "nn_0v_manna"        ["manna"]         nn_0v_manna,
  paradigm_h "nn_1u_ros"          ["ros"] $
   noun_compound 0 Utr ([e ""], [e "en"], [e "or"], [e "orna"], [e ""], [(ds,"")]),   
  paradigm_h "nn_2u_afton"        ["afton"] $
   noun_compound 0 Utr ([e ""], [e "en"], [(dv, "ar")], [(dv, "arna")],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_2u_bro"          ["bro"]           nn2,
  paradigm_h "nn_2u_dotter"       ["dotter"]          nn2_dotter,
  paradigm_h "nn_2u_fordran"      ["fordran"] $
   noun_compound 0 Utr ([e ""], [e ""], [(tk 2, "ingar")], [(tk 2, "ingarna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_bokanmälan"   ["bokanmälan"] $
   noun_compound 0 Utr ([e ""], [e ""], [(tk 2, "ningar")], [(tk 2, "ningarna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_fröken"       ["fröken"]          nn2_öken,
  paradigm_h "nn_2u_karl"         ["karl"]  $
   noun_f 0 Utr ([e ""], [e "n",e "en"], [e "ar"], [e "arna"]),  
  paradigm_h "nn_2u_mor"        ["mor"] nn2_moder,
  paradigm_h "nn_2u_morgon"       ["morgon"] $
   noun_f 0 Utr ([e ""], [e "en"], [(dv,"ar"),(tk 3,"nar")], [(dv,"arna"),(tk 3,"narna")]),  
  paradigm_h "nn_2u_mun"          ["mun"]             nn2_kam,
  paradigm_h "nn_3n_land"         ["land"] $
   noun_compound 0 Neutr ([e ""], [e "et"], [(vc "ä", "er")], [(vc "ä", "erna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_3n_stadium"       ["stadium"] $
             let f s = if (last s) == 'e' then s++"i" else s++"e" in
             noun_compound 2 Neutr ([e "um"], [e "et"], [e  "er"], [e "erna"],[(f,"")],[(f,"")]),
--         nn3_museum,
  paradigm_h "nn_3u_bok"          ["bok"]             nn3_bok,
  paradigm_h "nn_3u_fot"          ["fot"]             nn3_fot,
  paradigm_h "nn_3u_bockfot"      ["bockfot"]         nn3_bockfot,
  paradigm_h "nn_3u_historia"     ["historia"] $
   noun_f 0 Utr ([e ""], [(tk 1,"en"),e "n"], [(tk 1, "er")], [(tk 1 ,"erna")]),  
  paradigm_h "nn_3u_kavaljer"     ["kavaljer"]  $
   noun_compound 0 Utr ([e ""], [e "en", e "n"], [e "er"], [e "erna"],[e ""],[e "",(ds,"")]),  
  paradigm_h "nn_3u_motor"        ["motor"] $
   noun_compound 0 Utr ([e ""], [e "n"], [e "er"], [e "erna"],[e ""],[e ""]),  
  paradigm_h "nn_3u_son"          ["son"] $
   (noun_f 0 Utr ([e ""], [e "en"], [(vc "ö","er")], [(vc "ö","erna")])),  
  paradigm_h "nn_3u_stad"         ["stad"] $
   (noun_compound 0 Utr ([e ""], [e "en",(tk 1,"n")], [(vc "ä","er")], [(vc "ä","erna")],[(ds,"")],[(ds,"")])),  
  paradigm_h "nn_3u_tång"         ["tång"] $
   (noun_f 0 Utr ([e ""], [e "en"], [(vc "ä","er")], [(vc "ä","erna")])),  
  paradigm_h "nn_3u_vän"          ["vän"]             nn3_vän,
  paradigm_h "nn_3v_flanell"      ["flanell"]         nn3_flanell,
  paradigm_h "nn_4u_bonde"        ["bonde"]           nn4_bonde,
  paradigm_h "nn_5n_ansikte"      ["ansikte"] $
   noun_compound 0 Neutr ([e ""], [e "t"], [e "n"], [e "na"],[(ds_drop, "")],[(ds_drop,"")]),   
  paradigm_h "nn_5n_bo"           ["bo"] $
   (noun_f 0 Neutr ([e ""], [e "t", e "et"], [e "n"], [e "en",e "na"])), 
  paradigm_h "nn_5v_libido"           ["libido"] $
   noun_compound 0 Pend ([e ""], [e "n", e "t"], [e "n"], [e "na"],[e ""],[e ""]),   
  paradigm_h "nn_5u_anhållan"      ["anhållan"]         nn5_anmodan,
  paradigm_h "nn_6n_aber"         ["aber"] $
   (noun_f 0 Neutr ([e ""], [e "", (dv,"et"), e "et"], [e ""], [(dv, "en"),e "en"])),  
  paradigm_h "nn_6n_blad"         ["blad"] $
    noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e ""],[e "", (ds,"")]),   
  paradigm_h "nn_6n_system"         ["system"] $
    noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e ""],[e ""]),   
  paradigm_h "nn_6n_bord"         ["bord"] $
    noun_compound 0 Neutr ([e ""], [e "et"], [e ""], [e "en"],[e "",(ds,"")],[e "", (ds,"")]),   
  paradigm_h "nn_6n_foder"        ["foder"] $
    noun_compound 0 Neutr ([e ""], [(dvu, "et")], [e ""], [(dvu,"en"),e "na"],[e ""],[e ""]),   
  paradigm_h "nn_6n_frx"          ["f"]  $
     set_pos "nn" . nna Neutr ([e ""],[e "s"], [e "et"],[e "ets"],[e ""],[e "s"],[e "en"],[e "ens"]),
  paradigm_h "nn_6n_papper"       ["papper"] $
   (noun_compound 0 Neutr ([e ""], [(id,"et"),(dv,"et")], [(id,"")], [e "en", e "na", (dv,"en")],[e ""],[(ds,"")])),  
  paradigm_h "nn_6n_rum"          ["rum"]  $  
             noun_compound 0 Neutr ([e ""], [(geminate,"et")], [e ""], [(geminate,"en")],[(ds,"")],[(ds, "")]),  
  paradigm_h "nn_6n_program"     ["program"]  $  
             noun_compound 0 Neutr ([e ""], [(geminate, "et")], [e ""], [(geminate,"en")],[e ""],[e  ""]),  
-- nn6_program,


  paradigm_h "nn_6n_träd"         ["träd"] $
   (noun_f 0 Neutr ([e ""], [e "et"], [(id,""),(tk 1, "n")], [(id,"en"),(tk 1, "na")])),  
  paradigm_h "nn_6u_akademiker"   ["akademiker"]      nn6_akademiker,
  paradigm_h "nn_6u_vapenbroder" ["vapenbroder"]          nn_6u_broder,
  paradigm_h "nn_6u_anfader"        ["anfader"] $
   (noun_compound 0 Utr ([e ""], [e "n"], [(vc "ä" . tk 2, "er")],[(vc "ä" . tk 2,"erna")],[e "",(ds,"")],[e "",(ds,"")])),  
  paradigm_h "nn_6u_far"        ["far"] $
   (noun_compound 0 Utr ([e "",(tk 1,"der")], [(tk 1, "dern")], [(vc "ä" . tk 1, "der")],[(vc "ä" . tk 1,"derna")],[e "",(ds,""),(tk 1,"der"),(ds.tk 1,"der")],[e "",(ds,""),(tk 1,"der"),(ds.tk 1,"der")])),  
  paradigm_h "nn_6u_kammare"      ["kammare"] $
   (noun_compound 0 Utr ([e ""], [e "n"], [(tk 4,"rar"),(id,"")], [(tk 4,"rarna")],[(tk 1,"")],[(tk 1,"")])),  
  paradigm_h "nn_6u_kikare"       ["kikare"]  $
   noun_compound 0 Utr ([e ""], [e "n"], [e ""], [(tk 1, "na")],[(tk 1, "")],[(tk 1,"")]),   
  paradigm_h "nn_6u_mus"          ["mus"]             nn_6u_mus,
  paradigm_h "nn_6u_vaktman"      ["vaktman"]         nn_6u_vaktman,
  paradigm_h "nn_6v_borst"        ["borst"]           nn_6v_borst,
  paradigm_h "nn_7u_hit"          ["hit"] $
   noun_f 0 Utr ([e ""], [e "en"], [e "s"],[e "sen", e "sarna"]),  
  paradigm_h "nn_7u_ranger"      ["ranger"] $
   (noun_f 0 Utr ([e ""], [e "n"], [e "s"], [e "sen", e "sarna"])),  
  paradigm_h "nn_in_vaj"          ["vaj"]             nn_in_vaj,
  paradigm_h "nn_iu_vank"         ["vank"]            nn_iu_avvaktan,
  paradigm_h "nn_iv_hum"          ["hum"]             nn_iv_hum,
  paradigm_h "nn_on_öga"          ["öga"]       
   (noun_compound 0 Neutr ([e ""], [e "t"], [(tk 1,"on")], [(tk 1,"onen")],[(tk 1,"on")],[(tk 1,"on")])),  
  paradigm_h "nn_ou_officer"      ["officer"]         nn_ou_officer,
  paradigm_h "nn_vn_alfa_abc"     ["a"]  $
   replace_attr wp_attr w_attr . replace_attr h_attr w_attr . nn_vn_alfa_abc,
  paradigm_h "nn_vn_garn"         ["garn"]            nn_vn_garn,
  paradigm_h "nn_vn_huvud"        ["huvud"]           nn_vn_huvud,
  paradigm_h "nn_vn_kvantum"      ["kvantum"] $
   noun_f 0 Neutr  ([e ""], [e "et", e ""], [(tk 2,"a"),e ""], [(tk 2,"ana"),e "en"]), 
  paradigm_h "nn_vn_spektrum"     ["spektrum"]        nn_vn_spektrum,
  paradigm_h "nn_vu_blinker"      ["blinker"]         nn_vu_blinker,
  paradigm_h "nn_vu_cyklamen"     ["cyklamen"] $
   noun_f 0 Utr  ([e ""], [e ""], [(id,""), (tk 2,"er")], [(id,"a"),(tk 2,"erna")]),   
  paradigm_h "nn_vu_dress"        ["dress"]           nn_vu_dress,
  paradigm_h "nn_vu_hambo"        ["hambo"]           nn_vu_hambo,
  paradigm_h "nn_vu_kaliber"      ["kaliber"]         nn_vu_kaliber,
  paradigm_h "nn_vu_playboy"      ["playboy"]         nn_vu_playboy,
  paradigm_h "nn_vu_trio"         ["trio"]            nn_vu_trio,
  paradigm_h "nn_vv_borr"         ["borr"]            nn_vv_borr,
  paradigm_h "nn_vv_test"         ["test"]            nn_vv_test,
  paradigm_h "nna_iu_dr"          ["dr"] $
   nna Utr ([e ""],[],[],[],[],[],[],[]),
  paradigm_h "nna_iv_nxn"         ["log4/log3"] $
   nna Pend ([e ""],[],[],[],[],[],[],[]),
  paradigm_h "nna_6u_m"           ["m"] $
   nna Utr ([e ""],[e "s"], [e "en"],[e "ens"], [e ""],[e "s"],[e "en"],[e "ens"]),
  paradigm_h "pm_fph_alice"       ["Alice"]           $ pm_f_alice "ph",
  paradigm_h "pm_fph_karin"       ["Karin"]           $ pm_f "ph",
  paradigm_h "pm_fph_lisa"        ["lisa"]            $ 
   set_inhs ["f","ph"] . set_pos "pm" . noun_f 1 Utr  ([e "a"], [e "an"], [e "or"], [e "orna"]),   
  paradigm_h "pm_fpm_idun"        ["Idun"]            $ pm_f "pm",
  paradigm_h "pm_hph_berg"        ["Berg"]            $ pm_h "ph",
  paradigm_h "pm_hph_svensson"    ["Svensson"]        $ 
   set_inhs ["h","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e "en"], [(umlaut, "er")], [(umlaut,"erna")]),   
  paradigm_h "pm_mph_ansgar"      ["Ansgar"]          $ pm_m "ph",
  paradigm_h "pm_mph_bo"          ["Bo"]              $ pm_m "ph",
  paradigm_h "pm_mph_lars"        ["Lars"]            $ 
   set_inhs ["m","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e ""], [e "ar"], [e "arna"]),   
  paradigm_h "pm_mph_sture"       ["Sture"]           $ 
   set_inhs ["m","ph"] . set_pos "pm" . noun_f 0 Utr  ([e ""], [e ""], [(dv,"ar")], [(dv,"arna")]),   
  paradigm_h "pm_mpm_oden"        ["Oden"]            $ pm_m "pm",
  paradigm_h "pm_nlf_kreml"       ["Kreml"]           $ pm_n "lf",
  paradigm_h "pm_nlg_delhi"       ["Delhi"]           $ pm_n "lg",
  paradigm_h "pm_nlg_eurasien"    ["Eurasien"]        $ pm_n "lg",
  paradigm_h "pm_nlg_göteborg"    ["Göteborg"]        $ pm_n "lg",
  paradigm_h "pm_nlp_bender"      ["Bender"]          $ pm_n "lp",
  paradigm_h "pm_nlp_sverige"     ["Sverige"]         $ pm_n "lp",
  paradigm_h "pm_nog_volvo"       ["Volvo"]           $ pm_n "og",
  paradigm_h "pm_nop_centern"     ["Centern"]         $ pm_n "op",
  paradigm_h "pm_plg_alperna"     ["Alperna"]         $ pm_p "lg",
  paradigm_h "pm_uag_saab"        ["Saab"]            $ pm_u "ag",
  paradigm_h "pm_ula_månen"       ["Månen"]           $ pm_u "la",
  paradigm_h "pm_ulg_fyris"       ["Fyris"]           $ pm_u "lg",
  paradigm_h "pm_uwb_hemsöborna"  ["Hemsöborna"]      $ pm_u "wb",
  paradigm_h "pm_uwc_faust"                   ["Faust"]           $ pm_u "wc",
  paradigm_h "pm_uwn_aftonbladet"             ["Aftonbladet"]     $ pm_u "wn",
  paradigm_h "pm_vlf_globen"                  ["Globen"]          $ pm_v "lf",
  paradigm_h "pma_nog_fn"                     ["FN"]                    $ pma_n "og",
  paradigm_h "pma_nop_cuf"                    ["CUF"]                   $ pma_n "op",
  paradigm_h "pmm_h0ph_de_saussure"           ["de Saussure"]           $ noc $ pmm_h "ph",
  paradigm_h "pmm_m0ph_bo_ek"                 ["Bo Ek"]                 $ noc $ pmm_m "ph",
  paradigm_h "pmm_n0lg_new_delhi"             ["New Delhi"]             $ noc $ pmm_n "lg",
  paradigm_h "pmm_n0lg_svarta_havet"          ["Svarta havet"]          $ pmm_n "lg",
  paradigm_h "pmm_n0lp_sri_lanka"             ["Sri Lanka"]             $ pmm_n "lp",
  paradigm_h "pmm_n0oe_göteborgs_universitet" ["Göteborgs universitet"] $ pmm_n "oe",
  paradigm_h "pmm_n0og_nordiska_rådet"        ["Nordiska rådet"]        $ pmm_n "og",
  paradigm_h "pmm_u0lg_torne_älv"             ["Torne älv"]             $ pmm_u "lg",
  paradigm_h "pmm_u0wb_det_går_an"            ["Quo vadis?"]            $ pmm_u "wb",
  paradigm_h "pmm_n0wm_ring_p1"               ["Ring P1"]           $ pmam_n "wm",
  paradigm_h "pmm_u0ec_alla_hjärtans_dag"     ["Alla hjärtans dag"] $ pmm_u "ec",
  paradigm_h "pmm_u0og_svenska_akademien"     ["Svenska Akademien"] $ pmm_u "og",
  paradigm_h "pp_i_i"                         ["i"]                   prep,
  -- vbm_paradigm "vbm_1ai1_laga" ["tappa sugen"],

  vbm_paradigm "vbm_2ad1_viga" ["höja till skyarna"],     

  vbm_paradigm "vbm_1ad1_laga" ["jämna med marken"],     
  vbm_paradigm "vbm_2md1_hyra" ["föra bakom ljuset"],     
  vbm_paradigm "vbm_2msl1_lägga" ["lägga sig platt"],     
  vbm_paradigm "vbm_4ad1_rida" ["skrida till verket"],     
  vbm_paradigm "vbm_4ma1_komma" ["komma undan"],     
  vbm_paradigm "vbm_4mds1_dra" ["dra öronen åt sig"],     
  vbm_paradigm "vbm_4ml1_tillåta" ["låta bli"],     
  vbm_paradigm "vbm_4ms1_dra" ["dra sig"],     
  vbm_paradigm "vbm_4mt1_gå" ["gå sin gilla gång"],     

  vbm_paradigm "vbm_2ms1_sälja" ["sälja sig"],

  vbm_paradigm "vbm_2mt1_göra" ["göra slag i saken"],
  vbm_paradigm "vbm_4al1_ta" ["ta sönder"],
  vbm_paradigm "vbm_4at1_ta" ["ta på bar gärning"],
  vbm_paradigm "vbm_vms1_besluta" ["besluta sig"],                          

  vbm_paradigm "vbm_2ml1_ansöka" ["väcka uppmärksamhet"],
  vbm_paradigm "vbm_4md1_hålla" ["hålla låg profil"],
  vbm_paradigm "vbm_4mt1_hålla" ["hålla en låg profil"],

  vbm_paradigm "vbm_1ic_laga" ["handla och vandla"],
  vbm_paradigm "vbm_4mpl1_ge" ["ge upp andan"],
  vbm_paradigm "vbm_1mpt1_laga" ["surfa in på en räkmacka"],
  vbm_paradigm "vbm_1mq1_laga"  ["trampa på en öm tå"],
  vbm_paradigm "vbm_1mst1_laga" ["visa sig på styva linan"],
  vbm_paradigm "vbm_1md1_laga" ["råka illa ut"],
  vbm_paradigm "vbm_1md1_ansöka" ["klippa med ögonen"],
  vbm_paradigm "vbm_2md1_viga" ["ställa till svars"],
  vbm_paradigm "vbm_1ml1_laga" ["vädra morgonluft"],
  vbm_paradigm "vbm_1mp1_laga" ["lämna över"],
  vbm_paradigm "vbm_1sp2_andas" ["inte låssas om"],
  vbm_paradigm "vbm_1mps1_laga" ["tuffa till sig"],
  vbm_paradigm "vbm_1ms1_laga" ["överila sig"],
  vbm_paradigm "vbm_1msp1_laga" ["snigla sig fram"],
  vbm_paradigm "vbm_1mzd1_laga" ["omgjorda sina länder"],
  vbm_paradigm "vbm_1sp1_andas" ["turas om"],
  vbm_paradigm "vbm_1szt1_andas" ["utandas sin sista suck"],
  vbm_paradigm "vbm_2ad1_lägga" ["lägga i dagen"],
  vbm_paradigm "vbm_2ai1_ansöka" ["släppa gäcken lös"],
  vbm_paradigm "vbm_2al1_lägga" ["lägga emellan"],
  vbm_paradigm "vbm_2al1_sätta" ["sätta p"],
  vbm_paradigm "vbm_2md1_ha" ["ha på tråden"],
  vbm_paradigm "vbm_2md1_ansöka" ["tänka på refrängen"],
  vbm_paradigm "vbm_2md1_lägga" ["lägga på hullet"],
  vbm_paradigm "vbm_2mi1_lägga" ["lägga näsan i blöt"],
  --vbm_paradigm "vbm_2mi1_mysa" ["åka på en propp"],
  vbm_paradigm "vbm_2mi2_leda" ["inte skräda orden"],
  vbm_paradigm "vbm_2ml1_göra" ["göra susen"],
  vbm_paradigm "vbm_2ml1_ha" ["ha reda"],
  vbm_paradigm "vbm_2ml1_leda" ["skräda orden"],
  vbm_paradigm "vbm_2ml1_lägga" ["inlägga förtjänst"],
  --vbm_paradigm "vbm_2ml1_väga" ["fylla år"],
  --vbm_paradigm "vbm_2mlvs1_mysa" ["leka rommen av sig"],
  vbm_paradigm "vbm_2mp1_göra" ["göra åt"],
  vbm_paradigm "vbm_2mp1_hyra" ["höra till"],
  vbm_paradigm "vbm_2mp1_känna" ["känna till"],
  vbm_paradigm "vbm_2mp1_lägga" ["lägga ut"],
  -- vbm_paradigm "vbm_2mp1_mysa" ["tänka till"],
  vbm_paradigm "vbm_2mp1_sätta" ["sätta efter"],
  vbm_paradigm "vbm_2mp1_viga" ["hänga över"],
  vbm_paradigm "vbm_1mt1_laga" ["ana ugglor i mossen"],
  vbm_paradigm "vbm_1mvud1_laga" ["dansa efter någons pipa"],
  vbm_paradigm "vbm_1mvzd1_laga" ["peta i sin mat"],
  vbm_paradigm "vbm_2mi1_ansöka" ["åka på en propp"],
  vbm_paradigm "vbm_2ml1_viga" ["bygga broar"],
  vbm_paradigm "vbm_2mld1_viga" ["bygga bo i huvudet"],
  vbm_paradigm "vbm_2mlvs1_ansöka" ["leka rommen av sig"],
  vbm_paradigm "vbm_2mp1_ansöka" ["blåsa upp"],
  vbm_paradigm "vbm_2mps1_ansöka" ["smäcka i sig"],
  vbm_paradigm "vbm_2mrf1_hyra" ["begära någons huvud på ett fat"],
  vbm_paradigm "vbm_2msp1_ansöka" ["tänka sig för"],
  vbm_paradigm "vbm_2msp1_viga" ["ställa sig in"],
  vbm_paradigm "vbm_4md1_angå" ["få på moppe"],
  vbm_paradigm "vbm_4mi1_angå" ["få bära hundhuvudet"],
  vbm_paradigm "vbm_4ml1_angå" ["få bukt"],
  vbm_paradigm "vbm_4ml1_rida" ["knipa käft"],
  vbm_paradigm "vbm_4ml1_ryta" ["stryka flagg"],
  vbm_paradigm "vbm_4mpd1_gå" ["gå in i väggen"],
  vbm_paradigm "vbm_4mvrd1_gå" ["gå i någons ledband"],    
  vbm_paradigm "vbm_4mzt1_angå" ["få sina fiskar varma"],

  --vbm_paradigm "vbm_2mp1_väga" ["äga rum"],
  vbm_paradigm "vbm_2mps1_göra" ["göra på sig"],
  vbm_paradigm "vbm_2mps1_ha" ["ha för sig"],
  vbm_paradigm "vbm_2mps1_hyra" ["köra ihop sig"],
  vbm_paradigm "vbm_2mps1_känna" ["skämma ut sig"],
  --vbm_paradigm "vbm_2mps1_mysa" ["smäcka i sig"],
  vbm_paradigm "vbm_2mps1_sätta" ["sätta i sig"],
  vbm_paradigm "vbm_2mps1_viga" ["hänga upp sig"],
  -- vbm_paradigm "vbm_2mps1_väga" ["ställa om sig"],
  vbm_paradigm "vbm_2ms1_ansöka" ["stöta sig"],
  vbm_paradigm "vbm_2ms1_glädja" ["glädja sig"],
  vbm_paradigm "vbm_2ms1_göra" ["tillgodogöra sig"],
  vbm_paradigm "vbm_2ms1_hyra" ["uppföra sig"],
  vbm_paradigm "vbm_2ms1_hända" ["vända sig"],
  vbm_paradigm "vbm_2ms1_känna" ["missminna sig"],
  vbm_paradigm "vbm_2ms1_leda" ["reda sig"],
  vbm_paradigm "vbm_2ms1_lyfta" ["utfästa sig"],
  vbm_paradigm "vbm_2ms1_lägga" ["vinnlägga sig"],
  --vbm_paradigm "vbm_2ms1_mysa" ["vräka sig"],
  vbm_paradigm "vbm_2ms1_skilja" ["skilja sig"],
  vbm_paradigm "vbm_2ms1_säga" ["säga sig"],
  vbm_paradigm "vbm_2ms1_sätta" ["sätta sig"],
  vbm_paradigm "vbm_2ms1_viga" ["hänga sig"],
  --vbm_paradigm "vbm_2ms1_väga" ["åtnöja sig"],
  vbm_paradigm "vbm_2ms1_välja" ["vänja sig"],
  vbm_paradigm "vbm_2msl1_göra" ["göra sig kvitt"],
  vbm_paradigm "vbm_2msp1_göra" ["göra sig till"],
  vbm_paradigm "vbm_2msp1_hyra" ["höra sig för"],
  vbm_paradigm "vbm_2msp1_lägga" ["lägga sig vinn"],
  --vbm_paradigm "vbm_2msp1_mysa" ["tänka sig för"],
  vbm_paradigm "vbm_2msp1_sätta" ["sätta sig över"],
  --vbm_paradigm "vbm_2msp1_väga" ["leva sig in"],
  vbm_paradigm "vbm_2mzd1_göra" ["göra sina lärospån"],
  vbm_paradigm "vbm_2mzd1_sätta" ["sätta sin lit"],
  vbm_paradigm "vbm_2sp1_blygas" ["följas åt"],
  vbm_paradigm "vbm_2sp1_minnas" ["kännas vid"],
  vbm_paradigm "vbm_2sp1_synas" ["hjälpas åt"],
  vbm_paradigm "vbm_3mp1_sy" ["rå om"],
  vbm_paradigm "vbm_3ms1_sy" ["ty sig"],
  vbm_paradigm "vbm_3msp1_sy" ["bry sig om"],
  vbm_paradigm "vbm_3mzd1_sy" ["så sin vildhavre"],
  vbm_paradigm "vbm_4ad1_bära" ["bära i gullstol"],
  vbm_paradigm "vbm_4ad1_falla" ["falla till föga"],
  vbm_paradigm "vbm_4ad1_ge" ["ge till känna"],
  vbm_paradigm "vbm_4ad1_gå" ["gå om intet"],
  vbm_paradigm "vbm_4ad1_komma" ["komma till skott"],
  vbm_paradigm "vbm_4ad1_se" ["se i vitögat"],
  vbm_paradigm "vbm_4ad1_ta" ["ta till vara"],
  vbm_paradigm "vbm_4al1_dricka" ["vinna segerpalmen"],

  vbm_paradigm "vbm_4al1_ge" ["ge tillkänna"],
  -- vbm_paradigm "vbm_4al1_gå" ["gå omintet"],
  vbm_paradigm "vbm_4md1_ge" ["ge till spillo"],

  -- vbm_paradigm "vbm_4al1_gå" ["gå kjortelvägen"],
  vbm_paradigm "vbm_4al1_komma" ["komma ifråga"],
  vbm_paradigm "vbm_4al1_rida" ["rida barbacka"],
  vbm_paradigm "vbm_4l1_slå" ["slå mynt"],
  vbm_paradigm "vbm_4md1_flyga" ["krypa till korset"],
  vbm_paradigm "vbm_4md1_gå" ["gå till väga"],
  vbm_paradigm "vbm_4ad1_hålla" ["hålla i schack"],
  vbm_paradigm "vbm_4md1_komma" ["komma till kritan"],
  vbm_paradigm "vbm_4md1_le" ["le i mjugg"],
  vbm_paradigm "vbm_4md1_ligga" ["ligga på lur"],
  vbm_paradigm "vbm_4md1_sitta" ["sitta på understol"],
  vbm_paradigm "vbm_4md1_slå" ["slå en drill"],
  vbm_paradigm "vbm_4md1_stå" ["stå till svars"],
  vbm_paradigm "vbm_4md1_ta" ["ta till orda"],
  vbm_paradigm "vbm_4md1_vina" ["skita på sig"],
  vbm_paradigm "vbm_4mi1_bära" ["bära syn för sagen"],
  vbm_paradigm "vbm_4mi1_ge" ["ge bagarbarn bröd"],
  --vbm_paradigm "vbm_4mi1_gå" ["få bära hundhuvudet"],
  vbm_paradigm "vbm_4mzq1_ta" ["ta sin mats ur skolan"],
  --vbm_paradigm "vbm_4mzt1_gå" ["få sina fiskar varma"],
  vbm_paradigm "vbm_vmsp1_ryckas" ["ryckas bort"],
  vbm_paradigm "vbm_4mi1_vina" ["rida ranka"],
  vbm_paradigm "vbm_4ml1_ge" ["ge vika"],
  vbm_paradigm "vbm_4ml1_gå" ["gå miste"],
  vbm_paradigm "vbm_4ml1_hålla" ["hålla stången"],
  vbm_paradigm "vbm_4ml1_komma" ["komma underfund"],
  vbm_paradigm "vbm_4ml1_ligga" ["ligga ogill"],
  vbm_paradigm "vbm_4ml1_ljuda" ["ljuta döden"],
  vbm_paradigm "vbm_4ml1_slå" ["slå vad"],
  vbm_paradigm "vbm_4ml1_stå" ["stå rycken"],
  vbm_paradigm "vbm_4ml1_ta" ["ta betäckning"],
  vbm_paradigm "vbm_4mlp1_slå" ["slå dövörat till"],
  vbm_paradigm "vbm_4mp1_bli" ["bli kvar"],
  vbm_paradigm "vbm_4mp1_bära" ["skära för"],
  vbm_paradigm "vbm_4mp1_fara" ["fara ut"],
  vbm_paradigm "vbm_4mp1_flyga" ["bryta av"],
  vbm_paradigm "vbm_4mp1_le" ["dö ut"], 
  vbm_paradigm "vbm_4ms1_skjuta" ["utgjuta sig"],
  vbm_paradigm "vbm_4mp1_gå" ["gå till väga"],
  vbm_paradigm "vbm_4mp1_hålla" ["hålla upp"],
  vbm_paradigm "vbm_4mp1_komma" ["komma vid"],
  vbm_paradigm "vbm_4mp1_ryta" ["stryka med"],
  vbm_paradigm "vbm_4mp1_se" ["se ut"],
  vbm_paradigm "vbm_4mp1_sitta" ["sticka upp"],
  vbm_paradigm "vbm_4mp1_skjuta" ["skjuta över"],
  vbm_paradigm "vbm_4mp1_slå" ["slå över"],
  vbm_paradigm "vbm_4mp1_stå" ["stå ut"],
  vbm_paradigm "vbm_4mp1_ta" ["ta överbalansen"],
  vbm_paradigm "vbm_4mp1_vara" ["vara till"],
  vbm_paradigm "vbm_4mp1_vina" ["lida pin"],
  vbm_paradigm "vbm_4mpd1_ta" ["ta i med hårdhandskarna"],
  vbm_paradigm "vbm_4mpl1_ta" ["ta till storsläggan"],
  vbm_paradigm "vbm_4mps1_ge" ["ge med sig"],
  vbm_paradigm "vbm_4mps1_hålla" ["hålla i sig"],
  vbm_paradigm "vbm_4mps1_komma" ["komma ihop sig"],
  vbm_paradigm "vbm_4mps1_skjuta" ["skjuta in sig"],
  vbm_paradigm "vbm_4mps1_skåpäta" ["äta upp sig"],
  vbm_paradigm "vbm_4mps1_slå" ["slå ifrån sig"],
  vbm_paradigm "vbm_4mpzd1_slå" ["slå ned sina bopålar"],
  vbm_paradigm "vbm_4ms1_angå" ["förgå sig"],
  vbm_paradigm "vbm_4ms1_be" ["utbe sig"],
  vbm_paradigm "vbm_4ms1_bära" ["skära sig"],
  vbm_paradigm "vbm_4ms1_förhålla" ["uppehålla sig"],
  vbm_paradigm "vbm_4ms1_ge" ["hänge sig"],
  vbm_paradigm "vbm_4ms1_gråta" ["utlåta sig"],
  vbm_paradigm "vbm_4ms1_hålla" ["hålla sig"],
  vbm_paradigm "vbm_4ms1_komma" ["komma sig"],
  vbm_paradigm "vbm_4ms1_ljuda" ["ansluta sig"],
  vbm_paradigm "vbm_4ms1_ryta" ["snyta sig"],
  vbm_paradigm "vbm_4ms1_sitta" ["utspinna sig"],
  vbm_paradigm "vbm_4ms1_skåpäta" ["föräta sig"],
  vbm_paradigm "vbm_4ms1_slå" ["slå sig"],
  vbm_paradigm "vbm_4ms1_sova" ["försova sig"],
  vbm_paradigm "vbm_4ms1_stå" ["understå sig"],
  vbm_paradigm "vbm_4ms1_ta" ["åta sig"],
  vbm_paradigm "vbm_4ms1_tillåta" ["nedlåta sig"],
  vbm_paradigm "vbm_4ms1_utfalla" ["undfalla sig"],
  vbm_paradigm "vbm_4ms1_vina" ["slita sig"],
  vbm_paradigm "vbm_4msd1_ge" ["ge sig till tåls"],
  vbm_paradigm "vbm_4msd1_slå" ["slå sig till ro"],
  vbm_paradigm "vbm_4msp1_bära" ["bära sig åt"],
  vbm_paradigm "vbm_4msp1_ge" ["ge sig ut"],
  vbm_paradigm "vbm_4msp1_komma" ["komma sig upp"],
  vbm_paradigm "vbm_4msp1_slå" ["slå sig på"],
  vbm_paradigm "vbm_4msp1_stå" ["förstå sig på"],
  vbm_paradigm "vbm_4msp1_ta" ["ta sig ut"],
  vbm_paradigm "vbm_4msp1_finnas" ["finnas till"],
  vbm_paradigm "vbm_omp1_vilja" ["vilja till"],
  vbm_paradigm "vbm_omp1_veta" ["veta av"],
  vbm_paradigm "vbm_oms1_vilja" ["vilja sig"],
  vbm_paradigm "vbm_val1_klä" ["trå dansen"],
  vbm_paradigm "vbm_vad1_bringa" ["bringa till världen"],
  vbm_paradigm "vbm_vml1_mala" ["mala tomning"],
  vbm_paradigm "vbm_vmp1_smälta" ["smälta in"],
  vbm_paradigm "vbm_vmp1_upphäva" ["häva upp"],
  vbm_paradigm "vbm_vmps1_klä" ["klä ut sig"],
  vbm_paradigm "vbm_vmps1_smälla" ["smälla i sig"],
  vbm_paradigm "vbm_vmps1_vika" ["vika ut sig"],
  vbm_paradigm "vbm_vmps1_växa" ["växa till sig"],
  vbm_paradigm "vbm_vms1_besvärja" ["sammansvärja sig"],
  vbm_paradigm "vbm_vms1_förlöpa" ["belöpa sig"],
  vbm_paradigm "vbm_vms1_klä" ["utbre sig"],
  vbm_paradigm "vbm_vms1_tvinga" ["tilltvinga sig"],
  -- vbm_paradigm "vbm_vsp1_ryckas" ["ryckas bort"],
  vbm_paradigm "vbm_vsp1_talas" ["talas vid"],

  vbm_paradigm "vbm_4apl1_ta" ["ta ner skylten"],

  combine_vbm (vbm_paradigm "vbm_4ap1_falla" ["falla bort"])
               (paradigm_h  "vbm_4ap1_falla" ["falla bort"]
                 (verb_vbm 0 ([e ""], [], [], [], [],  [(tk 1 . fl, "en")]))),

  combine_vbm (vbm_paradigm "vbm_4ap1_sitta" ["sticka ner"])
               (paradigm_h  "vbm_4ap1_sitta" ["sticka ner"]
                 (verb_vbm 0 ([e ""], [], [], [], [],  [(vc "u" . tk 1 . fl, "en")]))),

  combine_vbm (vbm_paradigm "vbm_1ap1_laga" ["pigga upp"]) 
               (paradigm_h "vbm_1ap1_laga" ["pigga upp"]             
                 (verb_vbm 0 ([e ""], [], [], [], [], [(fl, "d")]))),

  combine_vbm (vbm_paradigm "vbm_4ap1_dricka" ["sitta av"]) 
               (paradigm_h "vbm_4ap1_dricka" ["sitta av"]             
                 (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl, "en")]))),

  combine_vbm (vbm_paradigm "vbm_1ap1_viga" ["slänga bort"])
              (paradigm_h "vbm_1ap1_viga" ["slänga bort"]              
               (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl, "d")]))),

  combine_vbm (vbm_paradigm "vbm_1ap1_skapa" ["tjäna av"])
              (paradigm_h "vbm_1ap1_skapa" ["tjäna av"]              
               (verb_vbm 0 ([e ""], [], [], [], [], [(fl, "d")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_ansöka" ["knäppa upp"])
                  (paradigm_h "vbm_2ap1_ansöka" ["knäppa upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl, "t")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_göra" ["göra ned"])
                  (paradigm_h "vbm_2ap1_göra" ["göra ned"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 3 . fl, "jord")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_hyra" ["hyra ut"])
                  (paradigm_h "vbm_2ap1_hyra" ["hyra ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl, "d")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_känna" ["skämma ut"])
                  (paradigm_h "vbm_2ap1_känna" ["skämma ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(ungeminate_m_n . tk 1 .fl,"d")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_leda" ["reda ut"])
                  (paradigm_h "vbm_2ap1_leda" ["reda ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"d")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_lyfta" ["gifta bort"])
                  (paradigm_h "vbm_2ap1_lyfta" ["gifta bort"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1.fl,"")]))),
  
  combine_vbm (vbm_paradigm "vbm_2ap1_lägga" ["lägga undan"])
                  (paradigm_h "vbm_2ap1_lägga" ["lägga undan"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "a" . tk 2 . fl,"d")]))),
   
  combine_vbm (vbm_paradigm "vbm_2ap1_säga" ["säga upp"])
                  (paradigm_h "vbm_2ap1_säga" ["säga upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "a" . tk 1 . fl,"d")]))),
  
  combine_vbm (vbm_paradigm "vbm_2ap1_sända" ["tända på"])
                  (paradigm_h "vbm_2ap1_sända" ["tända på"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"")]))),

  combine_vbm (vbm_paradigm "vbm_2ap1_sätta" ["sätta av"])
                  (paradigm_h "vbm_2ap1_sätta" ["sätta av"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "a" . tk 1 . fl,"")]))),
   
  combine_vbm (vbm_paradigm "vbm_2ap1_viga" ["döma ut"])
                  (paradigm_h "vbm_2ap1_viga" ["döma ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1.fl,"d")]))),
   
  --- combine_vbm (vbm_paradigm "vbm_2ap1_väga" ["leva ut"])
  ---                (paradigm_h "vbm_2ap1_väga" ["leva ut"]              
  ---                 (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"d")]))),
    
  combine_vbm (vbm_paradigm "vbm_3ap1_sy" ["sy in"])
                  (paradigm_h "vbm_3ap1_sy" ["sy in"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"dd")]))),
  
  combine_vbm (vbm_paradigm  "vbm_4ap1_bestå" ["stå bi"])
                  (paradigm_h "vbm_4ap1_bestå" ["stå bi"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"dd")]))),
  
  combine_vbm (vbm_paradigm "vbm_4ap1_bjuda" ["hugga ned"])
                  (paradigm_h "vbm_4ap1_bjuda" ["hugga ned"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1. fl,"en")]))),

  combine_vbm (vbm_paradigm "vbm_4ap1_bli" ["bli varse"])
                  (paradigm_h "vbm_4ap1_bli" ["bli varse"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"ven")]))),
   
  combine_vbm (vbm_paradigm "vbm_4ap1_bära" ["skära ner"])
                  (paradigm_h "vbm_4ap1_bära" ["skära ner"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1. fl,"en")]))),
     
  -- combine_vbm (vbm_paradigm "vbm_4ap1_dö" ["dö ut"])
  --                (paradigm_h "vbm_4ap1_dö" ["dö ut"] 
  --                 (set_pos "vbm" . vbm_dö_ut)),
  
  combine_vbm (vbm_paradigm "vbm_4ap1_flyga" ["bryta ut"])
                  (paradigm_h "vbm_4ap1_flyga" ["bryta ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl,"en")]))),
     
  combine_vbm (vbm_paradigm "vbm_4ap1_ge" ["ge upp"])
                  (paradigm_h "vbm_4ap1_ge" ["ge upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "i" . fl,"ven")]))),
     
  combine_vbm (vbm_paradigm "vbm_4ap1_gå" ["gå bort"])
                  (paradigm_h "vbm_4ap1_gå" ["gå bort"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"ngen")]))),
  
  combine_vbm (vbm_paradigm "vbm_4ap1_hålla" ["hålla av"])
                  (paradigm_h "vbm_4ap1_hålla" ["hålla av"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1.fl,"en")]))),

  combine_vbm (vbm_paradigm "vbm_4ap1_komma" ["komma ihåg"])
                  (paradigm_h "vbm_4ap1_komma" ["komma ihåg"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"en")]))),
   
  combine_vbm (vbm_paradigm "vbm_4ap1_rida" ["slita ut"])
                  (paradigm_h "vbm_4ap1_rida" ["slita ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"en")]))),
     
  combine_vbm (vbm_paradigm "vbm_4ap1_se" ["se till"])
                  (paradigm_h "vbm_4ap1_se" ["se till"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"dd")]))),
 
  combine_vbm (vbm_paradigm "vbm_4ap1_skjuta" ["skjuta in"])
                  (paradigm_h "vbm_4ap1_skjuta" ["skjuta in"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl, "en")]))),
    
  combine_vbm (vbm_paradigm  "vbm_4ap1_slå" ["slå ner"])
                  (paradigm_h "vbm_4ap1_slå" ["slå ner"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "a" . fl,"gen")]))),
     
  combine_vbm (vbm_paradigm "vbm_4ap1_ta" ["dra in"])
                  (paradigm_h "vbm_4ap1_ta" ["dra in"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"gen")]))),

  combine_vbm (vbm_paradigm "vbm_4ap1_tillåta" ["låta upp"])
                  (paradigm_h "vbm_4ap1_tillåta" ["låta upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"en")]))),
    
  combine_vbm (vbm_paradigm "vbm_4ap1_äta" ["äta upp"])
                  (paradigm_h "vbm_4ap1_äta" ["äta upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"en")]))),
  
  combine_vbm (vbm_paradigm "vbm_vap1_fnysa" ["dyka upp"])
                  (paradigm_h "vbm_vap1_fnysa" ["dyka upp"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"t")]))),
  
  combine_vbm (vbm_paradigm "vbm_vap1_frysa" ["frysa ut"])
                  (paradigm_h "vbm_vap1_frysa" ["frysa ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl, "en")]))),
    
  combine_vbm (vbm_paradigm "vbm_vap1_klä" ["klä ut"])
                  (paradigm_h "vbm_vap1_klä" ["klä ut"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(fl,"dd")]))),
   
  combine_vbm (vbm_paradigm "vbm_vap1_koka" ["koka in"])
                  (paradigm_h "vbm_vap1_koka" ["koka in"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"t")]))),
  
  combine_vbm (vbm_paradigm "vbm_vap1_lyfta" ["smälta ihop"])
                  (paradigm_h "vbm_vap1_lyfta" ["smälta ihop"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"")]))),
   
  combine_vbm (vbm_paradigm "vbm_vap1_nypa" ["nypa av"])
                  (paradigm_h "vbm_vap1_nypa" ["nypa av"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"t")]))),
 
  combine_vbm (vbm_paradigm "vbm_vap1_smälla" ["smälla av"])
                  (paradigm_h "vbm_vap1_smälla" ["smälla av"]              
                   (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl,"en")]))),
 
  combine_vbm (vbm_paradigm "vbm_vap1_smälta" ["smälta samman"])
               (paradigm_h "vbm_vap1_smälta" ["smälta samman"]              
                (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl,"en")]))),
 
  combine_vbm (vbm_paradigm "vbm_vap1_strypa" ["rycka fram"])              
               (paradigm_h "vbm_vap1_strypa" ["rycka fram"]              
                (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"t")]))),
   
  combine_vbm (vbm_paradigm "vbm_vap1_tala" ["spela upp"])
               (paradigm_h "vbm_vap1_tala" ["spela upp"]              
                (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"t")]))),
   
  combine_vbm (vbm_paradigm "vbm_vap1_vika" ["vika ut"])
               (paradigm_h "vbm_vap1_vika" ["vika ut"]              
                (verb_vbm 0 ([e ""], [], [], [], [], [(tk 1 . fl,"en")]))),
    
  combine_vbm (vbm_paradigm "vbm_vap1_växa" ["växa ur"])
               (paradigm_h "vbm_vap1_växa" ["växa ur"]              
                (verb_vbm 0 ([e ""], [], [], [], [], [(vc "u" . tk 1 . fl,"en")]))),

  paradigm_h "nn_1u_flicka"       ["flicka"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [(ungeminate, "")],[e "e",(ungeminate,""),(ds.ungeminate,"")]),

  paradigm_h "nn_1u_barnstuga" ["barnstuga"] $
   noun_compound 1 Utr ([e "a"], [e "an"], [e "or"], [e "orna"], [e "e", e "u"],[e "e",e "u"]),

  paradigm_h "nn_vv_demo" ["demo"] $
   noun_compound 0 Pend 
    ([e ""], [e "n", e "t"], [e "n",e "s",e "sar",e "r", e ""], [e "na",e "sen",e "sarna",e "rna", e "n"],[e ""],[e ""]),

  paradigm_h "nn_2u_botten" ["botten"] $
   noun_compound 0 Utr  ([e ""], [(dvu,"en"),e ""], [(dvu, "ar")], [(dvu,"arna")],[e ""],[e ""]),   
  paradigm_h "nn_2u_nyckel" ["nyckel"] $
    noun_compound 0 Utr ([e ""], [e "n"], [(mmr.dvu,"ar"),(mmr.dv, "ar")], [(mmr.dvu,"arna"),(mmr.dv,"arna")],[e ""],[e "",(ds,"")]),   

  paradigm_h "nn_2u_ålder" ["ålder"] $
    noun_compound 0 Utr ([e ""], [e "n"], [(dvu,"ar"),(dv, "ar")], [(dvu,"arna"),(dv,"arna")],[(ds,"")],[(ds,"")]),   
  paradigm_h "nn_2u_vinge"         ["vinge"] $
   noun_compound 0 Utr  ([e ""], [e "n"], [(tk 1,"ar")], [(tk 1, "arna")],[(ungeminate.tk 1, "")],[(ungeminate.tk 1,""),e "s",(ds. ungeminate . tk 1,"")]),   

  paradigm_h "nn_2u_skrake" ["skrake"] $
   noun_compound 0 Utr  ([e "",(tk 1,"")], [e "n"], [(tk 1,"ar")], [(tk 1, "arna")],[(ungeminate.tk 1, "")],[(ungeminate.tk 1,""),e "s",(ds. ungeminate . tk 1,"")]),   

  paradigm_h "nn_2u_stol"            ["stol"] $
   noun_compound 0 Utr  ([e ""], [e "en"], [e "ar"], [e "arna"],[e "",(ds,"")],[e "",(ds,"")]),   
  paradigm_h "nn_4u_linje"           ["linje"]       nn4,
  paradigm_h "nn_5n_knä"            ["knä"]         nn5_knä,
  paradigm_h "nn_6n_garage"       ["garage"] $ 
   noun_compound 0 Neutr  ([e ""], [e "t"], [e ""], [e "n"],[e ""],[e ""]),   
  paradigm_h "nn_6n_segel"         ["segel"] $
   noun_compound 0 Neutr  ([e ""], [(dvu,"et")], [e ""], [(dvu,"en")],[e ""],[e "",(ds,"")]),
   
  paradigm_h "nn_6n_medel"         ["medel"] $
   noun_compound 0 Neutr  ([e ""], [(dvu,"et")], [e ""], [(dvu,"en")],[(ds,"")],[(ds,"")]),   

  paradigm_h "nn_6u_gås"            ["gås"]            nn_6u_gås,
  paradigm_h "nn_ou_emeritus"    ["emeritus"]       nn_ou_emeritus, 
  paradigm_h "nn_ou_examen"     ["examen"]         nn_ou_examen,
  paradigm_h "nn_vn_centrum"     ["centrum"]        nn_vn_centrum,
  paradigm_h "nn_vn_nomen"       ["nomen"]          nn_vn_nomen,
  paradigm_h "nn_vu_jojo"            ["jojo"]       nn_vu_jojo,
  paradigm_h "nn_vu_partner"      ["partner"]        nn_vu_partner,
  paradigm_h "nn_vv_abdomen"   ["abdomen"]       nn_vv_abdomen,
  paradigm_h "av_2_ung"          ["ung"]        av_2_ung
 ] ++ verb_paradigms

combine_vbm :: (String, [String], [String] -> Entry) -> (String, [String], [String] -> Entry) -> (String, [String], [String] -> Entry)
combine_vbm (s,xs,f) (_,_,f1) = (s,xs,\xs -> combine_tables (f xs) (f1 xs))

vbm_paradigm :: String -> [String] -> (String, [String], [String] -> Entry)
vbm_paradigm p xs = case catMaybes [Map.lookup v_id verb_map | v_id <- vids] of
                      _ | pos /='1' -> paradigm_h p xs vbm_inf
                      (f:_) -> paradigm_h p xs $ expand . clean . first_mw "vbm" (f . (:[]))
                      _     -> paradigm_h p xs vbm_inf
 where verb_map = Map.fromList [(s,f) | (s,_,f) <- verb_paradigms]
       clean = (if is_reflexive p then remove_param "s-form" else id) . remove_param "sms" . remove_param "c" 
               . (if has_pret_part then id else remove_param "pret_part")
       expand = (if is_poss p then expand_poss else (if is_reflexive p then expand_ref else id))
       expand_poss = expand_multiword "sin" [([],[],"min"),([],[],"din"),([],[],"sin"),([],[],"vår"),([],[],"er")] .
                     expand_multiword "sina" [([],[],"mina"),([],[],"dina"),([],[],"sina"),([],[],"våra"),([],[],"era")]
       expand_ref  = expand_multiword "sig" [([],["imper"],"mig"),([],[],"dig"),([],["imper"],"sig"),([],["imper"],"oss"),([],[],"er")]
       vids = case p of
                (v:b:m:u:k:a:rest) ->
                    case span (/= '_') rest of
                      (_,r) -> [(v:b:u:k:a:r),(v:b:u:k:'a':r),(v:b:u:k:'m':r)]
       has_pret_part = case p of
                         (v:b:m:u:k:a:d:rest) -> elem [a,d] ["ad", "at","aq"]
       pos = case span (/='_') (reverse p) of
               (_,(_:n:_)) -> n
       is_reflexive p = case span (/='_') (reverse p) of
                          (_,(_:s)) -> case span (/='_') s of 
                                         (s',_) -> elem 's' s'
       is_poss p = case span (/='_') (reverse p) of
                     (_,(_:s)) -> case span (/='_') s of 
                                    (s',_) -> elem 'z' s'

verb_paradigms :: [(String, [String], [String] -> Entry)]
verb_paradigms = [
  paradigm_h "vb_1s_gillas" ["gillas"]  $
   verb_deponens 2 ([e "as"],[e "as",e "s"],[e "ades",e "des"], [e "ats",e "ts"], []),

  paradigm_h "vb_1a_spara" ["spara"]  $ 
   verb_weak 1 ([e "a"], [e "ar",e ""],[e "a",e ""],[e "ade"],[e "at"],[e "ad"]),

  paradigm_h "vb_1a_vissla" ["vissla"]  $ 
   verb_weak 1 ([e "a"], [e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"]),

  paradigm_h "vb_1a_skapa" ["skapa"]   $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "t"]),

  paradigm_h "vb_1a_hitta" ["hitta"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "e"]),

  paradigm_h "vb_1a_vänta" ["vänta"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "", e "e"]),

  paradigm_h "vb_1a_klaga" ["klaga"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[e "", e "o"]),

  paradigm_h "vb_1a_beundra" ["beundra"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad"],[]),

  paradigm_h "vb_1m_hisna" ["hisna"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[]),

  paradigm_h "vb_1m_svira" ["svira"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[e ""]),
  paradigm_h "vb_1m_existera" ["existera"]   $
   verb_weak_compound 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[],[]),

  paradigm_h "vb_1a_ugnsbaka" ["ugnsbaka"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "t"]),

  paradigm_h "vb_1m_vånna" ["vånna"]  $
   verb_full 1 ([e "a"], [],[e "ar"],[e "a"], [e "ade"], [e "e"], [e "at"], []),    

  paradigm_h "vb_1m_kackla" ["kackla"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[]),

  paradigm_h "vb_1a_unna" ["unna"]  $
   verb_weak 1 ([e "a"], [e "ar"], [e "a"], [e "ade"], [e "at"], [(id,"ad"),(tk 1,"t")]),

  paradigm_h "vb_1a_häda" ["häda"]  $
   verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at"],[e "ad",e "d"]),

  paradigm_h "vb_4m_ljuda" ["ljuda"] $
   verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[e "it"], []),

  paradigm_h "vb_4a_fara" ["fara"]  $
   verb_full_sform_variant 1 ([e "a"], [e "e"], [e ""], [e ""], [(vc "o","")], [(vc "o","e")], [e "it"], [e "en"]),

  paradigm_h "vb_2a_leva" ["leva"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [e "t", e "at"], [e "d"]),

  paradigm_h "vb_2a_stödja" ["stödja"]  $
   verb_weak_compound_sform_variant 1 ([(tk 1,"a"),e "a"], [(tk 1, "er"),(id,"er")], [(tk 1,"")], [(tk 1, "de")], [(tk 1 . dsuff "j", "tt")], [(tk 1, "d")],[(tk 1,""),e "e"]),

  paradigm_h "vb_2a_sälja" ["sälja"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "å".tk 1, "de")], [(vc "å".tk 1, "t")], [(vc "å" . tk 1,"d")]),

  paradigm_h "vb_2a_säga" ["säga"]  $
   verb_weak_sform_variant 1 ([(id,"a"),(tk 1, "ja")], [(id, "er"),(tk 1, "jer")], [e "",(tk 1,"j")], [(vc "a".tk 1,"de"),(vc "a".tk 1,"")], [(vc "a","t")] , [(vc "a", "d")]),

  paradigm_h "vb_2a_motsäga" ["motsäga"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a". tk 1, "de")], [(vc "a", "t")], [(vc "a","d")]),

  paradigm_h "vb_2a_mista" ["mista"]  $
   verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "ade",e "e"],[e "",e "at"],[e "ad",(\s -> s +? "t","")]),

  paradigm_h "vb_2a_välja" ["välja"]  $
   let g = vct [("ä","a"),("ö","o")] . tk 1 in 
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(g,"de")], [(g, "t")], [(g,"d")]),

  paradigm_h "vb_2m_hända" ["hända"]  $
   verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "e"],[(tk 1, "t")],[]),

  paradigm_h "vb_2d_må" ["må"] 
   (verb_weak 0 ([],[e ""],[],[e "tte"],[],[])),

  paradigm_h "vb_2m_gitta" ["gitta"]   
   (verb_weak 1 ([e "a"],[e "er"],[e ""], [e "e"], [e "at"], [])),    

  paradigm_h "vb_2a_städja" ["städja"] 
   (verb_weak_sform_variant 2 ([e "ja"], [e "jer",e "er"], [e ""], [(vc "a","de")],[(vc "a".tk 1,"tt")], [(vc "a","d")])),

  paradigm_h "vb_2a_genmäla" ["genmäla"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "de",e "te"],[e "t"],[e "d"])),

  paradigm_h "vb_2m_väga" ["väga"]  
   (verb_weak_no_sform 1 ([e "a"],[e "er"],[e ""],[e "de"],[e "t"],[])),

  paradigm_h "vb_4a_bottenfrysa" ["bottenfrysa"]  
   (verb_full_no_sform 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "te", (vc "ö","")],[(vc "u","e")],[(vc "u","it"),e "t"],[(vc "u","en"),e "t"])),

  paradigm_h "vb_va_frysa" ["frysa"]  
   (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "te", (vc "ö","")],[(vc "u","e")],[(vc "u","it"),e "t"],[(vc "u","en"),e "t"])),

  paradigm_h "vb_2m_höta" ["höta"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "ter",e "er"],[e ""],[e "te"],[e "t"],[])),

  paradigm_h "vb_2m_glädja" ["glädja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [(tk 1, "")], [(vc "a" . tk 1,"de")], [(vc "a" . tk 2, "tt")], [])),

  paradigm_h "vb_2m_böra" ["böra"] 
   (verb_weak_sform_variant 1 ([e "a"], [e ""], [e ""], [(vc "o", "de")], [(vc "o", "t")], [])),

  paradigm_h "vb_2a_tämja" ["tämja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [(id, "t"),(tk 1,"t")], [e "d"])),

  paradigm_h "vb_2a_spörja"           ["spörja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "o" . tk 1, "de")], [(vc "o" . tk 1, "t")], [(vc "o" . tk 1,"d")])),

  paradigm_h "vb_2s_blygas" ["blygas"]  $  
    (verb_dwc 2 ["as"] ["s","es"] ["des"] ["ts"] []),

  paradigm_h "vb_3s_brås" ["brås"]  $  
    (verb_dwc 1 ["s"] ["s"] ["ddes"] ["tts"] []),

  paradigm_h "vb_2s_trivas" ["trivas"]  $ 
   verb_deponens 2 ([e "as"],[(ungeminate,"s"),e "as"],[e"des"],[e "ts"],[]),

  paradigm_h "vb_2s_nöjas" ["nöjas"] $ 
   (verb_dwc 2 ["as"] ["s"] ["des"] ["ts"] []),

  paradigm_h "vb_2s_minnas" ["minnas"] $ 
   (verb_dwc 3 ["nas"] ["ns"] ["des"] ["ts"] []),

  paradigm_h "vb_2s_vämjas" ["vämjas"] $ 
   (verb_dwc 3 ["jas"] ["jes","js"] ["jdes","des"] ["jts","ts"] []),

  paradigm_h "vb_2s_töras" ["töras"] $ 
   (verb_dwc 4 ["öras","ordas"] ["örs"] ["ordes"] ["orts"] []),

  paradigm_h "vb_2s_rymmas" ["rymmas"] $ 
   (verb_dwc 3 ["mas"] ["s"] ["des"] ["ts"] []),

  paradigm_h "vb_2s_idas" ["idas"] $ 
   (verb_dwc 3 ["das"] ["ds","des"] ["ddes"] ["dats","tts"] []),

  paradigm_h "vb_2s_hövas" ["hövas"] $ 
   (verb_dwc 2 ["as"] ["es"] ["des"] ["ts"] []),

  paradigm_h "vb_2s_glädjas" ["glädjas"] $ 
   (verb_dwc 5 ["ädjas"] ["äds","ädes"] ["addes"] ["atts"] []),

  paradigm_h "vb_2s_giftas" ["giftas"] $ 
   (verb_dwc 2 ["as"] [] ["es"] ["s"] []),

  paradigm_h "vb_2s_skiljas" ["skiljas"] $ 
   (verb_dwc 3 ["jas"] ["js","s","jes"] ["des"] ["ts"] []),

  paradigm_h "vb_va_vika" ["vika"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "e","")], [(vc "e","e")],[e "it",e "t"], [e "en"])),

  paradigm_h "vb_va_tvinga" ["tvinga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [(id,"ade"),(vc "a","")], [(vc "u","e")],[e "at",(vc "u","it")], [e "ad",(vc "u","en")])),

  paradigm_h "vb_va_löpa" ["löpa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "o","p")], [(vc "u","e")],[(id,"t"),(vc "u","it")], [(vc "u","en"),e "t"])),

  paradigm_h "vb_vm_fnysa" ["fnysa"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "ö","")], [(vc "ö","e")],[e "t"], [])),

  -- paradigm_h "vb_vm_tilltvinga" ["tilltvinga"] 
  -- (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [(id,"ade"),(vc "a","")], [(vc "a","e")],[e "at",(vc "u","it")], [])),

  paradigm_h "vb_vm_avvara" ["avvara"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "it",e "at"],[])),
 
  paradigm_h "vb_va_växa" ["växa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e "a"], [e "te"],[(vc "u", "e")],[(vc "u", "it"),(id,"t")], [(vc "u", "en")])),

  paradigm_h "vb_va_stupa" ["stupa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "ö",""),(id,"ade")], [(vc "ö","e")],[e "at"], [e "ad"])),

  paradigm_h "vb_va_lyda" ["lyda"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"de"),(vc "ö","")], [(vc "ö","e")],[(tk 1,"tt")], [e "d"])),

  paradigm_h "vb_om_skola" ["skola"] 
   (verb_weak_no_sform 1 ([e "a"], [(vc "a" . tk 1, ""),(vc "a","l")], [e "a"], [(vc "u", "le")], [e "at"], [])),

  paradigm_h "vb_vm_snika" ["snika"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e "a"], [(vc "e", ""),(id,"te")], [(vc "e","e")], [e "it"], [])),

  paradigm_h "vb_vm_smälla" ["smälla"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "a",""),(id,"de")], [(vc "u","e")],[e ""], [])),

  paradigm_h "vb_va_tälja" ["tälja"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""],[(id,"de"),(vc "a" . tk 1,"de")], [(id,"t"),(vc "a" . tk 1,"t")], [e "d"])),

  paradigm_h "vb_va_två" ["två"] 
   (verb_weak 0 ([e ""], [e "r"], [e ""], [e "dde"], [(id,"tt"),(vc "a","git")],  [e "dd"])),

  paradigm_h "vb_va_smälta" ["smälta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "a", ""),(id,"e")], [(vc "u","e")],[(vc "u","it"),(id,"")], [(vc "u","en")])),

  paradigm_h "vb_va_förmäla" ["förmäla"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "te",e "de"],[e "t"],[e "d"])),

  paradigm_h "vb_va_besvärja" ["besvärja"]  
   (verb_full_sform_variant 1 ([e "a",(tk 1,"a")],[e "e"],[(id,"er"),(tk 1,"")],[e ""], [(vc "o" . tk 1, ""),(id,"de")], [(vc "u" .tk 1,"e")],[(vc "u" . tk 1, "it"),(id,"t")], [e "d", (vc "u" . tk 1, "en")])),

  paradigm_h "vb_om_kunna" ["kunna"]  
   (verb_weak_no_sform 0 ([e ""],[(vc "a" . tk 2, "")],[],[(tk 2, "de")],[e "t"],[])),

  paradigm_h "vb_vm_upphäva" ["upphäva"]       
   (verb_full_sform_variant 1 ([e "a"],[e "e"],[e "er"],[e ""],[e "de",(vc "o","")],[(vc "o","e")],[e "t"],[])),

  paradigm_h "vb_vm_undvara" ["undvara"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "at",e "it"],[])),

  paradigm_h "vb_vm_strida" ["strida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(id,"de"), (vc "e", "")], [(vc "e","e")],[(tk 1, "tt"), (id, "it")], [])),

  paradigm_h "vb_vm_sluta" ["sluta"] 
   (verb_full_sform_variant 1 ([e "a"],[e "e"],[e "ar"], [e ""], [(id,"ade"),(vc "ö","")], [(vc "ö","e")],[e "at"], [])),

  paradigm_h "vb_vm_samvara" ["samvara"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "ar"],[e "a"],[e "ade"],[e "it"],[])),

  paradigm_h "vb_vm_ryka" ["ryka"]  
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö",""),(id,"te")], [(vc "ö","e")],[(id,"t"),(vc "u","it")],[])),

  paradigm_h "vb_vm_nysa" ["nysa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(id,"te"),(vc "ö","")], [(vc "ö","e")],[(id,"t"),(id,"it"),(vc "u","it")], [])),

  paradigm_h "vb_vm_kvida" ["kvida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "e", ""),(id,"ade")], [(vc "e","e")],[e "it"], [])),

  paradigm_h "vb_vm_klinga" ["klinga"]  
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [(id,"ade"),(vc "a","")], [(vc "a","e")],[e "at"], [])),

  paradigm_h "vb_va_gälla_kastrera"   ["gälla"] 
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "de",e "ade"],[e "t",e "at"],[e "d",e "ad"])),

  paradigm_h "vb_vm_gala" ["gala"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "o","")], [(vc "o","e")],[(id,"it"),(vc "a","t")], [])),

  paradigm_h "vb_vm_duga" ["duga"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö",""),(id,"ade")], [(vc "ö", "e")],[(id,"t"),(id,"it")], [])),

  paradigm_h "vb_vm_drösa" ["drösa"]  
   (verb_weak_sform_variant 1 ([e "a"],[e "er"],[e ""],[e "te",e "ade"],[e "at",e "t"],[])),

  paradigm_h "vb_vm_drypa" ["drypa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[(vc "u","it"),(id,"t")], [])),

  paradigm_h "vb_va_utlöpa" ["utlöpa"] 
   (verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "te"], [e "t"], [(vc "u", "en")])),

  paradigm_h "vb_va_träda" ["träda"]  
   (verb_weak_sform_variant 1 ([e "a"], [e "er"],[e ""],[e "ade",e "de"], [(id,"at"),(tk 1,"tt")], [e "d"])),

  paradigm_h "vb_va_strypa" ["strypa"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö",""),(id,"te")], [(vc "ö","e")],[e "t"],[e "t"])),

  paradigm_h "vb_va_snusmala" ["snusmala"]  
   (verb_weak_sform_variant 1 ([e "a"],[e ""],[e ""],[e "de"],[e "t"],[e "d",e "en"])),

  paradigm_h "vb_va_skvätta" ["skvätta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a",""),(id,"e")], [(vc "a","e")],[e ""], [e "ad"])),

  paradigm_h "vb_va_simma" ["simma"] 
   (verb_full 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "a" . ungeminate ,""),(id,"ade")], [(vc "a","e")],[(vc "u","it"), (id,"at")],[(vc "u","en"), (id,"ad")])),

  paradigm_h "vb_va_nästa" ["nästa"] 
   (verb_weak 1 ([e "a"],[e "ar"],[e "a"],[e "ade",e "e"],[e "at"],[e "ad"])),

  paradigm_h "vb_va_mala" ["mala"] 
   (verb_weak 1 ([e "a"],[e "er",e ""],[e ""],[e "de"],[e "t"],[e "d",e "en"])),

  paradigm_h "vb_va_kväda" ["kväda"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a",""),(id,"de")], [(vc "a","e")],[e "it"], [e "en"])),

  paradigm_h "vb_va_klyva" ["klyva"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "ö","")], [(vc "ö","e")],[(vc "u","it"),e "it",e "t"], [e "en"])),

  paradigm_h "vb_va_gälda" ["gälda"]  
   (verb_full 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [e "ade"] , [(vc "u","e")],[e "at"], [(vc "u", "en"),(id,"ad")])),

  paradigm_h "vb_va_förse" ["förse"] 
   (verb_weak 0 ([e ""], [e "r"], [e ""],[(vc "å","g"),(id,"dde")], [e "tt"], [e "dd"])),

  paradigm_h "vb_va_förlöpa" ["förlöpa"] 
   (verb_full_no_sform 1 ([e "a"], [e "e"],[e "er"], [e ""], [e "te"], [(vc "u","e")],[(vc "u","it"),(id,"t")],[(vc "u","en"), (id,"t")])),

  paradigm_h "vb_va_framtvinga" ["framtvinga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"], [e "ade"],[(vc "u","e")],[(vc "u","it"),(id,"at")], [(vc "u","en"),(id, "ad")])),

  paradigm_h "vb_va_tala" ["tala"]  $ 
   (verb_weak_sform_variant 1 ([e "a"], [e "ar"], [e "a"], [e "ade",e "te"],[e "at",e "t"], [e "ad",e "d"])),

  paradigm_h "vb_va_bestrida" ["bestrida"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "e",""),(id,"de")], [(vc "e","e")],[(tk 1,"tt"),(id,"it")], [e "en",e "d"])),

  paradigm_h "vb_va_besluta" ["besluta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "ar"], [e "a"],[(vc "ö",""),(id,"ade")], [(vc "ö","e")],[e "at",e "it"], [e "en",e "ad"])),

  paradigm_h "vb_va_begrava" ["begrava"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "o",""),(id,"de")], [(vc "o","e")],[e "t",e "it"], [e "en",e "d"])),

  paradigm_h "vb_om_vilja" ["vilja"] 
   (verb_weak_no_sform 2 ([e "ja"], [e "l"], [e "ja"], [e "le"],[(vc "e", "at")],[])),

  paradigm_h "vb_om_veta" ["veta"] 
   (verb_full_no_sform 1 ([e "a"],[e "e"],[e ""],[e ""],[(vc "i". tk 1,"sste")],[], [e "at"],[])),

  paradigm_h "vb_om_måste" ["måste"] 
    (verb_weak_no_sform 1 ([e "a"],[e "e"],[e "a"],[e "e"],[e ""],[])),

  paradigm_h "vb_om_heta" ["heta"] 
    (verb_weak_no_sform 1 ([e "a"],[e "er"],[e ""],[e "te"],[e "at"],[])),

  paradigm_h "vb_oa_varda" ["varda"]  
    (verb_weak_no_sform 1 ([e "a"], [e "er"], [e "a"], [e "e"], [(tk 1,"t")],[(vc "o", "en")])),

  paradigm_h "vb_vs_dväljas" ["dväljas"]  $ 
     (verb_deponens 3 ([e "jas"], [e "jes",e "js"],[(vc "a","des"),(id,"jdes")], [(vc "a","ts"),(id,"ts")],[])),

  paradigm_h "vb_4s_vederfaras" ["vederfaras"]  $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "o","s")], [e "its"],[])),

  paradigm_h "vb_4s_tas" ["tas"]  $ 
     (verb_deponens 1 ([e "s"], [e "s"],[(vc "o","gs")], [e "gits"],[])),

  paradigm_h "vb_4s_umgås" ["umgås"] $ 
     (verb_deponens 0 ([e ""], [e ""],[(vc "i".tk 1,"cks")],[(tk 1, "tts")],[])),

  paradigm_h "vb_4s_munhuggas" ["munhuggas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s",e "es"],[(vc "ö","s")], [e "its"],[])),

  paradigm_h "vb_4s_bitas" ["bitas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "e","s")], [e "its"],[])),

  paradigm_h "vb_4s_hållas" ["hållas"] $ 
     (verb_deponens 2 ([e "as"], [e "s"],[(vc "ö","s")], [e "its"],[])),

  paradigm_h "vb_4s_finnas" ["finnas"]  $ 
     (verb_deponens 2 ([e "as"], [e "s",e "es"],[(vc "a","s")],[(vc "u","its")],[])),

  paradigm_h "vb_4s_slåss" ["slåss"]  $ 
     (verb_deponens 2 ([e "ss"], [e "ss"],[(vc "o","gs")],[(vc "a","gits")],[])),

  paradigm_h "vb_4m_svälta_1" ["svälta"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", "")], [(vc "u","e")],[(vc "u", "it")], [])),

  paradigm_h "vb_va_svälta_2" ["svälta"]  
   (verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", ""), e "e"], [(vc "u","e")],[(vc "u", "it")], [e ""])),

  paradigm_h "vb_4m_förslå" ["förslå"]    
   (verb_full 0 ([e ""], [e "ge"],[e "r"], [e ""],[(vc "o", "g")],[(vc "o","e")],[(vc "a","git")], [])),

  paradigm_h "vb_4a_stjäla" ["stjäla"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""],[(tk 3, "al")], [(tk 3,"ule")],[(tk 3,"ulit")],[(tk 3,"ulen")])),

  paradigm_h "vb_4m_vara" ["vara"]  
    (verb_full 4 ([e "vara"], [e "vare"],[e "är"],[], [e "var"], [e "vore"], [e "varit"], [])), 

  paradigm_h "vb_4m_sova" ["sova"] 
    (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""],[e ""],[e "e"],[e "it"],[])),

  --paradigm_h "vb_4a_försova" ["försova"] 
  --  (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""],[e ""],[e "e"],[e "it"],[e "en"])),

  paradigm_h "vb_4m_erfara" ["erfara"] 
    (verb_full 1 ([e "a"], [e "e"],[e ""], [e ""],[(vc "o", "")], [(vc "o","e")],[e "it"], [])),

  paradigm_h "vb_4a_bli" ["bli"]  vb_4a_bliva,

  paradigm_h "vb_4a_bestå" ["bestå"]     
   (verb_full 0 ([e ""], [],[e "r"], [e ""], [(vc "o", "d")], [(vc "o","de")],[e "tt"], [e "nden", e "tt"])),

  paradigm_h "vb_4a_äta" ["äta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "å", "")], [(vc "å","e")],[e "it"], [e "en"])),

  paradigm_h "vb_4a_svära" ["svära"] 
   (verb_full_compound_sform_variant 1 ([e "a",e "ja"], [e "je", e "e"],[e "",e "jer"], [e ""], 
               [(vc "o", "")],[(vc "u","e")],[(vc "u","it")],[(vc "u", "en")],[e ""])),

  paradigm_h "vb_4a_emotstå" ["emotstå"] 
   (verb_full 0 ([e ""], [],[e "r"], [e ""],[(vc "o", "d")], [(vc "o","de")],[e "tt"], [e "nden"])),

  paradigm_h "vb_4m_sitta" ["sitta"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a", "")],  [(vc "u","e")],[(vc "u", "it")], [])),

  paradigm_h "vb_4a_be" ["be"]  
   (verb_full_compound_sform_variant 0 ([e "",e "dja",e "da"], [e "dje",e "de"],[e "r",e "djer",e "der"], [e "",e "dj",e "d"],[(vc "a", "d")], [(vc "a","de")],[e "tt"], [e "dd"],[])),

  paradigm_h "vb_4m_ryta" ["ryta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö", "")],[(vc "u","e")],[(vc "u", "it")], [])),

  -- paradigm_h "vb_4a_ryta" ["förbryta"] 
  -- (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö", "")],[(vc "u","e")],[(vc "u", "it")], [e "t"])),

  paradigm_h "vb_4m_gråta" ["gråta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ä", "")],[(vc "ä","e")],[e "it"], [])),

  paradigm_h "vb_4m_ligga" ["ligga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "å" . tk 1, "")],[(vc "å","e")],[(vc "e". tk 1, "at")], [])),

  paradigm_h "vb_4m_le" ["le"] 
   (verb_full 0 ([e ""], [],[e "r"], [e ""],[(vc "o", "g")], [(vc "o","ge")],[e "tt"], [])),

  paradigm_h "vb_4m_bekomma" ["bekomma"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [],[(vc "o" . ungeminate, "")], [(vc "o", "e")], [e "it"], [])),

  paradigm_h "vb_4m_småsvära" ["småsvära"] 
   (verb_full_sform_variant 1 ([e "a", e "ja"], [e "e"],[e "",e "jer"], [e ""], [(vc "o", "")],[(vc "u","e")],[(vc "u", "it")], [])),

  paradigm_h "vb_4m_skåpäta" ["skåpäta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""],[(vc "å", "")],[(vc "å","e")],[e "it"], [])),
 
  paradigm_h "vb_4m_förevara" ["förevara"] 
   (verb_weak_sform_variant 1 ([e "a"], [], [], [e ""], [e "it"], [])),

  paradigm_h "vb_4a_stinga" ["stinga"]  
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [], [],[(vc "u","e")],[(vc "u","it")],[(vc "u","en")])),

  paradigm_h "vb_4a_förgäta" ["förgäta"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""],[(vc "a","")], [(vc "a","e")],[e "it"], [e "en"])),

  --paradigm_h "vb_3m_te" ["te"] 
  -- (verb_weak_no_sform 0 ([e ""], [e "r"], [e ""], [e "dde"], [e "tt"], [])),

  paradigm_h "vb_2m_ha" ["ha"]  $
   verb_weak_compound 0 ([e "",e "va"], [e "r",e "ver"], [e ""], [e "de"], [e "ft"], [],[e ""]),

  paradigm_h "vb_2m_mysa" ["mysa"] $ 
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(ungeminate_m_n, "te")], [(ungeminate_m_n, "t")], []),
  paradigm_h "vb_va_nypa" ["nypa"] $ 
   verb_full 1 ([e "a"], [e "e"],[e "er"], [e ""], [(vc "ö","")], [(vc "ö","e")],[(id,"t"),(vc "u","it")], [(id,"t"),(vc "u","en")]),
  paradigm_h "vb_0d_lyster"    ["lyster"]    $ 
   verb_weak 0 ([], [e ""], [], [], [], []),
  paradigm_h "vb_0d_värdes"    ["värdes"]    $
   verb_weak 0 ([], [e ""], [], [], [], []),
  paradigm_h "vb_0d_vederböra" ["vederböra"] $
   verb_weak 0 ([e ""], [(tk 1,"")], [], [], [], []),
  paradigm_h "vb_0d_nåde"      ["nåde"]      $ 
   verb_full 0 ([], [e ""],[],[], [], [], [], []), 

  paradigm_h "vb_0d_lyss"      ["lyss"]      $ 
   verb_full 0 ([e ""], [],[],[e ""], [], [], [], []), 

  paradigm_h "vb_4d_vederfås"  ["vederfås"]  $ 
    verb_dwc 1 [""] [] [] ["tts"] [],
  -- paradigm_h "vb_4d_sprätta"   ["spritta"]   $ 
  -- verb_weak 1 ([e "a"], [e "er"], [e ""], [(vc "a","")], [], []),

  paradigm_h "vb_id_månde"     ["månde"]  $ 
   verb_weak_no_sform 0 ([], [],[],[e ""], [], []), 

  paradigm_h "vb_2d_torde"     ["torde"] $
   verb_full 2 ([], [],[(vc "ö","")],[], [], [e "de"], [], []), 
  paradigm_h "vb_2d_rädas"     ["rädas"]     $ 
   verb_dwc 2 ["as"] ["as","s"] ["des"] [] [],
  paradigm_h "vb_1a_laga"                     ["laga"]                v1,
  paradigm_h "vb_1s_andas"                    ["andas"]               vb_1s_hoppas,
  paradigm_h "vb_2a_ansöka"                   ["ansöka"] $
    verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(ungeminate_m_n, "te")], [(ungeminate_m_n, "t")], [(ungeminate_m_n, "t")]),
  paradigm_h "vb_2a_göra"              ["göra"]                vb_2a_göra,
  paradigm_h "vb_2a_hyra"              ["hyra"] $
   verb_weak_sform_variant 1 ([e "a"], [e ""], [e ""], [e "de"], [e "t"], [e "d"]),
  paradigm_h "vb_2a_känna"             ["känna"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(tk 1, "de")], [(tk 1,"t")], [(tk 1, "d")]),
  paradigm_h "vb_2a_leda"              ["leda"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [(tk 1, "tt")], [e "d"]),
  paradigm_h "vb_2a_lägga"             ["lägga"]  $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a".tk 2,  "de"),(vc "a".tk 2,  "")], [(vc "a" . tk 1, "t")], [(vc "a". tk 1,"d")]),
  paradigm_h "vb_2a_sätta"             ["sätta"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [(vc "a", "e")], [(vc "a" , "")], [(vc "a","")]),
  paradigm_h "vb_2a_viga"              ["viga"] $
   verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "de"], [e "t"], [e "d"]),
  paradigm_h "vb_2s_synas"             ["synas"]               vb_2s_synas,
  paradigm_h "vb_4a_falla"             ["falla"] $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [e "it"], [e "en"])), 
  paradigm_h "vb_4a_flyga"             ["flyga"] 
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [(vc "u","it")], [(vc "u","en")])), 

  paradigm_h "vb_4a_ge"                ["ge"] $
   verb_full_sform_variant 0 ([e "", (vc "i","va")], [(vc "i","ve")], [e "r",(vc "i","ver")], [e "",(vc "i","v")], [(vc "a","v")], [(vc "å","ve")], [e "tt",(vc "i","vit")], [(vc "i", "ven")]),
  paradigm_h "vb_4a_hålla"             ["hålla"] $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"],[e ""], [(vc "ö", "")], [(vc "ö", "e")], [e "it"], [e "en"])), 
  paradigm_h "vb_4a_komma"             ["komma"] $             vb_4a_komma,
  paradigm_h "vb_4a_rida"              ["rida"]  $              vb_4a_bita,
  paradigm_h "vb_4a_skjuta"            ["skjuta"] $
  (verb_full_sform_variant 1 ([e "a"], [e "e"],[e "er"], [e ""], [(tk 3,"öt")], [(tk 3,"öte")],[e "it"], [e "en"])),
  paradigm_h "vb_4a_tillåta"           ["tillåta"]             vb_4a_låta,
  paradigm_h "vb_4a_slå"               ["slå"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "o","g")], [(vc "o","ge")], [(vc "a", "git")], [(vc "a","gen")])),
  paradigm_h "vb_4a_se"                ["se"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "å","g")], [(vc "å","ge")], [e "tt"], [e "dd"])),
  paradigm_h "vb_4a_gå"                ["gå"]  $
   (verb_full_sform_variant 0 ([e ""], [e "nge"],[e "r"],[e ""], [(vc "i", "ck")], [(vc "i", "nge")], [e "tt"], [e "ngen"])), 
  paradigm_h "vb_4a_dricka"            ["dricka"]              vb_4a_vinna,

  paradigm_h "vb_4a_bära"              ["bära"]   $
  (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "a","")], [(vc "u","e")],[(vc "u", "it")], [(vc "u","en")])),

  paradigm_h "vb_4m_innebära"     ["innebära"]   $
   (verb_full_sform_variant 1 ([e "a"], [e "e"],[e ""], [e ""], [(vc "a","")], [(vc "u","e")],[(vc "u", "it")], [])),

  paradigm_h "vb_4a_ta"                ["ta"] $ vb_4a_ta,
            
  paradigm_h "vb_va_klä"                ["klä"]             
  (verb_weak 0 ([e "da",e ""], [e "r",e "der"], [e "",e "d"], [e "dde"], [e "tt"], [e "dd"])),
  paradigm_h "vb_4m_angå"              ["angå"] 
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "i","ck")], [(vc "i","nge")], [e "tt"], [])),
  paradigm_h "vb_4m_stå"               ["stå"] $
  (verb_full 0 ([e ""], [],[e "r"],[e ""], [(vc "o","d")], [(vc "o","de")], [e "tt"], [])),
  paradigm_h "vb_4m_vina"              ["vina"]                vb_4m_vina,
  paradigm_h "vb_va_bringa"            ["bringa"]  vb_va_bringa,
  paradigm_h "vb_2a_lyfta"           ["lyfta"] $
    verb_weak_sform_variant 1 ([e "a"], [e "er"], [e ""], [e "e"], [e ""], [e ""]),
  paradigm_h "vb_2a_sända"         ["sända"]   vb_2a_sända,
  paradigm_h "vb_3a_sy"               ["sy"]      v3,
  paradigm_h "vb_va_koka"           ["koka"]    vb_va_koka,
  paradigm_h "vb_va_sprida"         ["sprida"]  $
   (verb_full 1 ([e "a"],[e "e"],[e "er"],[e ""], [(vc "e",""),e "de"], [(vc "e","e")],[(tk 1,"tt"),e "it"], [e "d"]))
 ]
