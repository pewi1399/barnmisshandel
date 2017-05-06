


deriveMFR = function(grund){

   #-------------------------- nyckelbensbrott tabell 2 ---------------------------
   grund$PclavikelICD10 = ifelse(grepl(" P134 ",grund$DIAGNOSER),1,0)
   grund$PclavikelICD9 = ifelse(grepl(" 767C ",grund$DIAGNOSER),1,0)
   grund$PclavikelICD8 = ifelse(grepl(" 77221 ",grund$DIAGNOSER),1,0)
   
   grund$Pclavikel_d1 = ifelse(rowSums(grund[,names(grund)[grep("Pclavikel",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------------- lårben tabell2  ----------------------------------
   grund$PlårbenICD10 = ifelse(grepl(" P132 ",grund$DIAGNOSER),1,0)
   grund$PlårbenICD9 = ifelse(grepl(" 767D ",grund$DIAGNOSER),1,0)
   grund$PlårbenICD8 = ifelse(grepl(" 77223 ",grund$DIAGNOSER),1,0)
   
   grund$Plårben_d1 = ifelse(rowSums(grund[,names(grund)[grep("Plårben",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------------- överarm tabell2  ---------------------------------
   grund$PöverarmICD8 = ifelse(grepl(" 77222 ",grund$DIAGNOSER),1,0)
   
   grund$Pöverarm_d1 = grund$PöverarmICD8
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- rörben tabell2  ---------------------------------
   
   grund$Prörben_d1 = ifelse(rowSums(grund[,names(grund)[grep("Plårben|Pöverarm",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------------- skallfraktur tabell2  ----------------------------
   grund$PskallfrakturICD10 = ifelse(grepl(" P130 ",grund$DIAGNOSER),1,0)
   grund$PskallfrakturICD8 = ifelse(grepl(" 77220 ",grund$DIAGNOSER),1,0)
   
   grund$PskallfrakturAndraForlossICD10 = ifelse(grepl(" P131 ",grund$DIAGNOSER),1,0)
   
   grund$Pskallfraktur_d1 = ifelse(rowSums(grund[,names(grund)[grep("Pskallfraktur",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------- andra skelettskador tabell2  --------------------------
   grund$PandraSkelettSkadorICD9 = ifelse(grepl(" 767D ",grund$DIAGNOSER),1,0)
   
   grund$PandraSkelettSkador_d1 = grund$PandraSkelettSkadorICD9 
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------- andra specificerade skelettskador tabell2  ------------------
   #----------------- andra ospecificerade skelettskador tabell2  ------------------
   grund$PandraSpecificeradeSkadorSkelettICD9 = ifelse(grepl(" P138 ",grund$DIAGNOSER),1,0)
   grund$PandraSpecificeradeSkadorSkelett_d1 = grund$PandraSpecificeradeSkadorSkelettICD9
   
   grund$PandraOSpecificeradeSkadorSkelettICD9 = ifelse(grepl(" P139 ",grund$DIAGNOSER),1,0)
   grund$PandraOSpecificeradeSkadorSkelett_d1 = grund$PandraOSpecificeradeSkadorSkelettICD9
   
   grund$PövrigFraktur_d1 = ifelse(rowSums(grund[,names(grund)[grep("PandraOSpecificeradeSkadorSkelett|PandraSpecificeradeSkadorSkelett",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------------- subdural tabell2 ---------------------------------
   grund$PsubduralICD10 = ifelse(grepl(" P100 ",grund$DIAGNOSER),1,0)
   grund$PsubduralICD9 = ifelse(grepl(" 767A ",grund$DIAGNOSER),1,0)
   grund$PsubduralICD8 = ifelse(grepl(" 77202 ",grund$DIAGNOSER),1,0)
   
   
   grund$Psubdural_d1 = ifelse(rowSums(grund[,names(grund)[grep("Psubdural",names(grund))]],na.rm=T)>0,1,0)
   #-------------------------------------------------------------------------------
   
   #-------------------------- intrakraniellskada tabell2 -------------------------
   grund$PintrakraniellskadaICD10 = ifelse(grepl(" P10 ",grund$DIAGNOSER),1,0)
   grund$Pintrakraniellskada_d1 = grund$PintrakraniellskadaICD10
   
   grund$PintrakraniellIckeTraumaICD10 = ifelse(grepl(" P52 ",grund$DIAGNOSER),1,0)
   grund$PintrakraniellIckeTrauma_d1 = grund$PintrakraniellIckeTraumaICD10
   
   grund$PintrakraniellSkadaBlödning_d1 = ifelse(rowSums(grund[,names(grund)[grep("PintrakraniellIckeTrauma|Pintrakraniellskada",names(grund))]],na.rm=T)>0,1,0)
   #-------------------------------------------------------------------------------
   
   
   
   #pick diagnosis vars
   cols = c(names(grund)[grep("MDIA|FLOP",names(grund))])
   
   #create diagnosis var based on extracted cols
   grund$DIAGNOSER <- apply(grund[ , cols] , 1 , paste , collapse = " ")
   grund$DIAGNOSER = paste(" ",grund$DIAGNOSER," ", sep="") 
   
   #------------------------------- grunde vars -----------------------------------
   
   #-------------------------------- Preeklampsi ----------------------------------
   grund$preeklampsi = ifelse(grepl(" O139 ",grund$DIAGNOSER), "Graviditetshypertoni",
                             ifelse(grepl(" O140 | O149 ",grund$DIAGNOSER), "Lätt preeklampsi",
                                    ifelse(grepl(" O141 | O142 ",grund$DIAGNOSER), "Svår preeklampsi",
                                           ifelse(grepl(" O15 ",grund$DIAGNOSER), "Eklampsi",0))))
   
   #table(grund$preeklampsi)
   #-------------------------------------------------------------------------------
   
   #----------------------------- Placentaavlossning ------------------------------
   grund$placentaavlossning = ifelse(grepl(" O45 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$placentaavlossning)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- kroniskHT -----------------------------------
   grund$kroniskHT = ifelse(grepl(" O10 | O11 | I1[0-5] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$kroniskHT)
   #-------------------------------------------------------------------------------
   
   #------------------------------ gravididtetsDiab -------------------------------
   grund$gravididtetsDiab = ifelse(grepl(" O244A ",grund$DIAGNOSER), "Ej insulinbehandlad",
                                  ifelse(grepl(" O244B ",grund$DIAGNOSER), "Insulinbehandlad",0))
   
   #table(grund$gravididtetsDiab)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- pregestDiab ---------------------------------
   grund$pregestDiab = ifelse(grepl(" O24[0-3] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$pregestDiab)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- oligohydramn --------------------------------
   grund$oligohydramn = ifelse(grepl(" O410 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$oligohydramn)
   #-------------------------------------------------------------------------------
   
   #------------------------------ värkrubbningar ---------------------------------
   grund$värkrubbningar = ifelse(grepl(" O62 | O62[0123489] | O63 | O63[0129] | O64 | O64[01234589] | O65 | O65[01234589] | O66 | O66[01234589] | O67 ",grund$DIAGNOSER), 1,0)
   
   #grepl(" O62[23489] ",c(" O621 "," O628 "," O625 "))
   
   #table(grund$värkrubbningar)
   #-------------------------------------------------------------------------------
   
   #---------------------------- värkrubbningarSvaghet ----------------------------
   grund$värkrubbningarSvaghet = ifelse(grepl(" O62 | O62[0123489] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$värkrubbningarSvaghet)
   #-------------------------------------------------------------------------------
   
   #----------------------- värkrubbningarUtdragenFörloss -------------------------
   grund$värkrubbningarUtdragenFörloss = ifelse(grepl(" O63 | O63[0129] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$värkrubbningarUtdragenFörloss)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- hotandeAsfyxi -----------------------------------
   grund$hotandeAsfyxi = ifelse(grepl(" O68 | O68[012389] | O69 | O69[01234589] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$hotandeAsfyxi)
   #-------------------------------------------------------------------------------
   
   #------------------------------ navelsträngsKompl ------------------------------
   grund$navelsträngsKompl = ifelse(grepl(" O69 | O69[01234589] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$navelsträngsKompl)
   #-------------------------------------------------------------------------------
   
   #----------------------------- feberUnderfFörloss ------------------------------
   grund$feberUnderfFörloss = ifelse(grepl(" O75[2-3] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$feberUnderfFörloss)
   #-------------------------------------------------------------------------------
   
   #------------------------------ sätesextraktion --------------------------------
   grund$sätesextraktion = ifelse(grepl(" O83[0-1] | O84 | O84[01289] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$sätesextraktion)
   #-------------------------------------------------------------------------------
   
   
   
   #pick diagnosis vars
   grund$DIAGNOSER = grund$BDIAG
   
   #------------------------- påverkanFosterPgaKejsarsnitt ------------------------
   grund$påverkanFosterPgaKejsarsnitt = ifelse(grepl(" P034 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$påverkanFosterPgaKejsarsnitt)
   #-------------------------------------------------------------------------------
   
   #------------------------- förlossningsskadaCnsBlödning ------------------------
   grund$förlossningsskadaCnsBlödning = ifelse(grepl(" P10 | P10[0123489] | P11 | P11[0123489] | P12 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$förlossningsskadaCnsBlödning)
   #-------------------------------------------------------------------------------
   
   #------------------------------- kefalhematom ----------------------------------
   grund$kefalhematom = ifelse(grepl(" P12 | P12[0123489] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$kefalhematom)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- frakturer -----------------------------------
   grund$frakturer = ifelse(grepl(" P13 | P13[0123489] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$frakturer)
   #-------------------------------------------------------------------------------
   
   #--------------------------------- nervskada -----------------------------------
   grund$nervskada = ifelse(grepl(" P14 | P14[012389] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$nervskada)
   #-------------------------------------------------------------------------------
   
   #----------------------------- andraFörlossSkador ------------------------------
   grund$andraFörlossSkador = ifelse(grepl(" P15 | P15[012345689] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$andraFörlossSkador)
   #-------------------------------------------------------------------------------
   
   #------------------------------ intrauterinHypoxi -------------------------------
   grund$intrauterinHypoxi = ifelse(grepl(" P20 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$intrauterinHypoxi)
   #-------------------------------------------------------------------------------
   
   #----------------------------------- HIE ---------------------------------------
   grund$HIE = ifelse(grepl(" P910A | P910B | P910C ",grund$DIAGNOSER), 1,0)
   
   #grund$DIAGNOSER[grepl(" P910A | P910B | P910C ",grund$DIAGNOSER)]
   #table(grund$HIE)
   #-------------------------------------------------------------------------------
   
   #------------------------------- respDistress ----------------------------------
   grund$respDistress = ifelse(grepl(" P22 | P22[0189] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$respDistress)
   #-------------------------------------------------------------------------------
   
   #-------------------------------- pneumoni -------------------------------------
   grund$pneumoni = ifelse(grepl(" P2[3-9] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$pneumoni)
   #-------------------------------------------------------------------------------
   
   #----------------------------- sepsisNyfödd ------------------------------------
   grund$sepsisNyfödd = ifelse(grepl(" P36 | P36[01234589] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$sepsisNyfödd)
   #-------------------------------------------------------------------------------
   
   #---------------------------- blödningFoster -----------------------------------
   grund$blödningFoster = ifelse(grepl(" P50 | P55 | P56 | P50[01234589] ",grund$DIAGNOSER), 1,0)
   
   #table(grund$blödningFoster)
   #-------------------------------------------------------------------------------
   
   #----------------------------- kärnikterus ------------------------------------
   grund$kärnikterus = ifelse(grepl(" P57 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$kärnikterus)
   #-------------------------------------------------------------------------------
   
   #------------------------------- kramper ---------------------------------------
   grund$kramper = ifelse(grepl(" P90 ",grund$DIAGNOSER), 1,0)
   
   #table(grund$kramper)
   #-------------------------------------------------------------------------------
   
   #---------------------------- missbildningar -----------------------------------
   grund$missbildningar = ifelse(grepl(" Q[0-9][0-9] ",grund$DIAGNOSER), 1,0)
   
   #table(mfr0$missbildningar)
   #-------------------------------------------------------------------------------
   
   
   
   out = grund
return(out)
}
