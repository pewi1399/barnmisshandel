#--------------------------------- Rakit ---------------------------------------
grund$aktivRakitICD10 = ifelse(grepl(" E55",grund$DIAGNOSER),1,0)
grund$aktivRakitICD9 = ifelse(grepl(" 268A ",grund$DIAGNOSER),1,0)
grund$aktivRakitICD8 = ifelse(grepl(" 265,00 ",grund$DIAGNOSER),1,0)

grund$aktivRakit = ifelse(rowSums(grund[,names(grund)[grep("aktivRakit",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ Osteomalaci ------------------------------------
grund$OsteomalaciICD8 = ifelse(grepl(" 265,20 ",grund$DIAGNOSER),1,0)

grund$Osteomalaci = grund$OsteomalaciICD8
#-------------------------------------------------------------------------------

#----------------------------- kalciumRubbningar -------------------------------
grund$kalciumRubbningarICD10 = ifelse(grepl(" E835 | E835X | E388 ",grund$DIAGNOSER),1,0)
grund$kalciumRubbningarICD9 = ifelse(grepl(" 269D ",grund$DIAGNOSER),1,0)
grund$kalciumRubbningarICD8 = ifelse(grepl(" 269D ",grund$DIAGNOSER),1,0)

grund$kalciumRubbningar = ifelse(rowSums(grund[,names(grund)[grep("kalciumRubbningar",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- misstankeOmBarnMisshdel -------------------------
grund$misstankeOmBarnMisshdelICD10 = ifelse(grepl(" Z038K | Y071A | T741 ",grund$DIAGNOSER),1,0)
grund$misstankeOmBarnMisshdelICD9 = ifelse(grepl(" 995F | 995.81 ",grund$DIAGNOSER),1,0)


grund$misstankeOmBarnMisshdel = ifelse(rowSums(grund[,names(grund)[grep("misstankeOmBarnMisshdel",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- frakturRevben -----------------------------------
grund$frakturRevbenICD10 = ifelse(grepl(" S223 | S224 ",grund$DIAGNOSER),1,0)
grund$frakturRevbenICD9 = ifelse(grepl(" 807A",grund$DIAGNOSER),1,0)
grund$frakturRevbenICD8 = ifelse(grepl(" 807,10",grund$DIAGNOSER),1,0)

grund$frakturRevben = ifelse(rowSums(grund[,names(grund)[grep("frakturRevben",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- multiplaFrakturerRevben -------------------------
grund$multiplaFrakturerRevbenICD10 = ifelse(grepl(" S224 ",grund$DIAGNOSER),1,0)

grund$multiplaFrakturerRevben = grund$multiplaFrakturerRevbenICD10
#-------------------------------------------------------------------------------


#------------------------------- frakturLårben ---------------------------------
grund$frakturLårbenICD10 = ifelse(grepl(" S72.0",grund$DIAGNOSER),1,0)
grund$frakturLårbenICD9 = ifelse(grepl(" 821(A|C) ",grund$DIAGNOSER),1,0)
grund$frakturLårbenICD8 = ifelse(grepl(" 82",grund$DIAGNOSER),1,0)  ##ÄNDRAS EFTER MÖTE 20150515!!

grund$frakturLårben = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- multiplaLårbensFrakturer ------------------------
grund$multiplaFrakturerLårben = ifelse(grepl(" S727",grund$DIAGNOSER),1,0)

#-------------------------------------------------------------------------------

#----------------------------- frakturUnderben -------------------------------
grund$frakturUnderbenICD10 = ifelse(grepl(" S82[1-9]0? ", grund$DIAGNOSER),1,0)
grund$frakturUnderbenICD9 = ifelse(grepl(" 823(A|C) ",grund$DIAGNOSER),1,0)
grund$frakturUnderbenICD8 = ifelse(grepl(" 823",grund$DIAGNOSER),1,0)##ÄNDRAS EFTER MÖTE 20150515!!

grund$frakturUnderben = ifelse(rowSums(grund[,names(grund)[grep("frakturUnderben",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- underbensFrakturer ------------------------------
grund$underbensFrakturer = ifelse(grepl(" S8270 | S823 | S825 ",grund$DIAGNOSER),1,0)

#-------------------------------------------------------------------------------

#----------------------------- frakturSluten -----------------------------------
grund$frakturSluten = ifelse(grepl(" S7230 ",grund$DIAGNOSER),1,0)

#-------------------------------------------------------------------------------

#----------------------------- frakturÖverUnderArm -------------------------------
grund$frakturÖverUnderArmICD10 = ifelse(grepl(" S4220? | S4240? | S52[0-9]0? ",grund$DIAGNOSER),1,0)
grund$frakturÖverUnderArmICD9 = ifelse(grepl(" 812[ACE] | 813[ACE] ",grund$DIAGNOSER),1,0)
grund$frakturÖverUnderArmICD8 = ifelse(grepl(" 812[ACE] | 813[ACE] ",grund$DIAGNOSER),1,0)##ÄNDRAS EFTER MÖTE 20150515!!

grund$frakturÖverUnderArm = ifelse(rowSums(grund[,names(grund)[grep("frakturÖverUnderArm",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- skallFraktur ------------------------------------
grund$skallFrakturICD10 = ifelse(grepl(" S020 | S021 | S027 | S028 | S029 ",grund$DIAGNOSER),1,0)


grund$skallFraktur = grund$skallFrakturICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- skadaSynnerven ----------------------------------
grund$skadaSynnervenICD10 = ifelse(grepl(" S04\\.0 ",grund$DIAGNOSER),1,0)  #FINNS INGEN

grund$skadaSynnerven = grund$skadaSynnervenICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- intrakraniellSkada ------------------------------
grund$intrakraniellSkadaICD10 = ifelse(grepl(" S060 | S061 | S062 | S063 | S064 | S065 | S066 | S067 | S068 | S069 ",grund$DIAGNOSER),1,0)


grund$intrakraniellSkada = grund$intrakraniellSkadaICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- klämskadaHuvud ----------------------------------
grund$klämskadaHuvudICD10 = ifelse(grepl(" S078 | S079 ",grund$DIAGNOSER),1,0)

grund$klämskadaHuvud = grund$klämskadaHuvudICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- ickeSpecHuvudskada ------------------------------
grund$ickeSpecHuvudskadaICD10 = ifelse(grepl(" S097 | S098 ",grund$DIAGNOSER),1,0)

grund$ickeSpecHuvudskada = grund$ickeSpecHuvudskadaICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#---------------------------- senaBesvärSkallfraktur ---------------------------
grund$senaBesvärSkallfrakturICD10 = ifelse(grepl(" T902 ",grund$DIAGNOSER),1,0)

grund$senaBesvärSkallfraktur = grund$senaBesvärSkallfrakturICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#------------------------ senaBesvärAvAndraSkallskador -------------------------
grund$senaBesvärAvAndraSkallskadorICD10 = ifelse(grepl(" T908 | T909 ",grund$DIAGNOSER),1,0)

grund$senaBesvärAvAndraSkallskador = grund$senaBesvärAvAndraSkallskadorICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- retinalBlödning ----------------------------------
grund$retinalBlödningICD10 = ifelse(grepl(" H356 | 362W ",grund$DIAGNOSER),1,0)

grund$retinalBlödning = grund$retinalBlödningICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#---------------------- subduralBlödningIckeTraumatisk -------------------------
grund$subduralBlödningIckeTraumatiskICD10 = ifelse(grepl(" I620 ",grund$DIAGNOSER),1,0)
grund$subduralBlödningIckeTraumatiskICD9 = ifelse(grepl(" 432B ",grund$DIAGNOSER),1,0)
grund$subduralBlödningIckeTraumatiskICD8 = ifelse(grepl(" 431,91 ",grund$DIAGNOSER),1,0)

grund$subduralBlödningIckeTraumatisk = ifelse(rowSums(grund[,names(grund)[grep("subduralBlödningIckeTraumatisk",names(grund))]],na.rm=T)>0,1,0)

#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------- subduralBlödningTraumatisk ----------------------------
grund$subduralBlödningTraumatiskICD10 = ifelse(grepl(" S065 ",grund$DIAGNOSER),1,0)
grund$subduralBlödningTraumatiskICD9 = ifelse(grepl(" 767A ",grund$DIAGNOSER),1,0)
grund$subduralBlödningTraumatiskICD8 = ifelse(grepl(" 772,02 ",grund$DIAGNOSER),1,0)

grund$subduralBlödningTraumatisk = ifelse(rowSums(grund[,names(grund)[grep("subduralBlödningTraumatisk",names(grund))]],na.rm=T)>0,1,0)

#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#----------------------------- Sinusventrombos ----------------------------------
grund$SinusventrombosICD10 = ifelse(grepl(" I636 ",grund$DIAGNOSER),1,0)

grund$Sinusventrombos = grund$SinusventrombosICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#------------------------- ostogenesisImperfecta -------------------------------
grund$osteogenesisImperfectaICD10 = ifelse(grepl(" Q780 ",grund$DIAGNOSER),1,0)
grund$osteogenesisImperfectaICD9 = ifelse(grepl(" 756F ",grund$DIAGNOSER),1,0)
grund$osteogenesisImperfectaICD8 = ifelse(grepl(" 756,50 ",grund$DIAGNOSER),1,0)

grund$osteogenesisImperfecta = ifelse(rowSums(grund[,names(grund)[grep("osteogenesisImperfecta",names(grund))]],na.rm=T)>0,1,0)

#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#------------------------- ostogenesisImperfecta -------------------------------
grund$spädbarnskolikICD10 = ifelse(grepl(" R104A ",grund$DIAGNOSER),1,0)

grund$spädbarnskolik = grund$spädbarnskolikICD10
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#------------------------------- Variabler nivå 2 ------------------------------

#Rakit
grund$rakit_d1 = ifelse(rowSums(grund[,names(grund)[grep("kalciumRubbningar|aktivTakit|Osteomalaci",names(grund))]],na.rm=T)>0,1,0)

#Fraktur revben
grund$frakturRevben_d1 = ifelse(rowSums(grund[,names(grund)[grep("multiplaFrakturerRevben|frakturRevben",names(grund))]],na.rm=T)>0,1,0)

#Fraktur lårben
grund$frakturLårben_d1 = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben|multiplaFrakturerLårben",names(grund))]],na.rm=T)>0,1,0)

#Fraktur underben
grund$frakturUnderben_d1 = ifelse(rowSums(grund[,names(grund)[grep("underbensFrakturer|frakturUnderben",names(grund))]],na.rm=T)>0,1,0)

#Övrig Skallskada
grund$övrigSkallskada_d1 = ifelse(rowSums(grund[,names(grund)[grep("intrakraniellSkada|klämskadaHuvud|ickeSpecHuvudskad",names(grund))]],na.rm=T)>0,1,0)

#variabler som redan finns

#Barnisshandel
grund$barnMisshandel_d1 = grund$misstankeOmBarnMisshdel

#armfraktur
grund$frakturÖverUnderArm_d1 = grund$frakturÖverUnderArm

#skallfraktur
grund$skallFraktur_d1 = grund$skallFraktur

#retinalblödning
grund$retinalBlödning_d1 = grund$retinalBlödning

#subduralblödning icje traumatisk
grund$subduralIT_d1 =  grund$subduralBlödningIckeTraumatisk

#subduralblödning traumatisk
grund$subduralTraum_d1 =  grund$subduralBlödningTraumatisk

#sinusventrombos
grund$svt_d1 = grund$Sinusventrombos

#Osteogenesis
grund$ogi_d1 = grund$osteogenesisImperfecta

#Spädbarnskolik
grund$spädbarnskolik_d1 = grund$spädbarnskolik

#-------------------------------------------------------------------------------

#------------------------------- Variabler nivå 3 ------------------------------


#fraktur rörben
grund$frakturRörben_d2 = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben_d1|frakturUnderben_d1|frakturÖverUnderArm_d1",names(grund))]],na.rm=T)>0,1,0)

#Subdural blödning
grund$subduralBlödning_d2 = ifelse(rowSums(grund[,names(grund)[grep("subduralTraum_d1|subduralIT_d1",names(grund))]],na.rm=T)>0,1,0)



#-------------------------------------------------------------------------------