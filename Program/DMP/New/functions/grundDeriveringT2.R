#-------------------------- nyckelbensbrott tabell 2 ---------------------------
grund$PclavikelICD10 = ifelse(grepl(" P134 ",grund$DIAGNOSER),1,0)
grund$PclavikelICD9 = ifelse(grepl(" 767C ",grund$DIAGNOSER),1,0)
grund$PclavikelICD8 = ifelse(grepl(" 772,21 ",grund$DIAGNOSER),1,0)

grund$Pclavikel_d1 = ifelse(rowSums(grund[,names(grund)[grep("Pclavikel",names(grund))]],na.rm=T)>0,1,0)

#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#---------------------------- lårben tabell2  ----------------------------------
grund$PlårbenICD10 = ifelse(grepl(" P134 ",grund$DIAGNOSER),1,0)
grund$PlårbenICD9 = ifelse(grepl(" 767D ",grund$DIAGNOSER),1,0)
grund$PlårbenICD8 = ifelse(grepl(" 772,23 ",grund$DIAGNOSER),1,0)

grund$Plårben_d1 = ifelse(rowSums(grund[,names(grund)[grep("Plårben",names(grund))]],na.rm=T)>0,1,0)

#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD8)
#-------------------------------------------------------------------------------

#---------------------------- överarm tabell2  ---------------------------------
grund$PöverarmICD8 = ifelse(grepl(" 772,22 ",grund$DIAGNOSER),1,0)

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
grund$PskallfrakturICD8 = ifelse(grepl(" 772,20 ",grund$DIAGNOSER),1,0)

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
grund$PsubduralICD8 = ifelse(grepl(" 772,02 ",grund$DIAGNOSER),1,0)


grund$Psubdural_d1 = ifelse(rowSums(grund[,names(grund)[grep("Psubdural",names(grund))]],na.rm=T)>0,1,0)
#-------------------------------------------------------------------------------

#-------------------------- intrakraniellskada tabell2 -------------------------
grund$PintrakraniellskadaICD10 = ifelse(grepl(" P10 ",grund$DIAGNOSER),1,0)
grund$Pintrakraniellskada_d1 = grund$PintrakraniellskadaICD10

grund$PintrakraniellIckeTraumaICD10 = ifelse(grepl(" P52 ",grund$DIAGNOSER),1,0)
grund$PintrakraniellIckeTrauma_d1 = grund$PintrakraniellIckeTraumaICD10

grund$PintrakraniellSkadaBlödning_d1 = ifelse(rowSums(grund[,names(grund)[grep("PintrakraniellIckeTrauma|Pintrakraniellskada",names(grund))]],na.rm=T)>0,1,0)
#-------------------------------------------------------------------------------
