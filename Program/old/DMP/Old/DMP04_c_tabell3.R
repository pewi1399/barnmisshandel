#------------------------------ psykdiagnos ------------------------------------
grund$psykdiagnosICD10 = ifelse(grepl(" F[0-9][0-9]",grund$DIAGNOSER),1,0)
grund$psykdiagnosICD9 = ifelse(grepl("29[0-9]|30[0-9]",grund$DIAGNOSER),1,0)
grund$psykdiagnosICD8 = ifelse(grepl("29[0-9],[0-9][0-9]|30[0-9],[0-9][0-9]",grund$DIAGNOSER),1,0)

grund$psykdiagnos = ifelse(rowSums(grund[,names(grund)[grep("psykdiagnos",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ bipolärSjukdom ---------------------------------
grund$bipolärSjukdomICD10 = ifelse(grepl(" F31",grund$DIAGNOSER),1,0)
grund$bipolärSjukdomICD9 = ifelse(grepl(" 269[ABCDEW] ",grund$DIAGNOSER),1,0)
grund$bipolärSjukdomICD8 = ifelse(grepl(" 296,[0-9][0-9] ",grund$DIAGNOSER),1,0)

grund$bipolärSjukdom = ifelse(rowSums(grund[,names(grund)[grep("bipolärSjukdom",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ Depression -------------------------------------
grund$depressionICD10 = ifelse(grepl(" F3[2-4]",grund$DIAGNOSER),1,0)
grund$depressionICD9 = ifelse(grepl(" 311| 300E",grund$DIAGNOSER),1,0)
grund$depressionICD8 = ifelse(grepl(" 300,40",grund$DIAGNOSER),1,0)

grund$depression = ifelse(rowSums(grund[,names(grund)[grep("depression",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ neuroticism ------------------------------------
grund$neuroticismICD10 = ifelse(grepl(" F4[0124] ",grund$DIAGNOSER),1,0)
grund$neuroticismICD9 = ifelse(grepl(" 300[ABCDEF] ",grund$DIAGNOSER),1,0)
grund$neuroticismICD8 = ifelse(grepl(" 300,[2-5]0 ",grund$DIAGNOSER),1,0)

grund$neuroticism = ifelse(rowSums(grund[,names(grund)[grep("neuroticism",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ postpartumDepr ---------------------------------
grund$postpartumDeprICD10 = ifelse(grepl(" F53 | F53.9 ",grund$DIAGNOSER),1,0)
grund$postpartumDeprICD9 = ifelse(grepl(" 648E | 311 | 300E | 306 | 307X | 309 ",grund$DIAGNOSER),1,0)

grund$postpartumDepr = ifelse(rowSums(grund[,names(grund)[grep("postpartumDepr",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ ADHD -------------------------------------------
grund$ADHDICD10 = ifelse(grepl(" F90",grund$DIAGNOSER),1,0)
grund$ADHDICD9 = ifelse(grepl(" 314J ",grund$DIAGNOSER),1,0)
grund$ADHDICD8 = ifelse(grepl(" 308 ",grund$DIAGNOSER),1,0)

grund$ADHD = ifelse(rowSums(grund[,names(grund)[grep("ADHD",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ autismSpektrum ---------------------------------
grund$autismSpektrumICD10 = ifelse(grepl(" F84,[^23][0-9] ",grund$DIAGNOSER),1,0)
grund$autismSpektrumICD9 = ifelse(grepl(" 299A ",grund$DIAGNOSER),1,0)
grund$autismSpektrumICD8 = ifelse(grepl(" 308 ",grund$DIAGNOSER),1,0)

grund$autismSpektrum = ifelse(rowSums(grund[,names(grund)[grep("autismSpektrum",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#------------------------------ postpartumPsykos -------------------------------
grund$postpartumPsykosICD10 = ifelse(grepl(" F53,1 ",grund$DIAGNOSER),1,0)
grund$postpartumPsykosICD9 = ifelse(grepl(" 29[5-9] | 645E ",grund$DIAGNOSER),1,0)
grund$postpartumPsykosICD8 = ifelse(grepl(" 294,40 ",grund$DIAGNOSER),1,0)

grund$postpartumPsykos = ifelse(rowSums(grund[,names(grund)[grep("postpartumPsykos",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#--------------------------------- alkohol -------------------------------------
grund$alkoholICD10 = ifelse(grepl(" O35.4 | Z71.4 | Z50.2 | Z50.2 | Z72.1 | F10.2 | F10 ",grund$DIAGNOSER),1,0)
grund$alkoholICD9 = ifelse(grepl(" 291[A-E] | 303 | 305A ",grund$DIAGNOSER),1,0)
grund$alkoholICD8 = ifelse(grepl(" 303,[0-9][0-9] ",grund$DIAGNOSER),1,0)

grund$alkohol = ifelse(rowSums(grund[,names(grund)[grep("alkohol",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#----------------------------- drogProblem -------------------------------------
grund$drogProblemICD10 = ifelse(grepl(" F1[1-6] | F19 ",grund$DIAGNOSER),1,0)
grund$drogProblemICD9 = ifelse(grepl(" 292[ABCWX] ",grund$DIAGNOSER),1,0)
grund$drogProblemICD8 = ifelse(grepl(" 304,[0-9][0-9] ",grund$DIAGNOSER),1,0)

grund$drogProblem = ifelse(rowSums(grund[,names(grund)[grep("drogProblem",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#----------------------------- Personlighetsstörning -------------------------------------
grund$PersonlighetsStörningICD10 = ifelse(grepl(" F6[0-9] ",grund$DIAGNOSER),1,0)
grund$PersonlighetsStörningICD9 = ifelse(grepl(" 301[ABCDEFGHJX] ",grund$DIAGNOSER),1,0)
grund$PersonlighetsStörningICD8 = ifelse(grepl(" 300,[0-9][0-9] ",grund$DIAGNOSER),1,0)

grund$PersonlighetsStörning = ifelse(rowSums(grund[,names(grund)[grep("PersonlighetsStörning",names(grund))]],na.rm=T)>0,1,0)
#sum(grund$AktivRakit)
#sum(grund$aktivRakitICD10)
#-------------------------------------------------------------------------------

#grepl("",c("F84,23","F84,00","F84,33"))
