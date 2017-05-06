
#------------------------ prepare case ids and dates ---------------------------
barnDiag0 = DMP_02_barndiagnoser
keyfile0 = keyfile

#for tabulation only misshdel cases are neccesary
barnDiag1 = subset(barnDiag0,barnMisshandel_d1==1)
barnDiag1$INDATUM.1 = NULL

#sum(table(unique(barnDiag1$lpnr)))
#481 st looking quite good

#Include only lpnr, diagnosis and date of diagnosis 
barnDiag2 = barnDiag1[,c("lpnr","barnMisshandel_d1","INDATUM")]

#merge on keyfile
barnDiag3 = merge(barnDiag2,keyfile0,by.x="lpnr",by.y="lpnr_BARN")

#set as date
barnDiag3$misshandelsDatum = as.Date(barnDiag3$INDATUM,format="%Y-%m-%d")
barnDiag3$INDATUM = NULL

barnDiag3$lpnr_barn = barnDiag3$lpnr
barnDiag3$lpnr = NULL


#calculate first case per id
barnDiag4 = data.table(barnDiag3)
barnDiag4 = barnDiag4[,list(lpnr_mor = unique(lpnr_mor),
                            misshandelsDatum= min(misshandelsDatum)),
                      by="lpnr_barn"]
barnDiag4 = data.frame(barnDiag4)
#-------------------------------------------------------------------------------

#--------------------- extract parents diagnoses -------------------------------
parentDiag0 = DMP_03_föräldradiagnoser

#remove subscores
parentDiag1 = parentDiag0[,names(parentDiag0)[!grepl("ICD",names(parentDiag0))]]

dia0 = merge(parentDiag1,barnDiag4,by.x="lpnr",by.y="lpnr_mor")

dia0$INDATUM= as.Date(dia0$INDATUM,format="%Y-%m-%d")

#calculate time difference as date minus event date negative values indicate
#diagnosis was present at time of event
dia0$timeDiff = difftime(dia0$INDATUM,dia0$misshandelsDatum,units="days")

dia0$timeClass = ifelse(dia0$timeDiff< -365.24,"Before event",
                        ifelse(dia0$timeDiff>-365.24 & dia0$timeDiff<=0,"Year before event","After event"))
dia1 = data.table(dia0)

#create table of diagnoses per timeclass and lopnr
dia2 = dia1[,list(psykdiagnos = sum(psykdiagnos),
                  bipolärSjukdom = sum(bipolärSjukdom),
                  depression = sum(depression),
                  neuroticism = sum(neuroticism),
                  postpartumDepr = sum(postpartumDepr),
                  ADHD = sum(ADHD),
                  autismSpektrum = sum(autismSpektrum),
                  postpartumPsykos = sum(postpartumPsykos),
                  alkohol = sum(alkohol),
                  drogProblem = sum(drogProblem),
                  PersonlighetsStörning = sum(PersonlighetsStörning)
),
by=c("lpnr","timeClass")]

dia2$timeClass = as.factor(dia2$timeClass)

dia2$timeClass = factor(dia2$timeClass,levels(dia2$timeClass)[c(2,3,1)])

setkey(dia2,timeClass)

#remove dupes we are only interested in first diagnosis
dia3 = data.frame(dia2[!duplicated(dia2[,c("lpnr"),with=F]),,])

tabvars = names(dia3)[!grepl("lpnr|timeClass",names(dia3))]

dia3[,tabvars] = lapply(dia3[,tabvars],function(x){ifelse(x>0,1,0)})

dia3 = data.table(dia3)

#Create final table 
dia4 = dia3[,list(psykdiagnos = sum(psykdiagnos),
                  bipolärSjukdom = sum(bipolärSjukdom),
                  depression = sum(depression),
                  neuroticism = sum(neuroticism),
                  postpartumDepr = sum(postpartumDepr),
                  ADHD = sum(ADHD),
                  autismSpektrum = sum(autismSpektrum),
                  postpartumPsykos = sum(postpartumPsykos),
                  alkohol = sum(alkohol),
                  drogProblem = sum(drogProblem),
                  PersonlighetsStörning = sum(PersonlighetsStörning)
),
by=c("timeClass")]

dia5 = melt(dia4,id="timeClass")

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(dia5,old =names(dia5) , new = c("Tidpunkt","Diagnos","Antal"))

dia5 = dia5[,c("Diagnos","Tidpunkt","Antal"),with=F]

dia6 = ddply(dia5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------


#---------------------- extract parents medicine -------------------------------
parentLak0 = data.frame(DMP_05_föräldramedicin)

#remove subscores
lak0 = merge(parentLak0,barnDiag4,by.x="lpnr",by.y="lpnr_mor")

lak0$FDATUM = as.Date(lak0$FDATUM,format="%Y-%m-%d")

#calculate time difference as date minus event date negative values indicate
#Mednosis was present at time of event
lak0$timeDiff = difftime(lak0$FDATUM,lak0$misshandelsDatum,units="days")

lak0$timeClass = ifelse(lak0$timeDiff< -365.24,"Before event",
                        ifelse(lak0$timeDiff>-365.24 & lak0$timeDiff<=0,"Year before event","After event"))
lak1 = data.table(lak0)

#create table of Mednoses per timeclass and lopnr
lak2 = lak1[,list(neruoleptika = sum(neruoleptika),
                  bensoSläkt = sum(bensoSläkt),
                  lugnandeAtaraktika = sum(lugnandeAtaraktika),
                  sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel),
                  bensoDerivat = sum(bensoDerivat),
                  N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn),
                  N06AAntidepressiva = sum(N06AAntidepressiva),
                  psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism),
                  antabus = sum(antabus),
                  antacida = sum(antacida),
                  magsår = sum(magsår)
),
by=c("lpnr","timeClass")]

lak2$timeClass = as.factor(lak2$timeClass)

lak2$timeClass = factor(lak2$timeClass,levels(lak2$timeClass)[c(2,3,1)])

setkey(lak2,timeClass)

#remove dupes we are only interested in first Medication
lak3 = data.frame(lak2[!duplicated(lak2[,c("lpnr"),with=F]),,])

tabvars = names(lak3)[!grepl("lpnr|timeClass",names(lak3))]

lak3[,tabvars] = lapply(lak3[,tabvars],function(x){ifelse(x>0,1,0)})

lak3 = data.table(lak3)

#Create final table 
lak4 = lak3[,list(neruoleptika = sum(neruoleptika),
                  bensoSläkt = sum(bensoSläkt),
                  lugnandeAtaraktika = sum(lugnandeAtaraktika),
                  sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel),
                  bensoDerivat = sum(bensoDerivat),
                  N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn),
                  N06AAntidepressiva = sum(N06AAntidepressiva),
                  psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism),
                  antabus = sum(antabus),
                  antacida = sum(antacida),
                  magsår = sum(magsår)
),
by=c("timeClass")]

lak5 = melt(lak4,id="timeClass")

setnames(lak5,old =names(lak5) , new = c("Tidpunkt","Läkemedel","Antal"))

lak5 = lak5[,c("Läkemedel","Tidpunkt","Antal"),with=F]

lak6 = ddply(lak5, .(Läkemedel),rowAdder)

#-------------------------------------------------------------------------------
