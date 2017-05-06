# SETUP #########################################################
# NAME      : UH
#
# R-version : 3.0.2
#              
# PRODUCES  : multivariat analys
# 
# STATUS    : DRAFT
#
# AUTHOR    : Per Wikman
# CREATED   : 2015-04-08
# NOTES     : 
# 
rm(list=objects())
gc()
# Todays date
datet <- format(Sys.time(), "%x")
fileDate <- gsub("-", "", datet)

# Rversion
Rversion <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep=".")

rm(list=ls())
library(data.table)
library(ggplot2)
#library(XLConnect)
library(scales)
library(reshape2)
#library(rtf)
library(plyr)
library(magrittr)
library(dplyr)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)
#-------------------------------------------------------------------------------
load(file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_keyfile2.Rdata"))
load(file.path(path, "Output","DMP","DMP_02_barndiagnoser.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))
#load(file.path(path, "Output","DMP","DMP_04_barnmedicin.Rdata"))
load(file.path(path, "Output","DMP","DMP_05_föräldramedicin_mor.Rdata"))
load(file.path(path, "Output","DMP","DMP_05_föräldramedicin_far.Rdata"))
load(file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
load(file=file.path(path, "Output","DMP","DMP_07_dodfor_mor.Rdata"))
load(file=file.path(path, "Output","DMP","DMP_07_dodfor_far.Rdata"))

#--------------------- extract child diagnoses ---------------------------------
barnDiag0 = DMP_02_barndiagnoser

UTAN_oppen <- FALSE
if(UTAN_oppen){
  barnDiag0 <-
  barnDiag0 %>% 
    filter(SOURCE!="oppen")
}


#remove subscores
dia0 = barnDiag0#[,names(barnDiag0)[grepl("lpnr|_d",names(barnDiag0))]]
dia1 = data.table(dia0)

vars <- grep("^n_", names(dia1), value = TRUE)
dia2 <- dia1[,lapply(.SD, function(x) sum(x, na.rm=TRUE)), by = "lpnr", .SDcols = vars]

dia3 = data.frame(dia2)

tabvars = names(dia3)[!grepl("lpnr",names(dia3))]

dia3[,tabvars] = lapply(dia3[,tabvars],function(x){ifelse(x>0,1,0)})
dia3 = data.table(dia3)

kalenderVars = grep("alderDiagnosManad|diagnosInnanUtskrivning|BFODDAT",names(dia0),value=T)

timedata = dia0[,c("lpnr",kalenderVars, "lanMaltreatment")]
timedata =timedata[!duplicated(timedata),]

#merge on timevars
dia4 = merge(dia3,timedata, by = "lpnr")

#merge on all diagnoses
#tmp0 = tmp[,list(DIAGNOSER= paste(DIAGNOSER)),by="lpnr"]
#diagdata = tmp[,list(DIAGNOSER= list(DIAGNOSER)),by="lpnr"]
dia0 = data.table(dia0)
diagdata = dia0[, list(DIAGNOSER = paste(DIAGNOSER, collapse = "")), by = lpnr]

diagdata$DIAGNOSER = gsub("NA","", diagdata$DIAGNOSER) 
diagdata$DIAGNOSER = gsub(" ","_", diagdata$DIAGNOSER) 
diagdata[,DIAGNOSER:=gsub("_{2,}","_",DIAGNOSER),]
diagdata[,DIAGNOSER:=gsub("^_","",DIAGNOSER),]
diagdata[,DIAGNOSER:=gsub("_$","",DIAGNOSER),]

diagdata[,AllDiagnosesPar:=paste(unique(unlist(strsplit(DIAGNOSER, split="_"))), collapse = ' '),by = lpnr]
diagdata$DIAGNOSER = NULL
dia5 = merge(dia4,diagdata, by = "lpnr")

regs = structure(c(
  rep("Norra Sjukvårdsregionen",4),
  rep("Uppsala-Örebro",6),
  rep("Stockholms sjukvårdsregion",3),
  rep("Sydöstra sjukvårdsregionen",3),
  rep("Västra sjukvårdsregionen",2),
  rep("Södra sjukvårdsregionen",3)),
  names=
    c("Norrbotten","Jämtland","Västerbotten","Västernorrland",
      "Gävleborg","Dalarna","Uppsala","Värmland","Örebro","Västmanland",
      "Stockholm", "Gotland","Södermanland",
      "Östergötland","Jönköping","Kalmar",
      "Västra Götaland","Halland",
      "Blekinge", "Kronoberg", "Skåne"))

dia5$regionMaltreatment = regs[dia5$lanMaltreatment]



if(UTAN_oppen){
  save(dia5,file = file.path(path,"Output","DMP","DMP_100_barndiagnoserUtanOppen.Rdata"))
  out1 <- dia5[,grep("diagnosInnan|BFODDAT", names(dia5)),with=F]
  out1$year <- as.numeric(substr(out1$BFODDAT,1,4))
  out1$BFODDAT <- NULL
  save(out1,file = file.path(path,"Output","DMP","DMP_100_diagnosinnanUtanOppen.Rdata"))
}else{
  save(dia5,file = file.path(path,"Output","DMP","DMP_100_barndiagnoser.Rdata"))
  out1 <- dia5[,grep("diagnosInnan|BFODDAT", names(dia5)),with=F]
  out1$year <- as.numeric(substr(out1$BFODDAT,1,4))
  out1$BFODDAT <- NULL
  save(out1,file = file.path(path,"Output","DMP","DMP_100_diagnosinnan.Rdata"))
}
#------------------------- perinatala tillstånd --------------------------------
mfr0 = DMP_01_mfr

#remove subscores
mfr1 = mfr0[,names(mfr0)[!grepl("ICD",names(mfr0))]]

mfr1 = data.table(mfr0)

#create timeclassification 
mfr1$year = as.numeric(substr(mfr1$BFODDAT,1,4))

mfr1$yearClass = cut(mfr1$year,c(1973,1979,1989,1999,2004,2009,2013), include.lowest = T)

#create table of diagnoses per timeclass and lopnr
# mfr2 = mfr1[,list(Pclavikel_d1 = sum(Pclavikel_d1),
#                   Plårben_d1 = sum(Plårben_d1),
#                   Pöverarm_d1 = sum(Pöverarm_d1),
#                   Prörben_d1 = sum(Prörben_d1,na.rm=T),
#                   Pskallfraktur_d1 = sum(Pskallfraktur_d1,na.rm=T),
#                   PandraSkelettSkador_d1 = sum(PandraSkelettSkador_d1,na.rm=T),
#                   PandraSpecificeradeSkadorSkelett_d1 = sum(PandraSpecificeradeSkadorSkelett_d1,na.rm=T),
#                   PövrigFraktur_d1 = sum(PövrigFraktur_d1,na.rm=T),
#                   Psubdural_d1 = sum(Psubdural_d1,na.rm=T),
#                   Pintrakraniellskada_d1 = sum(Pintrakraniellskada_d1,na.rm=T),
#                   PintrakraniellIckeTrauma_d1 = sum(PintrakraniellIckeTrauma_d1,na.rm=T),
#                   PintrakraniellSkadaBlödning_d1 = sum(PintrakraniellSkadaBlödning_d1,na.rm=T)
# ),
# by=c("lpnr_BARN","yearClass")]

tabvars = names(mfr1)[grepl("_d",names(mfr1))]

mfr3 = data.frame(mfr1[,c("lpnr_BARN","yearClass",tabvars),with=F])

mfr3[,tabvars] = lapply(mfr3[,tabvars],function(x){ifelse(x>0,1,0)})

mfr3 = data.table(mfr3)

setkey(mfr3,"lpnr_BARN")
setkey(mfr1,"lpnr_BARN")

extraMfrVars = c("preeklampsi","placentaavlossning" ,"kroniskHT" , "gravididtetsDiab","pregestDiab" , 
                 "oligohydramn","värkrubbningar","värkrubbningarSvaghet", "värkrubbningarUtdragenFörloss", 
                 "hotandeAsfyxi", "navelsträngsKompl","feberUnderfFörloss", "sätesextraktion",
                 "påverkanFosterPgaKejsarsnitt",  "förlossningsskadaCnsBlödning", "kefalhematom", 
                 "frakturer","nervskada",  "andraFörlossSkador", "intrauterinHypoxi", "HIE", "respDistress", "pneumoni",  
                 "sepsisNyfödd", "blödningFoster", "kärnikterus", "kramper", "missbildningar")


mfr34 = merge(mfr3,mfr1[,c("lpnr_BARN",extraMfrVars),with=F])
#mfr34[,extraMfrVars ] = lapply(mfr34[,extraMfrVars ],function(x){ifelse(as.numeric(as.character(x))>0,1,0)})

mfr3 = mfr34

save(mfr3,file = file.path(path,"Output","DMP","DMP_100_perinatala_mfr.Rdata"))
#--------------------- extract parents diagnoses -------------------------------
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))

DMP_03_föräldradiagnoser_mor$src = "Mor"
DMP_03_föräldradiagnoser_far$src = "Far"

DMP_03_föräldradiagnoser_far = merge(DMP_03_föräldradiagnoser_far,key[,c("lpnr_far","lpnr_BARN")],by.x="lpnr",by.y="lpnr_far", all.x=T)
DMP_03_föräldradiagnoser_mor = merge(DMP_03_föräldradiagnoser_mor,key[,c(c("lpnr_mor","lpnr_BARN"))],by.x="lpnr",by.y="lpnr_mor", all.x=T)

parentDiag0 = rbind.fill(DMP_03_föräldradiagnoser_mor,DMP_03_föräldradiagnoser_far)

#no overlap good!
#table(is.na(parentDiag0b$"lpnr_BARN.x"), is.na(parentDiag0b$"lpnr_BARN.y"))

#parentDiag0b$lpnr_BARN = ifelse(is.na(parentDiag0b$"lpnr_BARN.x"),parentDiag0b$"lpnr_BARN.y",
#                           ifelse(is.na(parentDiag0b$"lpnr_BARN.y"),parentDiag0b$"lpnr_BARN.x",NA))

#parentDiag0b$"lpnr_BARN.y" = NULL
#parentDiag0b$"lpnr_BARN.x" = NULL

#table(DMP_03_föräldradiagnoser_mor$lpnr %in% DMP_03_föräldradiagnoser_far$lpnr)
#remove subscores
pdia0 = parentDiag0[,names(parentDiag0)[!grepl("ICD",names(parentDiag0))]]

pdia1 = data.frame(pdia0)

#----------------------- Add misshdelsclassification ---------------------------


misshBarn0 = DMP_02_barndiagnoser
keyfile0 = keyfile

#for tabulation only misshdel cases are neccesary
misshBarn1 = subset(misshBarn0,barnMisshandel_d1==1)
misshBarn1$INDATUM.1 = NULL

#sum(table(unique(misshBarn1$lpnr)))

#Include only lpnr, diagnosis and date of diagnosis 
misshBarn2 = misshBarn1[,c("lpnr","barnMisshandel_d1","INDATUM")]

#merge on keyfile
misshBarn3 = merge(misshBarn2,keyfile0,by.x="lpnr",by.y="lpnr_BARN")

#set as date
misshBarn3$misshandelsDatum = as.Date(misshBarn3$INDATUM,format="%Y-%m-%d")
misshBarn3$INDATUM = NULL

misshBarn3$lpnr_barn = misshBarn3$lpnr
misshBarn3$lpnr = NULL


#calculate first case per id
misshBarn4 = data.table(misshBarn3)
misshBarn4 = misshBarn4[,list(misshandelsDatum= min(misshandelsDatum)),
                      by="lpnr_barn"]
misshBarn4 = data.frame(misshBarn4)




pdia2 = merge(pdia1,misshBarn4, by.x = "lpnr_BARN", by.y="lpnr_barn",all.x=T)

#calculate time difference as date minus event date negative values indicate
#diagnosis was present at time of event
pdia2$INDATUM = as.Date(pdia2$INDATUM,format="%Y-%m-%d")

pdia2$timeDiff = difftime(pdia2$INDATUM,pdia2$misshandelsDatum,units="days")

pdia2$timeClass = ifelse(pdia2$timeDiff< -365.24,"Before event", 
                        ifelse(pdia2$timeDiff<=0,"Year before event",
                        ifelse(pdia2$timeDiff<=365.24,"Year after event",
                        ifelse(pdia2$timeDiff<=365.24*4,"1-4 years after event","5+ years after event")))) 
pdia2 = data.table(pdia2) 


#test
#table(pdia2$timeClass)
#tmp = subset(pdia2, !is.na(timeClass))

#-------------------------------------------------------------------------------
#create table of pdiagnoses per timeclass and lopnr
pdia3 = pdia2[,list(lpnr = unique(lpnr),
                  psykdiagnos = sum(psykdiagnos,na.rm=T),
                  bipolärSjukdom = sum(bipolärSjukdom,na.rm=T),
                  depression = sum(depression,na.rm=T),
                  neuroticism = sum(neuroticism,na.rm=T),
                  postpartumDepr = sum(postpartumDepr,na.rm=T),
                  ADHD = sum(ADHD,na.rm=T),
                  autismSpektrum = sum(autismSpektrum,na.rm=T),
                  postpartumPsykos = sum(postpartumPsykos,na.rm=T),
                  alkohol = sum(alkohol,na.rm=T),
                  drogProblem = sum(drogProblem,na.rm=T),
                  PersonlighetsStörning = sum(PersonlighetsStörning,na.rm=T),
                  våldtäkt = sum(våldtäkt,na.rm=T),
                  misshandelÖvergrepp = sum(misshandelÖvergrepp,na.rm=T),
                  s_diabetes = sum(s_diabetes,na.rm=T),
                  s_hyperparathyroidism = sum(s_hyperparathyroidism,na.rm=T)
),
by=c("lpnr_BARN","src")] 

pdia4 = data.frame(pdia3) 

pdia5 = melt(pdia4,id=c("lpnr_BARN","src"))

pdia6 = dcast(pdia5,lpnr_BARN~variable+src,value.var="value")

tabvars = names(pdia6)[!grepl("lpnr|src",names(pdia6))]

pdia6[,tabvars] = lapply(pdia6[,tabvars],function(x){ifelse(x>0,1,0)})

pdia6 = data.table(pdia6)

setnames(pdia6,"lpnr_BARN","lpnr")

#------------------------------- event dependency ------------------------------
pdiaEvent0 = pdia2[,list(lpnr = unique(lpnr),
            psykdiagnos = sum(psykdiagnos,na.rm=T),
            bipolärSjukdom = sum(bipolärSjukdom,na.rm=T),
            depression = sum(depression,na.rm=T),
            neuroticism = sum(neuroticism,na.rm=T),
            postpartumDepr = sum(postpartumDepr,na.rm=T),
            ADHD = sum(ADHD,na.rm=T),
            autismSpektrum = sum(autismSpektrum,na.rm=T),
            postpartumPsykos = sum(postpartumPsykos,na.rm=T),
            alkohol = sum(alkohol,na.rm=T),
            drogProblem = sum(drogProblem,na.rm=T),
            PersonlighetsStörning = sum(PersonlighetsStörning,na.rm=T),
            våldtäkt = sum(våldtäkt,na.rm=T),
            misshandelÖvergrepp = sum(misshandelÖvergrepp,na.rm=T)
),
by=c("lpnr_BARN","src","timeClass")]

pdiaEvent1 = subset(pdiaEvent0,!is.na(timeClass))

pdiaEvent1$lpnr = NULL

pdiaEvent2 = data.frame(pdiaEvent1)

pdiaEvent3 = melt(pdiaEvent2,id=c("lpnr_BARN","src","timeClass"))

pdiaEvent4 = dcast(pdiaEvent3,lpnr_BARN~variable+src+timeClass,value.var="value")

tabvars = names(pdiaEvent4)[!grepl("lpnr|src",names(pdiaEvent4))]

pdiaEvent4[,tabvars] = lapply(pdiaEvent4[,tabvars],function(x){ifelse(x>0,1,0)})

pdiaEvent4 = data.table(pdiaEvent4)

setnames(pdiaEvent4,"lpnr_BARN","lpnr")


setkey(pdiaEvent4,"lpnr")
setkey(pdia6,"lpnr")

pdia7 = pdiaEvent4[pdia6]

save(pdia7,file = file.path(path,"Output","DMP","DMP_100_diagnoser_parents.Rdata"))

#---------------------- extract parents medicine -------------------------------
DMP_05_föräldramedicin_mor$src = "Mor" 
DMP_05_föräldramedicin_far$src = "Far"

lak0 = rbind.fill(DMP_05_föräldramedicin_mor, DMP_05_föräldramedicin_far)
#table(DMP_05_föräldramedicin_mor$lpnr %in% DMP_05_föräldramedicin_far$lpnr)

lak0a = merge(lak0,key,by.x="lpnr",by.y="lpnr_far", all.x=T)
lak0a$lpnr_far = NULL
#table(is.na(lak0a$lpnr_BARN))
lak0b = merge(lak0a,key,by.x="lpnr",by.y="lpnr_mor", all.x=T)
names(lak0b)
lak0b$lpnr_far = NULL
lak0b$lpnr_mor = NULL

#no overlap good!
#table(is.na(lak0b$"lpnr_BARN.x"), is.na(lak0b$"lpnr_BARN.y"))

lak0b$lpnr_BARN = ifelse(is.na(lak0b$"lpnr_BARN.x"),lak0b$"lpnr_BARN.y",
                                ifelse(is.na(lak0b$"lpnr_BARN.y"),lak0b$"lpnr_BARN.x",NA))

lak0b$"lpnr_BARN.y" = NULL
lak0b$"lpnr_BARN.x" = NULL

lak1 = data.frame(lak0b)


#-------------------------------- add timeclass --------------------------------
lak2 = merge(lak1,misshBarn4, by.x = "lpnr_BARN", by.y="lpnr_barn",all.x=T)

#calculate time difference as date minus event date negative values indicate
#diagnosis was present at time of event
lak2$FDATUM = as.Date(lak2$FDATUM,format="%Y-%m-%d")

lak2$timeDiff = difftime(lak2$FDATUM,lak2$misshandelsDatum,units="days")

lak2$timeClass = ifelse(lak2$timeDiff< -365.24,"Before event",
                        ifelse(lak2$timeDiff<=0,"Year before event",
                        ifelse(lak2$timeDiff<=365.24,"Year after event",
                        ifelse(lak2$timeDiff<=365.24*4,"1-4 years after event","5+ years after event")))) 
lak2 = data.table(lak2)

#-------------------------------------------------------------------------------

#create table of Mednoses per timeclass and lopnr
lak3 = lak2[,list(neruoleptika = sum(neruoleptika),
                  bensoSläkt = sum(bensoSläkt,na.rm=T),
                  lugnandeAtaraktika = sum(lugnandeAtaraktika,na.rm=T),
                  sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel,na.rm=T),
                  bensoDerivat = sum(bensoDerivat,na.rm=T),
                  N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn,na.rm=T),
                  N06AAntidepressiva = sum(N06AAntidepressiva,na.rm=T),
                  psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism,na.rm=T),
                  antabus = sum(antabus,na.rm=T),
                  antacida = sum(antacida,na.rm=T),
                  magsår = sum(magsår,na.rm=T)
),
by=c("lpnr_BARN","src")]

lak4 = data.frame(lak3)

lak5 = melt(lak4,id=c("lpnr_BARN","src"))

lak6 = dcast(lak5,lpnr_BARN~variable+src,value.var="value")

tabvars = names(lak6)[!grepl("lpnr",names(lak6))]

lak6[,tabvars] = lapply(lak6[,tabvars],function(x){ifelse(x>0,1,0)})

lak6 = data.table(lak6)

#------------------------------- event dependency ------------------------------
lakEvent0 = lak2[,list(neruoleptika = sum(neruoleptika,na.rm=T),
                       bensoSläkt = sum(bensoSläkt,na.rm=T),
                       lugnandeAtaraktika = sum(lugnandeAtaraktika,na.rm=T),
                       sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel,na.rm=T),
                       bensoDerivat = sum(bensoDerivat,na.rm=T),
                       N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn,na.rm=T),
                       N06AAntidepressiva = sum(N06AAntidepressiva,na.rm=T),
                       psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism,na.rm=T),
                       antabus = sum(antabus,na.rm=T),
                       antacida = sum(antacida,na.rm=T),
                       magsår = sum(magsår,na.rm=T)
),
by=c("lpnr_BARN","src","timeClass")]

lakEvent1 = subset(lakEvent0,!is.na(timeClass))

lakEvent2 = data.frame(lakEvent1)

lakEvent3 = melt(lakEvent2,id=c("lpnr_BARN","src","timeClass"))

lakEvent4 = dcast(lakEvent3,lpnr_BARN~variable+src+timeClass,value.var="value")

tabvars = names(lakEvent4)[!grepl("lpnr|src",names(lakEvent4))]

lakEvent4[,tabvars] = lapply(lakEvent4[,tabvars],function(x){ifelse(x>0,1,0)})

lakEvent4 = data.table(lakEvent4)

setnames(lakEvent4,"lpnr_BARN","lpnr")
setnames(lak6,"lpnr_BARN","lpnr")

setkey(lakEvent4,"lpnr")
setkey(lak6,"lpnr")

lak7 = lakEvent4[lak6]


save(lak7,file = file.path(path,"Output","DMP","DMP_100_medicine_parents.Rdata"))
#-------------------------------------------------------------------------------

#---------------------------- Give misshandels diagnoses -----------------------

barnDiag0 = DMP_02_barndiagnoser

if(UTAN_oppen){
  barnDiag0 <-
    barnDiag0 %>% 
    filter(SOURCE!="oppen")
}

keyfile0 = keyfile

#for tabulation only misshdel cases are neccesary
barnDiag1 = subset(barnDiag0,barnMisshandel_d1==1)
barnDiag1$INDATUM.1 = NULL

barnDiag1$UTDATUM = as.Date(barnDiag1$UTDATUM,"%Y-%m-%d")
barnDiag1$INDATUM = as.Date(barnDiag1$INDATUM,"%Y-%m-%d")

barnDiag1$vårddagarBarnMisshandel = difftime(barnDiag1$UTDATUM,barnDiag1$INDATUM,unit="days")
barnDiag1$vårddagarBarnMisshandel = ifelse(is.na(barnDiag1$vårddagarBarnMisshandel)|barnDiag1$vårddagarBarnMisshandel==0 ,0.5,barnDiag1$vårddagarBarnMisshandel)

#create unique case file
names(barnDiag1)

barnDiag1 = data.table(barnDiag1)

sum(table(unique(barnDiag1$lpnr)))
barnDiag1$n = 1

barnDiag2 = barnDiag1[,list(sjukhusnamn = paste(sjukhusnamn,collapse=":"),
                            klinik = paste(klinik,collapse=":"),
                            lan_text = paste(lan_text,collapse=":"),
                            INDATUM = min(INDATUM,na.rm=T),
                            UTDATUM = min(UTDATUM,na.rm=T),
                            vårddagarBarnMisshandel = sum(vårddagarBarnMisshandel),
                            MDC = gsub("NA","",paste(MDC,collapse=" ")),
                            EKO = gsub("NA","",paste(EKO,collapse="")),
                            DIAGNOSER = gsub("NA","",paste(DIAGNOSER,collapse="")),
                            antalVårdtillfällen = sum(n)
),
by="lpnr"]

#sum(table(unique(barnDiag2$lpnr)))
#names(barnDiag1)

#spaces
#MDC   EKO   DIAGNOSER
spacevars = c("MDC", "EKO", "DIAGNOSER")

barnDiag2 = data.frame(barnDiag2)

#remove extra spaces
barnDiag2[,spacevars] = lapply(barnDiag2[,spacevars], function(x){gsub(" {2,}"," ",x)})
barnDiag2[,spacevars] = lapply(barnDiag2[,spacevars], function(x){gsub("^ ","",x)})
barnDiag2[,spacevars] = lapply(barnDiag2[,spacevars], function(x){gsub(" $","",x)})

barnDiag2$kalenderÅrEvent = substr(barnDiag2$INDATUM,1,4)
barnDiag2$länVidDiagnosBarnmisshandel = gsub(":.*$","",barnDiag2$lan_text)
barnDiag2$sjukhusVidDiagnosBarnmisshandel = gsub(":.*$","",barnDiag2$sjukhusnamn)

regs = structure(c(
   rep("Norra Sjukvårdsregionen",4),
   rep("Uppsala-Örebro",6),
   rep("Stockholms sjukvårdsregion",3),
   rep("Sydöstra sjukvårdsregionen",3),
   rep("Västra sjukvårdsregionen",2),
   rep("Södra sjukvårdsregionen",3)),
   names=
   c("Norrbotten","Jämtland","Västerbotten","Västernorrland",
     "Gävleborg","Dalarna","Uppsala","Värmland","Örebro","Västmanland",
     "Stockholm", "Gotland","Södermanland",
     "Östergötland","Jönköping","Kalmar",
     "Västra Götaland","Halland",
     "Blekinge", "Kronoberg", "Skåne"))

barnDiag2$sjukvårdsRegionVidDiagnosbarnmisshandel = regs[barnDiag2$länVidDiagnosBarnmisshandel]

#------------------------- reformat diagnosis ----------------------------------
listDia = melt(lapply(strsplit(barnDiag2$DIAGNOSER," "),as.list))
diags0 = dcast(listDia, L1~L2)

diags0$L1 = NULL

names(diags0) = paste("dia", 1:ncol(diags0), sep="")

barnDiag3 = cbind(barnDiag2,diags0)

#------------------------------ reformat MDC -----------------------------------
barnDiag3$MDC[barnDiag3$MDC == ""] = "Ej Notat"
listMDC = melt(lapply(strsplit(barnDiag3$MDC," "),as.list))
MDC0 = dcast(listMDC, L1~L2)

MDC0$L1 = NULL

names(MDC0) = paste("MDC", 1:ncol(MDC0), sep="")

barnDiag4 = cbind(barnDiag3,MDC0)

#----------------------------- reformat EKO ------------------------------------
barnDiag4$EKO[barnDiag4$EKO == ""] = "Ej_Notat"
listEKO = melt(lapply(strsplit(barnDiag4$EKO," "),as.list))
EKO0 = dcast(listEKO, L1~L2)

EKO0$L1 = NULL

names(EKO0) = paste("EKO", 1:ncol(EKO0), sep="")

barnDiag5 = cbind(barnDiag4,EKO0)
#-------------------------------------------------------------------------------

barnDiag5$EKO = NULL
barnDiag5$MDC = NULL
barnDiag5$DIAGNOSER = NULL

#:
#sjukhusnamn   klinik   lan_text
clnvars = c("sjukhusnamn", "klinik", "lan_text")

#---------------------------- reformat Sjukhus ---------------------------------
listSJKH = melt(lapply(strsplit(barnDiag5$sjukhusnamn,":"),as.list))
sjukhusnamn0 = dcast(listSJKH, L1~L2)

sjukhusnamn0$L1 = NULL

names(sjukhusnamn0) = paste("SJUKHUS", 1:ncol(sjukhusnamn0), sep="")

barnDiag6 = cbind(barnDiag5,sjukhusnamn0)

#---------------------------- reformat klinik ----------------------------------
listKlinik = melt(lapply(strsplit(barnDiag5$klinik,":"),as.list))
klinik0 = dcast(listKlinik, L1~L2)

klinik0$L1 = NULL

names(klinik0) = paste("KLINIK", 1:ncol(klinik0), sep="")

barnDiag7 = cbind(barnDiag6,klinik0)

#---------------------------- reformat lan_text --------------------------------
listlan_text = melt(lapply(strsplit(barnDiag5$lan_text,":"),as.list))
lan_text0 = dcast(listlan_text, L1~L2)

lan_text0$L1 = NULL

names(lan_text0) = paste("lan_text", 1:ncol(lan_text0), sep="")

barnDiag8 = cbind(barnDiag7,lan_text0)
#-------------------------------------------------------------------------------

#barnDiag8$sjukhusnamn = NULL
#barnDiag8$klinik = NULL
#barnDiag8$lan_text = NULL



if(UTAN_oppen){
  save(barnDiag8,file = file.path(path,"Output","DMP","DMP_100_barndiagnoserMisshandelUtanOppen.Rdata"))
}else{
  save(barnDiag8,file = file.path(path,"Output","DMP","DMP_100_barndiagnoserMisshandel.Rdata"))
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------- dödsorsaker föräldrar ------------------------------
#-------------------------------------------------------------------------------

#ladda in diagnosdata
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))

#markera vilken typ av löpnummer det rör sig om 
DMP_03_föräldradiagnoser_far$src = "far"
DMP_03_föräldradiagnoser_mor$src = "mor"

#För alla
fDod0 = data.table(rbind(DMP_03_föräldradiagnoser_far,DMP_03_föräldradiagnoser_mor))

#summera antal diagnoser per individ samt första datum 
fDod1 = fDod0[,list(självmordsförsök = sum(självmordsförsök,na.rm=T),
                    suicid = sum(suicid,na.rm=T),
                    övergreppMisshMordDråp = sum(övergreppMisshMordDråp,na.rm=T),
                    mordDråp = sum(mordDråp,na.rm=T),
                    INDATUM = min(INDATUM,na.rm=T),
                    src = unique(src)
),by = "lpnr"]

fDod1 = data.frame(fDod1)

#är endast intresserade om diagnos ej hur många. dikotomisera.
vars = c("självmordsförsök", "suicid", "övergreppMisshMordDråp", "mordDråp")
fDod1[,vars] = lapply(fDod1[,vars],function(x){ifelse(x>0,1,0)})

#kontrollera om självmord/suicid m.h.a. dödsorsaksregistret
load(file=file.path(path, "Output","DMP","DMP_07_dodfor_mor.Rdata"))
load(file=file.path(path, "Output","DMP","DMP_07_dodfor_far.Rdata"))

#table(c(dodFor0$lpnr) %in% c(DMP_03_föräldradiagnoser_far$lpnr,DMP_03_föräldradiagnoser_mor$lpnr) )

#kombindera mödrar och fäder
dodFor0 = rbind.fill(DMP_07_dodfor_mor,DMP_07_dodfor_far)

#dödsdatum är det enda vi behöver inkludera 
dodFor1 = dodFor0[,c("lpnr","DODSDAT")]

#skapa dödmarkör. Eg onödigt eftersom dödsdatum finns men bra för tabellering.
dodFor1$dod = 1

#lägg till dödsmarkör till diagnoslista
fDod2 = merge(fDod1,dodFor1,by="lpnr", all.x=T)

#gör om na till 0 för strikt 0-1 variabel
fDod2$dod[is.na(fDod2$dod)] = 0

#------------- Kontrollera/skapa självmordsförsöksvariabel ---------------------
#table(fDod2$självmordsförsök ,fDod2$dod)
fDod2$självmordsförsök = ifelse(fDod2$dod==1,0,fDod2$självmordsförsök)
#table(fDod2$självmordsförsök ,fDod2$dod)

#skapa suicid variabel
#table(fDod2$suicid ,fDod2$dod)
fDod2$suicid = ifelse(fDod2$dod==1 & fDod2$suicid ==1, 1, 0)
#table(fDod2$suicid ,fDod2$dod)


#table(fDod2$övergreppMisshMordDråp,fDod2$dod)
fDod2$övergreppMisshMordDråp = ifelse(fDod2$dod==1,0,fDod2$övergreppMisshMordDråp)

#table(fDod2$mordDråp,fDod2$dod)
fDod2$mordDråp = ifelse(fDod2$dod==1 & fDod2$mordDråp == 1,1, 0)


#-------------------------------------------------------------------------------
#För de med barnmisshandelsdiagnos skapa ny nyckel
keyMissh0 = subset(key, key$lpnr_BARN %in% barnDiag1$lpnr)

#tag med första barnmishandelsdatum från barndiagnoser och koppla till ny nyckelfil
keyMissh1 = data.table(merge(keyMissh0,data.frame(barnDiag2[,c("lpnr","INDATUM")]), 
                             by.x = "lpnr_BARN", by.y = "lpnr")
)
setnames(keyMissh1,"INDATUM","DatumFörstaBarnMisshandel")

#förtydliga att lpnr i fDod härrör både från mödrar och fäder
fDod2$lpnrMF = fDod2$lpnr 
fDod2$lpnr = NULL

#Gör om till data.table för enkel merge
fDod2 = data.table(fDod2)


#för varje förälder koppla ett datum för barnmisshandel eller ej. 
#Detta måste göras i två steg för att få med både moder och fader.

#alltså föräldrar utan diagnos kommer ej att ha värden för det som kommer från nyckeln 
#keyMissh1...

fDod2 = data.frame(fDod2)
##keyMissh1mor = data.frame(keyMissh1mor)

#number of rows increases a bit since we parents tend to have more than one kid
fDod3 = merge(fDod2,key[,c("lpnr_BARN","lpnr_mor")], by.x="lpnrMF", by.y = "lpnr_mor", all.x=T)
fDod4 = merge(fDod3,key[,c("lpnr_BARN","lpnr_far")], by.x="lpnrMF", by.y = "lpnr_far", all.x=T)


fDod4$lpnr_BARN =ifelse(is.na(fDod4$lpnr_BARN.x),fDod4$lpnr_BARN.y,
                        ifelse(is.na(fDod4$lpnr_BARN.y),fDod4$lpnr_BARN.x,NA))
#table(is.na(fDod4$lpnr_BARN)) no NA

fDod4$lpnr_BARN.x = NULL
fDod4$lpnr_BARN.y = NULL

#lägg till om barnmisshandel för att kunna tidsbestämma diagnoser

fDod5 = merge(fDod4,data.frame(keyMissh1)[c("lpnr_BARN","DatumFörstaBarnMisshandel")], by="lpnr_BARN", all.x =T) 


fDod5$INDATUM = as.Date(fDod5$INDATUM, "%Y-%m-%d")
fDod5$DODSDAT = as.Date(as.character(fDod5$DODSDAT), "%Y%m%d")

# diagnosdatum/dödsdatum - barnmisshdatum
#en individ som misshandlar sitt barn 2005-01-01 och avlider ett år senare 2006-01-01 
#Får värdet 365.24 dagar
fDod5$diffDiagnosMisshandel = difftime(fDod5$INDATUM,fDod5$DatumFörstaBarnMisshandel, unit = "days")
fDod5$diffDödMisshandel = difftime(fDod5$DODSDAT,fDod5$DatumFörstaBarnMisshandel, unit = "days")

#alltså kan man omöjligen ha negativa värden för variabler som rör död eftersom det skulle innebära att
#misshandel inträffat efter döden. Anledningen till att dessa fall existerar är att
#den andre föräldern är den som misshandlat (högst troligt, det eller registerfel).

#table(fDod5$diffDödMisshandel)
fDod5$diffDödMisshandel = ifelse(fDod5$diffDödMisshandel<0,NA,fDod5$diffDödMisshandel)

fDod5$självmordsförsök_tid = ifelse(fDod5$självmordsförsök==1 & fDod5$diffDiagnosMisshandel< -365.24,1,
                                    ifelse(fDod5$självmordsförsök==1 & fDod5$diffDiagnosMisshandel<0,2,
                                           ifelse(fDod5$självmordsförsök==1 & fDod5$diffDiagnosMisshandel<365.24,3,
                                                  ifelse(fDod5$självmordsförsök==1 & fDod5$diffDiagnosMisshandel<365.24*3,4,
                                                         ifelse(fDod5$självmordsförsök==1 & fDod5$diffDiagnosMisshandel<365.24*5,5,
                                                                ifelse(fDod5$självmordsförsök==0 | is.na(fDod5$diffDiagnosMisshandel),NA,6)
                                                         )))))
#table(fDod5$självmordsförsök_tid)
#table(fDod5$suicid==1 & !is.na(fDod5$diffDiagnosMisshandel))
#tmp = subset(fDod5, !is.na(fDod5$självmordsförsök_tid))

fDod5$suicid_tid = ifelse(fDod5$suicid==1 & fDod5$diffDödMisshandel<7,1,
                          ifelse(fDod5$suicid==1 & fDod5$diffDödMisshandel<365.24,2,
                                 ifelse(fDod5$suicid==1 & fDod5$diffDödMisshandel<365.24*3,3,
                                        ifelse(fDod5$suicid==1 & fDod5$diffDödMisshandel<365.24*5,4,
                                               ifelse(fDod5$suicid==0 | is.na(fDod5$diffDödMisshandel),NA,5)
                                        ))))
#table(fDod5$suicid_tid)
#table(fDod5$suicid==1 & !is.na(fDod5$diffDödMisshandel))
#tmp = subset(fDod5, !is.na(fDod5$suicid_tid))

fDod5$övergreppMisshMordDråp_tid = ifelse(fDod5$övergreppMisshMordDråp==1 & fDod5$diffDiagnosMisshandel<0,1,
                                          ifelse(fDod5$övergreppMisshMordDråp==0 | is.na(fDod5$diffDiagnosMisshandel),NA,2
                                          ))

#table(fDod5$övergreppMisshMordDråp_tid)
#table(fDod5$övergreppMisshMordDråp==1 & !is.na(fDod5$diffDiagnosMisshandel))
tmp = subset(fDod5, !is.na(fDod5$övergreppMisshMordDråp_tid))

fDod5$mordDråp_tid = ifelse(fDod5$mordDråp==1 & fDod5$diffDödMisshandel<7,1,
                            ifelse(fDod5$mordDråp==0 | is.na(fDod5$diffDödMisshandel),NA,2
                            ))

#ingen mördad barnmisshandelsmisstänkt.
#table(fDod5$mordDråp_tid)
#table(fDod5$mordDråp==1 & !is.na(fDod5$diffDödMisshandel))
#tmp = subset(fDod5, !is.na(fDod5$mordDråp_tid))
#---------------------- Smält data för uppdelning mor far ----------------------

fDod5$lpnr = fDod5$lpnrMF
fDod5$lpnrMF = NULL

fDod6 = melt(fDod5,id = c("lpnr_BARN","src","DatumFörstaBarnMisshandel"))

fDod7 = dcast(fDod6,lpnr_BARN+DatumFörstaBarnMisshandel~variable+src,value.var = "value")

save(fDod7,file = file.path(path,"Output","DMP","DMP_100_suicidMord_far_mor.Rdata"))
#-------------------------------------------------------------------------------

#------------------------------ Dödsorsaker barn -------------------------------
load(file=file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))

dodBarn0 = DMP_06_dodBarn

names(dodBarn0)[!grepl("lpnr",names(dodBarn0))] = paste(names(dodBarn0)[!grepl("lpnr",names(dodBarn0))],"dodbarn",sep="_")

save(dodBarn0 ,file = file.path(path,"Output","DMP","DMP_100_dodBarn.Rdata"))
#------------------------------- SNQ variabler ---------------------------------

#------------------------------ SNQ variabler ----------------------------------
load(file=file.path(path, "Output","DMP","DMP_08_snq.Rdata"))
DMP_100_snq = DMP_08_snq 
save(DMP_100_snq ,file = file.path(path,"Output","DMP","DMP_100_snq.Rdata"))
#-------------------------------------------------------------------------------
#EOF