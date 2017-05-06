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

# Todays date
datet <- format(Sys.time(), "%x")
fileDate <- gsub("-", "", datet)

# Rversion
Rversion <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep=".")

rm(list=ls())
library(data.table)
library(ggplot2)
library(XLConnect)
library(scales)
library(reshape2)
library(rtf)
library(plyr)

path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)

#-------------------------------------------------------------------------------

load(file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))
load(file.path(path, "Output","DMP","DMP_02_barndiagnoser.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.Rdata"))
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))
#load(file.path(path, "Output","DMP","DMP_04_barnmedicin.Rdata"))
load(file.path(path, "Output","DMP","DMP_05_föräldramedicin.Rdata"))
load(file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
load(file=file.path(path, "Output","DMP","DMP_07_dodfor.Rdata"))

#--------------------- extract child diagnoses ---------------------------------
barnDiag0 = DMP_02_barndiagnoser

#remove subscores
dia0 = barnDiag0[,names(barnDiag0)[grepl("lpnr|_d",names(barnDiag0))]]

dia1 = data.table(dia0)

#create table of diagnoses per timeclass and lopnr
dia2 = dia1[,list(rakit_d1 = sum(rakit_d1),
                  frakturRevben_d1 = sum(frakturRevben_d1),
                  frakturLårben_d1 = sum(frakturLårben_d1),
                  frakturUnderben_d1 = sum(frakturUnderben_d1),
                  övrigSkallskada_d1 = sum(övrigSkallskada_d1),
                  barnMisshandel_d1 = sum(barnMisshandel_d1),
                  frakturÖverUnderArm_d1 = sum(frakturÖverUnderArm_d1),
                  skallFraktur_d1 = sum(skallFraktur_d1),
                  retinalBlödning_d1 = sum(retinalBlödning_d1),
                  subduralIT_d1 = sum(subduralIT_d1),
                  subduralTraum_d1 = sum(subduralTraum_d1),
                  svt_d1 = sum(svt_d1),
                  ogi_d1 = sum(ogi_d1),
                  spädbarnskolik_d1 = sum(spädbarnskolik_d1),
                  frakturRörben_d2 = sum(frakturRörben_d2),
                  subduralBlödning_d2 = sum(subduralBlödning_d2)
),
by=c("lpnr")]

dia3 = data.frame(dia2)

tabvars = names(dia3)[!grepl("lpnr",names(dia3))]

dia3[,tabvars] = lapply(dia3[,tabvars],function(x){ifelse(x>0,1,0)})
dia3 = data.table(dia3)


#------------------------- perinatala tillstånd --------------------------------
mfr0 = DMP_01_mfr

#remove subscores
mfr1 = mfr0[,names(mfr0)[!grepl("ICD",names(mfr0))]]

mfr1 = data.table(mfr0)

#create timeclassification 
mfr1$year = as.numeric(substr(mfr1$BFODDAT,1,4))

mfr1$yearClass = cut(mfr1$year,c(1973,1979,1989,1999,2004,2009,2013), include.lowest = T)

#create table of diagnoses per timeclass and lopnr
mfr2 = mfr1[,list(Pclavikel_d1 = sum(Pclavikel_d1),
                  Plårben_d1 = sum(Plårben_d1),
                  Pöverarm_d1 = sum(Pöverarm_d1),
                  Prörben_d1 = sum(Prörben_d1),
                  Pskallfraktur_d1 = sum(Pskallfraktur_d1),
                  PandraSkelettSkador_d1 = sum(PandraSkelettSkador_d1),
                  PandraSpecificeradeSkadorSkelett_d1 = sum(PandraSpecificeradeSkadorSkelett_d1),
                  PövrigFraktur_d1 = sum(PövrigFraktur_d1),
                  Psubdural_d1 = sum(Psubdural_d1),
                  Pintrakraniellskada_d1 = sum(Pintrakraniellskada_d1),
                  PintrakraniellIckeTrauma_d1 = sum(PintrakraniellIckeTrauma_d1),
                  PintrakraniellSkadaBlödning_d1 = sum(PintrakraniellSkadaBlödning_d1)
),
by=c("lpnr_BARN","yearClass")]

mfr3 = data.frame(mfr2)

tabvars = names(mfr3)[grepl("_d",names(mfr3))]

mfr3[,tabvars] = lapply(mfr3[,tabvars],function(x){ifelse(x>0,1,0)})

mfr3 = data.table(mfr3)

#--------------------- extract parents diagnoses -------------------------------
parentDiag0 = rbind.fill(DMP_03_föräldradiagnoser_mor,DMP_03_föräldradiagnoser_far)

#remove subscores
pdia0 = parentDiag0[,names(parentDiag0)[!grepl("ICD",names(parentDiag0))]]

pdia1 = data.table(pdia0)

#create table of pdiagnoses per timeclass and lopnr
pdia2 = pdia1[,list(psykdiagnos = sum(psykdiagnos),
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
by=c("lpnr")]

pdia3 = data.frame(pdia2)

tabvars = names(pdia3)[!grepl("lpnr",names(pdia3))]

pdia3[,tabvars] = lapply(pdia3[,tabvars],function(x){ifelse(x>0,1,0)})

pdia3 = data.table(pdia3)


#---------------------- extract parents medicine -------------------------------
lak0 = data.frame(DMP_05_föräldramedicin)


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
by=c("lpnr")]

lak3 = data.frame(lak2)

tabvars = names(lak3)[!grepl("lpnr",names(lak3))]

lak3[,tabvars] = lapply(lak3[,tabvars],function(x){ifelse(x>0,1,0)})

lak3 = data.table(lak3)
