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
library(scales)
library(rtf)
library(plyr)

path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)

#Source
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/functions/deriveParentDiagnoses.R", encoding="utf-8")
#-------------------------------------------------------------------------------

#------------------------------- mor orsaker -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
dod_for0_mor = read.csv(file.path(path,"Indata", "mor", "sos_mor","DOD_6113                        .CSV"), stringsAsFactors=F)
dod_for14_15_mor = read.csv(file.path(path,"Indata", "mor", "sos_mor","DOD_2014_2015                   .CSV"), stringsAsFactors=F)
dod_mor00 = read.csv(file.path(path,"Indata","Komplettering","mor","DOD                             .CSV"),stringsAsFactors=F)

# komplettereing 2015
dod_mor00a = read.csv(file.path(path,"Indata","Komplettering_2","sos","DOD_6114                        .CSV"),stringsAsFactors=F)
dod_mor00b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","DOD_6114                        .CSV"),stringsAsFactors=F)

komplettOct = rbind.fill(dod_mor00a,dod_mor00b)

#remove mothers using mfr
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_mor,mfr_kompl2a$lpnr_mor))

#insert komplettering cases
dod_for0_mor = rbind(dod_for0_mor,dod_mor00)


dod1_mor = rbind.fill(dod_for0_mor,dod_for14_15_mor,komplettOct)


cols = c(names(dod1_mor)[grep("ORSAK",names(dod1_mor))])

dod1_mor$DIAGNOSER <- apply(dod1_mor[ , cols ] , 1 , paste , collapse = " ")
dod1_mor$DIAGNOSER = paste(" ",dod1_mor$DIAGNOSER," ", sep="") 
dod1_mor$EKO = dod1_mor$DIAGNOSER

#important vars 
impvars = c("DODSDAT", "lpnr","DIAGNOSER","EKO")


dod2_mor = dod1_mor[,impvars]

dod3_mor = deriveParentDiagnoses(dod2_mor)


vars = c("DODSDAT", "lpnr","självmordsförsökICD10", "självmordsförsök", "suicidICD10", "suicid", 
"övergreppMisshMordDråpICD10", "övergreppMisshMordDråp","mordDråpICD10","mordDråp")

dod4_mor = dod3_mor[,vars]
#-------------------------------------------------------------------------------

#------------------------------- far orsaker -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
dod_for0_far = read.csv2(file.path(path,"Indata", "far", "SOS","DÖDSORSAKER","dod_6113.csv"), stringsAsFactors=F)
dod_for14_15_far = read.csv2(file.path(path,"Indata", "far", "SOS","DÖDSORSAKER","dod_2014_2015.csv"), stringsAsFactors=F)


dod1_far = rbind.fill(dod_for0_far,dod_for14_15_far)


cols = c(names(dod1_far)[grep("ORSAK",names(dod1_far))])

dod1_far$DIAGNOSER <- apply(dod1_far[ , cols ] , 1 , paste , collapse = " ")
dod1_far$DIAGNOSER = paste(" ",dod1_far$DIAGNOSER," ", sep="") 
dod1_far$EKO = dod1_far$DIAGNOSER

#important vars 
impvars = c("DODSDAT", "lpnr","DIAGNOSER","EKO")


dod2_far = dod1_far[,impvars]

dod3_far = deriveParentDiagnoses(dod2_far)


vars = c("DODSDAT", "lpnr","självmordsförsökICD10", "självmordsförsök", "suicidICD10", "suicid", 
         "övergreppMisshMordDråpICD10", "övergreppMisshMordDråp","mordDråpICD10","mordDråp")

dod4_far = dod3_far[,vars]
#-------------------------------------------------------------------------------


#--------------------------- prepare for print ---------------------------------
DMP_07_dodfor_mor = dod4_mor
DMP_07_dodfor_far = dod4_far

save(DMP_07_dodfor_mor, file=file.path(path, "Output","DMP","DMP_07_dodfor_mor.Rdata"))
save(DMP_07_dodfor_far, file=file.path(path, "Output","DMP","DMP_07_dodfor_far.Rdata"))