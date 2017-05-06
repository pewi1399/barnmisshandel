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
#source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/functions/deriveATC.R", encoding="utf-8")
#-------------------------------------------------------------------------------

#------------------------------- föräldramedicin -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
dod_barn0 = read.csv(file.path(path,"Indata", "SOS_BARN", "SOS_BARN","DOD_6113                        .CSV"), stringsAsFactors=F)
dod_barn14_15 = read.csv(file.path(path,"Indata", "SOS_BARN", "SOS_BARN","DOD_2014_2015                   .CSV"), stringsAsFactors=F)
dod_barn00 = read.csv(file.path(path,"Indata","Komplettering","BARN","DOD                             .CSV"),stringsAsFactors=F)

# komplettereing 2015
dod_barn00a = read.csv(file.path(path,"Indata","Komplettering_2","sos","DOD_6114                        .CSV"),stringsAsFactors=F)
dod_barn00b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","DOD_6114                        .CSV"),stringsAsFactors=F)

komplettOct = rbind.fill(dod_barn00a,dod_barn00b)

#remove mothers using mfr
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_BARN,mfr_kompl2a$lpnr_BARN))


#insert komplettering cases
dod_barn0 = rbind(dod_barn0,dod_barn00)

dod1 = data.table(rbind.fill(dod_barn0,dod_barn14_15,komplettOct))


#important vars 
impvars = c("DODSDATN", "lpnr","ALDER","AR","DALDDAG","DALDMAN","MORD","SKADA",
            names(dod1)[grepl("MORSAK",names(dod1))])

#temporärt för variabelval.
write.csv2(names(dod1), file=file.path(path, "Output","DMP","DMP_06_variabelnamnDOD.csv"))

dod2 = dod1[,impvars,with=F]
#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_06_dodBarn = dod2

save(DMP_06_dodBarn, file=file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
#EOF