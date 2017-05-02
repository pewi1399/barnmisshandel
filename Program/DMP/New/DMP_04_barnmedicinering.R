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
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/functions/deriveATC.R", encoding="utf-8")
#-------------------------------------------------------------------------------

#------------------------------- barnmedicin -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
lak_barn0 = read.csv(file.path(path,"Indata","SOS_BARN","SOS_BARN","LAKMEDEL                        .CSV"), stringsAsFactors=F)

#read in data from komplettering august 2015
lak_barn05 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2005                        .CSV"),stringsAsFactors=F)
lak_barn06 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2006                        .CSV"),stringsAsFactors=F)
lak_barn07 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2007                        .CSV"),stringsAsFactors=F)
lak_barn08 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2008                        .CSV"),stringsAsFactors=F)
lak_barn09 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2009                        .CSV"),stringsAsFactors=F)
lak_barn10 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2010                        .CSV"),stringsAsFactors=F)
lak_barn11 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2011                        .CSV"),stringsAsFactors=F)
lak_barn12 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2012                        .CSV"),stringsAsFactors=F)
lak_barn13 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2013                        .CSV"),stringsAsFactors=F)
lak_barn14 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2014                        .CSV"),stringsAsFactors=F)
lak_barn15 = read.csv(file.path(path,"Indata","Komplettering","BARN","LMED2015                        .CSV"),stringsAsFactors=F)

#read in data from komplettering october 2015
lak_barn05a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2005                        .CSV"),stringsAsFactors=F)
lak_barn06a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2006                        .CSV"),stringsAsFactors=F)
lak_barn07a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2007                        .CSV"),stringsAsFactors=F)
lak_barn08a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2008                        .CSV"),stringsAsFactors=F)
lak_barn09a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2009                        .CSV"),stringsAsFactors=F)
lak_barn10a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2010                        .CSV"),stringsAsFactors=F)
lak_barn11a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2011                        .CSV"),stringsAsFactors=F)
lak_barn12a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2012                        .CSV"),stringsAsFactors=F)
lak_barn13a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2013                        .CSV"),stringsAsFactors=F)
lak_barn14a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2014                        .CSV"),stringsAsFactors=F)
lak_barn15a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2015                        .CSV"),stringsAsFactors=F)

lak_barn05b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2005                        .CSV"),stringsAsFactors=F)
lak_barn06b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2006                        .CSV"),stringsAsFactors=F)
lak_barn07b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2007                        .CSV"),stringsAsFactors=F)
lak_barn08b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2008                        .CSV"),stringsAsFactors=F)
lak_barn09b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2009                        .CSV"),stringsAsFactors=F)
lak_barn10b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2010                        .CSV"),stringsAsFactors=F)
lak_barn11b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2011                        .CSV"),stringsAsFactors=F)
lak_barn12b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2012                        .CSV"),stringsAsFactors=F)
lak_barn13b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2013                        .CSV"),stringsAsFactors=F)
lak_barn14b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2014                        .CSV"),stringsAsFactors=F)
lak_barn15b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2015                        .CSV"),stringsAsFactors=F)


komplettOct = rbind.fill(lak_barn05a,
                         lak_barn06a,
                         lak_barn07a,
                         lak_barn08a,
                         lak_barn09a,
                         lak_barn10a,
                         lak_barn11a,
                         lak_barn12a,
                         lak_barn13a,
                         lak_barn14a,
                         lak_barn15a,
                         lak_barn05b,
                         lak_barn06b,
                         lak_barn07b,
                         lak_barn08b,
                         lak_barn09b,
                         lak_barn10b,
                         lak_barn11b,
                         lak_barn12b,
                         lak_barn13b,
                         lak_barn14b,
                         lak_barn15b)

mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_BARN,mfr_kompl2a$lpnr_BARN))


lak_barn0 = rbind.fill(lak_barn0,
           lak_barn05,
           lak_barn06,
           lak_barn07,
           lak_barn08,
           lak_barn09,
           lak_barn10,
           lak_barn11,
           lak_barn12,
           lak_barn13,
           lak_barn14,
           lak_barn15,
           komplettOct)


#important vars 
impvars = c("lpnr", "EDATUM", "FDATUM", "atc")

lak_barn1 = lak_barn0[,impvars]

#introduce diagnoses
lak_barn2 = deriveATC(lak_barn1)

#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_04_barnmedicin = lak_barn2

save(DMP_04_barnmedicin, file=file.path(path, "Output","DMP","DMP_04_barnmedicin.Rdata"))
#EOF