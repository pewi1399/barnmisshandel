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

#------------------------------- mor medicin -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
lak_föräldra0m = read.csv(file.path(path,"Indata","mor","sos_mor","LAKMEDEL                        .CSV"), stringsAsFactors=F)
lak_föräldra0m$lpnr = as.numeric(lak_föräldra0m$lpnr)
lak_föräldra0m = lak_föräldra0m[!is.na(lak_föräldra0m$lpnr),]


#read in data from komplettering august 2015
lak_mor05 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2005                        .CSV"),stringsAsFactors=F)
lak_mor06 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2006                        .CSV"),stringsAsFactors=F)
lak_mor07 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2007                        .CSV"),stringsAsFactors=F)
lak_mor08 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2008                        .CSV"),stringsAsFactors=F)
lak_mor09 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2009                        .CSV"),stringsAsFactors=F)
lak_mor10 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2010                        .CSV"),stringsAsFactors=F)
lak_mor11 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2011                        .CSV"),stringsAsFactors=F)
lak_mor12 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2012                        .CSV"),stringsAsFactors=F)
lak_mor13 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2013                        .CSV"),stringsAsFactors=F)
lak_mor14 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2014                        .CSV"),stringsAsFactors=F)
lak_mor15 = read.csv(file.path(path,"Indata","Komplettering","mor","LMED2015                        .CSV"),stringsAsFactors=F)

#read in data from komplettering october 2015
lak_mor05a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2005                        .CSV"),stringsAsFactors=F)
lak_mor06a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2006                        .CSV"),stringsAsFactors=F)
lak_mor07a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2007                        .CSV"),stringsAsFactors=F)
lak_mor08a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2008                        .CSV"),stringsAsFactors=F)
lak_mor09a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2009                        .CSV"),stringsAsFactors=F)
lak_mor10a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2010                        .CSV"),stringsAsFactors=F)
lak_mor11a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2011                        .CSV"),stringsAsFactors=F)
lak_mor12a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2012                        .CSV"),stringsAsFactors=F)
lak_mor13a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2013                        .CSV"),stringsAsFactors=F)
lak_mor14a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2014                        .CSV"),stringsAsFactors=F)
lak_mor15a = read.csv(file.path(path,"Indata","Komplettering_2","sos","LMED2015                        .CSV"),stringsAsFactors=F)

lak_mor05b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2005                        .CSV"),stringsAsFactors=F)
lak_mor06b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2006                        .CSV"),stringsAsFactors=F)
lak_mor07b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2007                        .CSV"),stringsAsFactors=F)
lak_mor08b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2008                        .CSV"),stringsAsFactors=F)
lak_mor09b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2009                        .CSV"),stringsAsFactors=F)
lak_mor10b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2010                        .CSV"),stringsAsFactors=F)
lak_mor11b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2011                        .CSV"),stringsAsFactors=F)
lak_mor12b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2012                        .CSV"),stringsAsFactors=F)
lak_mor13b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2013                        .CSV"),stringsAsFactors=F)
lak_mor14b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2014                        .CSV"),stringsAsFactors=F)
lak_mor15b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","LMED2015                        .CSV"),stringsAsFactors=F)


komplettOct = rbind.fill(lak_mor05a,
                         lak_mor06a,
                         lak_mor07a,
                         lak_mor08a,
                         lak_mor09a,
                         lak_mor10a,
                         lak_mor11a,
                         lak_mor12a,
                         lak_mor13a,
                         lak_mor14a,
                         lak_mor15a,
                         lak_mor05b,
                         lak_mor06b,
                         lak_mor07b,
                         lak_mor08b,
                         lak_mor09b,
                         lak_mor10b,
                         lak_mor11b,
                         lak_mor12b,
                         lak_mor13b,
                         lak_mor14b,
                         lak_mor15b)

mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_mor,mfr_kompl2a$lpnr_mor))

lak_föräldra0m = rbind.fill(lak_föräldra0m,
                       lak_mor05,
                       lak_mor06,
                       lak_mor07,
                       lak_mor08,
                       lak_mor09,
                       lak_mor10,
                       lak_mor11,
                       lak_mor12,
                       lak_mor13,
                       lak_mor14,
                       lak_mor15,
                       komplettOct)


#important vars 
impvars = c("lpnr", "EDATUM", "FDATUM", "atc")

lak_föräldra1m = lak_föräldra0m[,impvars]

#introduce diagnoses
lak_föräldra2m = deriveATC(lak_föräldra1m)

#-------------------------------------------------------------------------------

#------------------------------- far medicin -----------------------------------

fa_files = list.files(file.path(path,"Indata","far","SOS","LÄKEMEDELREG"))
files = fa_files[grepl("\\.CSV",fa_files)]

ll = lapply(files,function(x){read.csv(file.path(path,"Indata","far",
                                                  "SOS","LÄKEMEDELREG",x), stringsAsFactors=F)})


lak_föräldra0f = do.call("rbind.fill",ll)

#important vars 
impvars = c("lpnr", "EDATUM", "FDATUM", "atc")

lak_föräldra1f = lak_föräldra0f[,impvars]

#introduce diagnoses
lak_föräldra2f = deriveATC(lak_föräldra1f)



#test

#
#lak_föräldra2f$year = as.numeric(substr(lak_föräldra2f$EDATUM,1,4))
#
#lakTidiga = subset(lak_föräldra2f,lak_föräldra2f$year<200)



#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_05_föräldramedicin_mor = lak_föräldra2m
write.csv2(DMP_05_föräldramedicin_mor, file=file.path(path, "Output","DMP","DMP_05_föräldramedicin_mor.csv"),na="",row.names=F)
save(DMP_05_föräldramedicin_mor, file=file.path(path, "Output","DMP","DMP_05_föräldramedicin_mor.Rdata"))


DMP_05_föräldramedicin_far = lak_föräldra2f

write.csv2(DMP_05_föräldramedicin_far, file=file.path(path, "Output","DMP","DMP_05_föräldramedicin_far.csv"),na="",row.names=F)
save(DMP_05_föräldramedicin_far, file=file.path(path, "Output","DMP","DMP_05_föräldramedicin_far.Rdata"))
#EOF