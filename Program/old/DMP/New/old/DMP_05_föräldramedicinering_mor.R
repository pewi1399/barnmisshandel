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

path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)

#Source
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/functions/deriveATC.R", encoding="utf-8")
#-------------------------------------------------------------------------------

#------------------------------- föräldramedicin -----------------------------------
#read in mfr from grunddata and mfr from mfr_1
lak_föräldra0 = read.csv(file.path(path,"Indata","mor","sos_mor","LAKMEDEL                        .CSV"), stringsAsFactors=F)

#important vars 
impvars = c("lpnr", "EDATUM", "FDATUM", "atc")

lak_föräldra1 = lak_föräldra0[,impvars]

#introduce diagnoses
lak_föräldra2 = deriveATC(lak_föräldra1)

#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_05_föräldramedicin = lak_föräldra2

save(DMP_05_föräldramedicin, file=file.path(path, "Output","DMP","DMP_05_föräldramedicin_mor.Rdata"))
#EOF