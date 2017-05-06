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
load(file.path(path, "Output","DMP","DMP_03_föräldradiagnoser.Rdata"))
#load(file.path(path, "Output","DMP","DMP_04_barnmedicin.Rdata"))
load(file.path(path, "Output","DMP","DMP_05_föräldramedicin.Rdata"))
load(file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
load(file=file.path(path, "Output","DMP","DMP_07_dodfor.Rdata"))

#open excel file and create sheets
wb = loadWorkbook(file.path(path,"Output","TLP","TLP_01_grundtabeller.xlsx"),create=T)
createSheet(wb,"Barndiagnoser")
createSheet(wb,"Perinatala tillstånd")
createSheet(wb,"Psykdiagnoser (Mor)")
createSheet(wb,"Psykdiagnoser bmh (Mor)")
createSheet(wb,"Läkemedel (Mor)")
createSheet(wb,"Läkemedel bmh (Mor)")
createSheet(wb,"MFR Morbarn")
createSheet(wb,"DOD")

setColumnWidth(wb, sheet = c(1,3,4,5,6,7,8), column = 1, width = 8000)
setColumnWidth(wb, sheet = c(2), column = 1, width = 10000)
setColumnWidth(wb, sheet = c(1,2,3,4,5,6,7,8), column = 2, width = 5000)
#-------------------------------------------------------------------------------

#-------------------------- Source table scripts -------------------------------

#Creates lak6 and dia6 which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_010_diagnoses in  relation to event.R", encoding="utf-8")
writeWorksheet(wb,dia6,sheet = "Psykdiagnoser bmh (Mor)")
writeWorksheet(wb,lak6,sheet = "Läkemedel bmh (Mor)")

#Creates lak6_all and dia6_all which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_011_diagnoses regardless of event.R", encoding="utf-8")
writeWorksheet(wb,dia6_all,sheet = "Psykdiagnoser (Mor)")
writeWorksheet(wb,lak6_all,sheet = "Läkemedel (Mor)")

#Creates dia6_barn which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_012_barndiagnoser.R", encoding="utf-8")
writeWorksheet(wb,dia6_all,sheet = "Barndiagnoser")

#Creates mfr6 which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_013_perinatala tillstånd.R", encoding="utf-8")
writeWorksheet(wb,mfr6,sheet = "Perinatala tillstånd")

#Creates mfr_new6 which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_014_extra_mfr.R", encoding="utf-8")
writeWorksheet(wb,mfr_new6,sheet = "MFR Morbarn")

#Creates mfr_new6 which should be printed
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/TLP/TLP_015_suicidMord.R", encoding="utf-8")
writeWorksheet(wb,fDod6,sheet = "DOD")

saveWorkbook(wb)
#-------------------------------------------------------------------------------
#EOF