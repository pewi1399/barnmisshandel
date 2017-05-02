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
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/functions/deriveChildDiagnoses.R", encoding="utf-8")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
snq0 = read.csv2(file.path(path,"Indata","SNQ","din_fil_SNQ.csv"))

snq1 = subset(snq0,!is.na(snq0$lpnr_barn))
snq2 = snq1[!duplicated(snq1$lpnr_barn),] #some dupes

snq2$DIAGNOSER  = paste(" ",gsub(";", " ",as.character(snq2$Alla.diagnoser)), " ", sep="") 
snq2$EKO = snq2$DIAGNOSER

snq3 = deriveChildDiagnoses(snq2)

snq3$DIAGNOSER = NULL
snq3$EKO = NULL

snq4 = snq3[,!grepl("ICD",names(snq3))]

snq4$lpnr_mor = NULL

names(snq4) = paste(names(snq4),"SNQ", sep="_")

snq4$lpnr = snq4$lpnr_barn_SNQ
snq4$lpnr_barn_SNQ = NULL

#--------------------------- prepare for print ---------------------------------
DMP_08_snq = snq4

save(DMP_08_snq, file=file.path(path, "Output","DMP","DMP_08_snq.Rdata"))
#EOF