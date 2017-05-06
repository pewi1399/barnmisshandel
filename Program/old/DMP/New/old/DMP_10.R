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