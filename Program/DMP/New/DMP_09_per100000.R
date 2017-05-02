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
library(haven)
library(scales)
library(reshape2)
library(readxl)
library(XLConnect)
library(plyr)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)
#-------------------------------------------------------------------------------
load(file.path(path,"Output","DMP","DMP_100_barndiagnoser.Rdata"))
nollAringar <- read_excel(file.path(path, "Indata", "0aringar.xlsx"))

names(nollAringar) <- c("year", "gend1", "gend2", "n")

noll0 <- nollAringar[nollAringar$year %in% 1987:2014,c("year", "n")]

#Just for testing purposes
#dia5 <- dia5[1:1000,]
dia5 <- dia5[,!grepl("lpnr|iagnos", names(dia5)), with=FALSE]

fracs <- function(x, N, unit = 100000){
  
  #n <- sum(!is.na(x))
  n <- unique(N)
  sigmaX <- sum(x, na.rm=TRUE)
  #nPerFrac <-sigmaX/n*unit
  
  return(sigmaX)
}

dia5$year <- as.numeric(substr(as.character(dia5$BFODDAT), 1, 4))
dia5$BFODDAT <- NULL
#dia5$year <- NULL

dia5 <- merge(dia5, noll0, by = "year", all.x=T)

out <- dia5[,as.list(sapply(.SD, fracs, N = n)), by = "year"]
#out$n <- NULL

setcolorder(out, c("year", grep("year", names(out), value=TRUE, invert = TRUE)))
setkey(out, year)
#-------------------------------------------------------------------------------
#writeWorksheetToFile(out, file = file.path(path,"Output","DMP","DMP_08_per100000.xlsx"), sheet= "summaries")
save(out, file = file.path(path,"Output","DMP","DMP_09_per100000.rdata"))
save(noll0, file = file.path(path,"Output","DMP","DMP_09_nollar.rdata"))
     