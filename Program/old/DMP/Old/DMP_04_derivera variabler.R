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
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/functions/func.R", encoding="utf-8")

#grunddata
load(file.path(path,"Output","DMP","DMP_02_grunddata.Rdata"))
#load(file.path(path,"Output","DMP","Databas 2015","sos_barn.Rdata"))

#prt5 från DMP02 är grunddata
grund = prt5

#-------------------------------------------------------------------------------
names(grund)

#these are due to different spellings in infiles otherwise stack would be appropriate
table(!is.na(grund$HDIA) & !is.na(grund$hdia))

table(is.na(grund$HDIA))

cols = c("HDIA","hdia",names(grund)[grep("bdia",names(grund))])

grund$DIAGNOSER <- apply(grund[ , cols ] , 1 , paste , collapse = " ")
grund$DIAGNOSER = paste(" ",grund$DIAGNOSER," ", sep="") 



#beräkna födelsedag och diagnosdag
grund$fdag = as.Date(as.character(grund$fod_man_dag), "%Y%m%d")
grund$diagnosdag = as.Date(grund$INDATUM)

#grund1 = subset(grund1,is.na(INDATUM))
grund$diagnoseAgeWeeks = difftime(grund$diagnosdag,grund$fdag, units = "weeks")

grund$ageClassWeeks = ifelse(grund$diagnoseAgeWeeks<28,"<=28",
                         ifelse(grund$diagnoseAgeWeeks<=32,"29-32",
                                ifelse(grund$diagnoseAgeWeeks<=36,"33-36",
                                       ifelse(grund$diagnoseAgeWeeks>=37,"37+",NA))))
namesstart = names(grund)
#-------------------------------------------------------------------------------


#--------------------------- beräkna nya variabler -----------------------------

#diagnoser från tabell 1
source(file.path(path,"Program","DMP","DMP04_a_tabell1.R"), encoding="UTF-8")

#diagnoser från tabell 2
source(file.path(path,"Program","DMP","DMP04_b_tabell2.R"), encoding="UTF-8")

#diagnoser från tabell 3
source(file.path(path,"Program","DMP","DMP04_c_tabell3.R"), encoding="UTF-8")

save(grund,file=file.path(path,"Output","DMP","TMPfile.Rdata"))


#----------------------------- diagnoser från lak ------------------------------
source(file.path(path,"Program","DMP","DMP04_d_tabell4_läkemedel.R"), encoding="UTF-8")

#-------------------------------------------------------------------------------
