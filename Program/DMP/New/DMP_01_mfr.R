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

path =  "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)

#Source
source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveMFR.R", encoding="utf-8")

#read in mfr from grunddata and mfr from mfr_1
#mfr_grund0 = read.csv(file.path(path,"Indata","Grunddata","grunddata","MFR                             .CSV"), stringsAsFactors=F)
mfr_1_0 = read.csv(file.path(path,"Indata","mfr_1","MFR                             .CSV"), stringsAsFactors=F)
mfr_kompl = read.csv(file.path(path,"Indata","Komplettering","mor","MFR                             .CSV"),stringsAsFactors=F)
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

#-------------------------------------------------------------------------------

#Check how many ids
#table(mfr_grund0$lpnr_BARN %in% mfr_1_0$lpnr_BARN)

#There is one non unique id exclude it
mfr_1_1 = mfr_1_0[!duplicated(mfr_1_0$lpnr_BARN),]

#add new data
mfr_1_1 = rbind(mfr_1_1,mfr_kompl,mfr_kompl2a,mfr_kompl2b)

mfr0 = mfr_1_1
#calculate diagnosis vars

#Create diagnosis variable
childDiagnoses = c(names(mfr0)[grep("BDIA",names(mfr0))])

mfr0$BDIAG <- apply(mfr0[ ,childDiagnoses] , 1 , paste , collapse = " ")
mfr0$BDIAG = paste(" ",mfr0$BDIAG," ", sep="") 

#create mother diagnosis
motherDiagnoses = c(names(mfr0)[grep("MDIA",names(mfr0))])

mfr0$MDIAG <- apply(mfr0[ ,motherDiagnoses] , 1 , paste , collapse = " ")
mfr0$MDIAG = paste(" ",mfr0$MDIAG," ", sep="") 



#create flop
motherFlop = c(names(mfr0)[grep("FLOP",names(mfr0))])

mfr0$MFLOP <- apply(mfr0[ ,motherFlop] , 1 , paste , collapse = " ")
mfr0$MFLOP = paste(" ",mfr0$MFLOP," ", sep="") 


#at this point all that is needed is MDIAG BDIAG date and lpnr
outvars = c("lpnr_BARN","lpnr_mor","MFODDAT", "BFODDAT","BDIAG","MDIAG","MFLOP", "BUTDAT")
mfr1 = mfr0[,outvars]


#only child diagnoses are interesting at this point
#mfr1$MDIAG = NULL
mfr1$DIAGNOSER = mfr1$BDIAG 

#introduce tabulator diagnoses
mfr2 = deriveMFR(mfr1)
#-------------------------------------------------------------------------------
#save keyfile
#mfr2$vårdtidFödsel <- difft


keyfile = mfr2[,c("lpnr_BARN","lpnr_mor")]
save(keyfile, file=file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))

#--------------------------- prepare for print ---------------------------------
DMP_01_mfr = mfr2
save(DMP_01_mfr, file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))

#EOF