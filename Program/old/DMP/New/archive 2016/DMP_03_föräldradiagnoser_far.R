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
#library(rtf)
library(plyr)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)

#Source
source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveParentDiagnoses.R", encoding="utf-8")
load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))
#-------------------------------------------------------------------------------

#------------------------------- föräldradata ----------------------------------

#read in mfr from grunddata and mfr from mfr_1
dag_far0 = read.csv2(file.path(path,"Indata","far","SOS","PATIENTREG","patient_dag_kiru.csv"), stringsAsFactors=F, fileEncoding = "ISO-8859-1")
oppen_far0 = read.csv2(file.path(path,"Indata","far","SOS","PATIENTREG","patient_oppen.csv"), stringsAsFactors=F, fileEncoding = "ISO-8859-1")
sluten_far0 = read.csv2(file.path(path,"Indata","far","SOS","PATIENTREG","patient_sluten.csv"), stringsAsFactors=F, fileEncoding = "ISO-8859-1")

#secondary keyfile
keyfile2 = read.csv2(file.path(path,"Indata","far","SOS","info_lpnr.csv"))
names(keyfile2) = c("lpnr_BARN","lpnr_far","foddag","fod")
keyfile2$foddag = NULL
keyfile2$fod = NULL

key = merge(keyfile, keyfile2, by = "lpnr_BARN", all=T)

#important vars 
impvars = c("lpnr","INDATUM")

dag_far1 = dag_far0[,c(impvars,names(dag_far0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik",names(dag_far0))])]
oppen_far1 = oppen_far0[,c(impvars,names(oppen_far0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik",names(oppen_far0))])]
sluten_far1 = sluten_far0[,c(impvars,names(sluten_far0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik",names(sluten_far0))])]

#--------------------------- add source 2016-02-05 -----------------------------
dag_far1$SOURCE <- "dagKir"
oppen_far1$SOURCE <- "oppen"
sluten_far1$SOURCE <- "sluten"
#-------------------------------------------------------------------------------

par_far = rbind.fill(dag_far1,oppen_far1,sluten_far1)

cols = c(names(par_far)[grep("bdia|hdia",names(par_far))])

par_far$DIAGNOSER <- apply(par_far[ , cols ] , 1 , paste , collapse = " ")
par_far$DIAGNOSER = paste(" ",par_far$DIAGNOSER," ", sep="") 

#new eko
cols = c(names(par_far)[grep("EKO",names(par_far))])

par_far$EKO <- apply(par_far[ , cols ] , 1 , paste , collapse = " ")
par_far$EKO = paste(" ",par_far$EKO," ", sep="") 

#something is strange with this one 
par_far$INDATUM = gsub(" ","",as.character(par_far$INDATUM))

par_far[par_far$INDATUM=="","INDATUM"] = NA

par_far1 = par_far[!is.na(par_far$INDATUM),c("lpnr","INDATUM","DIAGNOSER","EKO",
                                             "MDC","INDATUM", "UTDATUM","sjukhusnamn",
                                             "lan_text","klinik", "SOURCE")]

#extract kids from parent file
par_far2 = subset(par_far1,!(par_far1$lpnr %in% key$lpnr_BARN))
par_barn = subset(par_far1,par_far1$lpnr %in% key$lpnr_BARN)
#nrow(par_far1)
#nrow(par_far2)+ nrow(par_barn)

par_far2 = par_far2[,c("lpnr","INDATUM","DIAGNOSER","EKO")]
#create diagnoses
par_far3 = deriveParentDiagnoses(par_far2)



par_far3$övergreppMisshMordDråpVård = par_far3$övergreppMisshMordDråp

#outs = c("självmordsförsökICD10", "självmordsförsök", "suicidICD10", "suicid",
#"övergreppMisshMordDråpICD10", "övergreppMisshMordDråp","mordDråpICD10","mordDråp")

#par_far4 = par_far3[,!c(names(par_far3) %in% outs)]
#table(par_mor3$övergreppMisshMordDråpVård)
#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_03_tillagg_barn = par_barn
DMP_03_föräldradiagnoser_far = par_far3

save(key, file=file.path(path, "Output","DMP","DMP_03_keyfile2.Rdata"))
save(DMP_03_tillagg_barn, file=file.path(path, "Output","DMP","DMP_03_tillagg_barn.Rdata")) #added because kids and fathers was mixed in this file


write.csv2(DMP_03_föräldradiagnoser_far[,c("lpnr","INDATUM","DIAGNOSER","EKO","självmordsförsök")], file=file.path(path, "Output","DMP","DMP_03_foraldradiagnoser_far.csv"),na="",row.names=F)
DMP_03_föräldradiagnoser_far$EKO = NULL 
save(DMP_03_föräldradiagnoser_far, file=file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))
#EOF