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
library(XLConnect)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)

#Source
source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveParentDiagnoses.R", encoding="utf-8")

# diagnoses
metadata <- readWorksheetFromFile("Indata/dataDictionary.xlsx", sheet = 1) 
metadata <- subset(metadata, Group == "studentProjektRakitMor")
metadata$variable <- paste0("s_", metadata$variable)
# collapse all code variables
diags <- grep("kod", names(metadata), value = TRUE)

#insert leading spaces
metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))

# assemble search phrases
metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))

# searches should start with an exact match but may end on any string 
metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#------------------------------- föräldradata ----------------------------------

#read in mfr from grunddata and mfr from mfr_1
dag_mor0 = read.csv(file.path(path,"Indata","mor","sos_mor","PATIENT_DAG_KIRU                .CSV"), stringsAsFactors=F)
oppen_mor0 = read.csv(file.path(path,"Indata","mor","sos_mor","PATIENT_OPPEN                   .CSV"), stringsAsFactors=F)
sluten_mor0 = read.csv(file.path(path,"Indata","mor","sos_mor","PATIENT_SLUTEN                  .CSV"), stringsAsFactors=F)

#read in data from komplettering august 2015
sluten_mor00 = read.csv(file.path(path,"Indata","Komplettering","mor","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
oppen_mor00 = read.csv(file.path(path,"Indata","Komplettering","mor","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)

#read in data from komplettering nr 2 october 2015
sluten_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
oppen_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)

sluten_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
oppen_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)

komplettOct = rbind.fill(sluten_barnOct1,oppen_barnOct1,sluten_barnOct2,oppen_barnOct2)

#split kids and mothers by mfr data
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_mor,mfr_kompl2a$lpnr_mor))

#Combine
oppen_mor0 = rbind.fill(oppen_mor0,oppen_mor00,komplettOct)
sluten_mor0 = rbind(sluten_mor0,sluten_mor00)

#important vars 
impvars = c("lpnr","INDATUM")

dag_mor1 = dag_mor0[,c(impvars,names(dag_mor0)[grepl("bdia|hdia|EKO",names(dag_mor0))])]
oppen_mor1 = oppen_mor0[,c(impvars,names(oppen_mor0)[grepl("bdia|hdia|EKO",names(oppen_mor0))])]
sluten_mor1 = sluten_mor0[,c(impvars,names(sluten_mor0)[grepl("bdia|hdia|EKO",names(sluten_mor0))])]


par_mor = rbind.fill(dag_mor1,oppen_mor1,sluten_mor1)


cols = c(names(par_mor)[grep("bdia|hdia",names(par_mor))])

par_mor$DIAGNOSER <- apply(par_mor[ , cols ] , 1 , paste , collapse = " ")
par_mor$DIAGNOSER = paste(" ",par_mor$DIAGNOSER," ", sep="") 


#new eko
cols = c(names(par_mor)[grep("EKO",names(par_mor))])

par_mor$EKO <- apply(par_mor[ , cols ] , 1 , paste , collapse = " ")
par_mor$EKO = paste(" ",par_mor$EKO," ", sep="") 

#something is strange with this one 
par_mor$INDATUM = gsub(" ","",as.character(par_mor$INDATUM))

par_mor[par_mor$INDATUM=="","INDATUM"] = NA

par_mor1 = par_mor[!is.na(par_mor$INDATUM),c("lpnr","INDATUM","DIAGNOSER","EKO")]

#create diagnoses
par_mor2 = deriveParentDiagnoses(par_mor1)

par_mor2$övergreppMisshMordDråpVård = par_mor2$övergreppMisshMordDråp



# apply search phrase on these
applySearch <- function(variable, phrase){
  ## variable: variable containing ICD diag
  ## phrase: a string containing string or regex to be used for matching
  out<- ifelse(grepl(phrase, variable), 1,0)  
  return(out)
}

# create diagnosis variables for stuudent project
setDT(par_mor2)
par_mor2[,(metadata$variable):=lapply(metadata$search, applySearch, variable = par_mor2$DIAGNOSER),]
# derive new diagnoses
par_mor2 <- data.frame(par_mor2)

#outs = c("självmordsförsökICD10", "självmordsförsök", "suicidICD10", "suicid",
#"övergreppMisshMordDråpICD10", "övergreppMisshMordDråp","mordDråpICD10","mordDråp")

#par_mor3 = par_mor2[,!c(names(par_mor2) %in% outs)]
#table(par_mor3$övergreppMisshMordDråpVård)
#-------------------------------------------------------------------------------

#--------------------------- prepare for print ---------------------------------
DMP_03_föräldradiagnoser_mor = par_mor2

write.csv2(DMP_03_föräldradiagnoser_mor[,c("lpnr","INDATUM","DIAGNOSER","EKO","självmordsförsök")], file=file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.csv"),na="",row.names=F)
DMP_03_föräldradiagnoser_mor$EKO = NULL 

save(DMP_03_föräldradiagnoser_mor, file=file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_mor.Rdata"))
#EOF