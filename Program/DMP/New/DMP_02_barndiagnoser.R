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
library(haven)
library(plyr)
library(BBmisc)
library(XLConnect)
library(magrittr)
library(dplyr)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)

#Source
source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveChildDiagnoses.R", encoding="utf-8")

# diagnoses
metadata <- readWorksheetFromFile("/home/per/Polycarp/KBH/P101_barnmisshandel/Indata/dataDictionary_161009.xlsx", sheet = "barnmissh") 

metadata$variable <- paste0("n_", metadata$variable)
# collapse all code variables
diags <- grep("kod", names(metadata), value = TRUE)

#insert leading spaces
metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))

# assemble search phrases
metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))

derivVars <- subset(metadata, derived == "ja")
metadata <- subset(metadata, derived == "nej")

# searches should start with an exact match but may end on any string 
metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)
#-------------------------------------------------------------------------------

#--------------------------------- barndata ------------------------------------
if(FALSE){
  #read in mfr from grunddata and mfr from mfr_1
  dag_barn0 = read.csv(file.path(path,"Indata","SOS_BARN","SOS_BARN","PATIENT_DAG_KIRU                .CSV"), stringsAsFactors=F)
  oppen_barn0 = read.csv(file.path(path,"Indata","SOS_BARN","SOS_BARN","PATIENT_OPPEN                   .CSV"), stringsAsFactors=F)
  sluten_barn0 = read.csv(file.path(path,"Indata","SOS_BARN","SOS_BARN","PATIENT_SLUTEN                  .CSV"), stringsAsFactors=F)
  
  
  #dag_barnG = read_sas(file.path(path,"Indata","Grunddata","grunddata","patient_dag_kiru.sas7bdat"))
  dag_barnG = read.csv(file.path(path,"Indata","Grunddata","grunddata","PATIENT_DAG_KIRU                .CSV"), stringsAsFactors=F, fileEncoding = "ISO-8859-1")
  oppen_barnG = read.csv(file.path(path,"Indata","Grunddata","grunddata","PATIENT_OPPEN                   .CSV"), stringsAsFactors=F)
  sluten_barnG = read.csv(file.path(path,"Indata","Grunddata","grunddata","PATIENT_SLUTEN                  .CSV"), stringsAsFactors=F)
  
  dag_barn0 = rbind.fill(dag_barn0,dag_barnG)
  oppen_barn0 = rbind.fill(oppen_barn0,oppen_barnG)
  sluten_barn0 = rbind.fill(sluten_barn0,sluten_barnG)
  
  #--------------------------- add source 2016-02-05 -----------------------------
  dag_barn0$SOURCE <- "dagKir"
  oppen_barn0$SOURCE <- "oppen"
  sluten_barn0$SOURCE <- "sluten"
  #-------------------------------------------------------------------------------
  
  #read in data from komplettering august 2015
  #sluten_barn00 = read_sas(file.path(path,"Indata","Komplettering","BARN","patient_sluten.sas7bdat"))
  sluten_barn00 = read.csv(file.path(path,"Indata","Komplettering","BARN","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F, fileEncoding = "ISO-8859-1")
  oppen_barn00 = read.csv(file.path(path,"Indata","Komplettering","BARN","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)
  
  sluten_barnGG = read.csv2(file.path(path,"Indata","Komplettering","BARN","GRUND DATA FÖR BARN","patient_sluten.csv"),stringsAsFactors=F)
  
  sluten_barn00 = rbind.fill(sluten_barn00,sluten_barnGG)
  
  #--------------------------- add source 2016-02-05 -----------------------------
  oppen_barn00$SOURCE <- "oppen"
  sluten_barn00$SOURCE <- "sluten"
  #-------------------------------------------------------------------------------
  
  oppen_barn0 = rbind.fill(oppen_barn0,oppen_barn00)
  sluten_barn0 = rbind.fill(sluten_barn0,sluten_barn00)
  
  #read in data from komplettering nr 2 october 2015
  sluten_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
  oppen_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)
  
  sluten_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
  oppen_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)
  
  #--------------------------- add source 2016-02-05 -----------------------------
  oppen_barnOct1$SOURCE <- "oppen"
  sluten_barnOct1$SOURCE <- "sluten"
  
  oppen_barnOct2$SOURCE <- "oppen"
  sluten_barnOct2$SOURCE <- "sluten"
  #-------------------------------------------------------------------------------
  
  
  komplettOct = rbind.fill(sluten_barnOct1,oppen_barnOct1,sluten_barnOct2,oppen_barnOct2)
  
  #split kids and mothers by mfr data
  mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
  mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october
  
  komplettOct = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_BARN,mfr_kompl2a$lpnr_BARN))
  sluten_barn0  = rbind.fill(sluten_barn0, komplettOct)
  
  #Loaded from indata for fathers since some kids was not included in the first delivery of data
  #added early june 2015
  load(file=file.path(path, "Output","DMP","DMP_03_tillagg_barn.Rdata"))
  par_barn_new = DMP_03_tillagg_barn
  
  #important vars 
  impvars = c("lpnr","INDATUM")
  
  dag_barn1 = dag_barn0[,c(impvars,names(dag_barn0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik|ALDER|SOURCE",names(dag_barn0))])]
  oppen_barn1 = oppen_barn0[,c(impvars,names(oppen_barn0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik|ALDER|SOURCE",names(oppen_barn0))])]
  sluten_barn1 = sluten_barn0[,c(impvars,names(sluten_barn0)[grepl("bdia|hdia|EKO|MDC|INDATUM|UTDATUM|sjukhusnamn|lan_text|klinik|ALDER|SOURCE",names(sluten_barn0))])]
  
  
  par_barn = rbind.fill(dag_barn1,oppen_barn1,sluten_barn1)
  #par_barn <- par_barn[0:1000,]
  
  cols = c(names(par_barn)[grep("bdia|hdia",names(par_barn))])
  
  par_barn$DIAGNOSER <- apply(par_barn[ , cols ] , 1 , paste , collapse = " ")
  par_barn$DIAGNOSER = paste(" ",par_barn$DIAGNOSER," ", sep="") 
  
  #new eko
  cols = c(names(par_barn)[grep("EKO",names(par_barn))])
  
  par_barn$EKO <- apply(par_barn[ , cols ] , 1 , paste , collapse = " ")
  par_barn$EKO = paste(" ",par_barn$EKO," ", sep="") 
  
  #new Special var for derivation of UtrObsMissh 2015-11-03
  cols = c(names(par_barn)[grep("EKO|MDC|bdia|hdia",names(par_barn))])
  
  par_barn$SPEC <- apply(par_barn[ , cols ] , 1 , paste , collapse = " ")
  par_barn$SPEC = paste(" ",par_barn$SPEC," ", sep="") 
  
  #merge EKO and diagnosis
  par_barn$DIAGNOSER = paste(par_barn$DIAGNOSER,par_barn$EKO,sep="")
  
  par_barn = rbind.fill(par_barn,par_barn_new)
  
  #something is strange with this one 
  par_barn$INDATUM = gsub(" ","",as.character(par_barn$INDATUM))
  
  par_barn$INDATUM = ifelse(par_barn$INDATUM=="",NA,par_barn$INDATUM)
  #-------------------------------------------------------------------------------
  par_barn = subset(par_barn,par_barn$ALDER<1) #viktig rad. Kanske för hård
  
  #subset on vars and date
  par_barn1 = par_barn[!is.na(par_barn$INDATUM),c("lpnr","INDATUM","DIAGNOSER","EKO",
                                                  "MDC","INDATUM","UTDATUM","sjukhusnamn","lan_text","klinik","SPEC","SOURCE")]
  
  rm(oppen_barn0);rm(oppen_barn1);rm(oppen_barnG);rm(par_barn);rm(sluten_barn0)
  rm(sluten_barn1)
  gc()
    
  
  
  #--------------------------- tilläg oktober 2016--------------------------------
  
  load(file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
  DMP_01_mfr <- DMP_01_mfr[,c("lpnr_BARN", "BDIAG", "BFODDAT")]
  names(DMP_01_mfr) <- c("lpnr","DIAGNOSER", "INDATUM")
  DMP_01_mfr$INDATUM <- as.character(as.Date(as.character(DMP_01_mfr$INDATUM), "%Y%m%d"))
  
  par_barn1 <- bind_rows(par_barn1,DMP_01_mfr)
  rm(DMP_01_mfr);gc()
  
  load(file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
  
  #new eko
  cols = c(names(DMP_06_dodBarn)[grep("ORSAK",names(DMP_06_dodBarn))])
  
  DMP_06_dodBarn <- as.data.frame(DMP_06_dodBarn)
  
  DMP_06_dodBarn$DIAGNOSER <- apply(DMP_06_dodBarn[ , cols ] , 1 , paste , collapse = " ")
  DMP_06_dodBarn$DIAGNOSER = paste(" ",DMP_06_dodBarn$DIAGNOSER," ", sep="") 
  
  DMP_06_dodBarn <- DMP_06_dodBarn[,c("lpnr","DIAGNOSER", "DODSDATN")]
  DMP_06_dodBarn$INDATUM <- DMP_06_dodBarn$DODSDATN
  DMP_06_dodBarn$DODSDATN <- NULL
  
  par_barn1 <- bind_rows(par_barn1, DMP_06_dodBarn)
  rm(DMP_06_dodBarn);gc()
  
  saveRDS(par_barn1, "Output/par_barn1")
}else{
  par_barn1 <- readRDS("Output/par_barn1")
}

#-------------------------------------------------------------------------------

#create diagnoses
par_barn2 <- par_barn1;rm(par_barn1)
# apply search phrase on these
applySearch <- function(variable, phrase){
  ## variable: variable containing ICD diag
  ## phrase: a string containing string or regex to be used for matching
  out<- ifelse(grepl(phrase, variable), 1,0)  
  return(out)
}




# create diagnosis variables for student project
setDT(par_barn2)
par_barn2[,(metadata$variable):=lapply(metadata$search, applySearch, variable = par_barn2$DIAGNOSER),]
# derive new diagnoses

par_barn2 <- data.frame(par_barn2)

# calculate derived vars
for( i in 1:nrow(derivVars)){
  # row: dataframe with code definitions
  row <- derivVars[i,]
  variable <- as.character(row$variable)
  
  vars <- gsub(" ", "", unlist(strsplit(gsub("\\|","",as.character(row$search)), " ")))
  vars <- subset(vars, vars!="" & vars != "NA")
  
  par_barn2[, variable] <- ifelse(rowSums(par_barn2[,paste0("n_",vars)])>0,1,0)
}
#-------------------------------------------------------------------------------

#---------- extra oct 2015 add time dependency for some diagnosis vars ---------
vars = c(metadata$variable, derivVars$variable)
vars <- gsub("\\.", "_", vars)
vars <- gsub(" ", "_", vars)
vars <- gsub("-", "_", vars)

kalenderVars = paste("alderDiagnosManad_",vars,sep="")

load(file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
mfr = DMP_01_mfr[,c("lpnr_BARN","BFODDAT","BUTDAT")]
names(mfr) = c("lpnr","BFODDAT","BUTDAT")

par_barn2 = merge(par_barn2,mfr,by="lpnr",all.x=TRUE)

par_barn2$INDATUM = as.Date(par_barn2$INDATUM,format="%Y-%m-%d")
par_barn2$BFODDAT = as.Date(as.character(par_barn2$BFODDAT),format="%Y%m%d")
par_barn2$BUTDAT = as.Date(as.character(par_barn2$BUTDAT),format="%Y%m%d")

#fix vars
par_barn2$INDATUM.1 = NULL
par_barn2$kalenderAr = difftime(par_barn2$INDATUM,par_barn2$BFODDAT,units ="days")
par_barn2$kalenderAr = as.numeric(par_barn2$kalenderAr/(365.24/12))

#Ålder vid utskrivning
par_barn2$kalenderUtskrivning = difftime(par_barn2$BUTDAT,par_barn2$BFODDAT,units ="days")
par_barn2$kalenderUtskrivning = as.numeric(par_barn2$kalenderUtskrivning/(365.24/12))

par_barn2 = data.table(par_barn2)

names(par_barn2) <- gsub("\\.", "_", names(par_barn2))
names(par_barn2) <- gsub(" ", "_", names(par_barn2))
names(par_barn2) <- gsub("-", "_", names(par_barn2))

# if diagnosis exists (x==1) find date (or rather age) for first occurence.
datefinder = function(x,dte){ifelse(is.null(which.first(x == 1)),NA,dte[which.first(x == 1)])}

setkey(par_barn2,lpnr,kalenderAr)
par_barn2[,(kalenderVars):=lapply(vars, function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]

# Koda om vars, 1,2 för att avgöra om diagnos inträffat innan utskrivning
diagnosUtskrivning <- paste("diagnosInnanUtskrivning", kalenderVars,sep="")
diagnosUtskrivning <- gsub("alderDiagnosManad", "", diagnosUtskrivning)
par_barn2[,(diagnosUtskrivning):=lapply(kalenderVars, function(x){ifelse(get(x)>get("kalenderUtskrivning"),1,2)}),]

par_barn2$kalenderAr = NULL

# first maltreatment syndrome
table(par_barn2$alderDiagnosManad_n_maltreatmentSyndrome)

#par_barn2[,minMaltreatment:=min(alderDiagnosManad_n_maltreatmentSyndrome, na.rm =TRUE), by = lpnr]
#par_barn2$minMaltreatment <- ifelse(par_barn2$minMaltreatment==Inf, NA, par_barn2$minMaltreatment)
par_barn2$lan_text  = gsub("\xe4","ä",par_barn2$lan_text)
par_barn2$lan_text<- gsub("\xf6","ö",par_barn2$lan_text)
par_barn2$lan_text<- gsub("\U3e35653c","å",par_barn2$lan_text)
par_barn2$lan_text<- gsub("\xd6","Ö",par_barn2$lan_text)

times <- 
par_barn2 %>% 
  select(lpnr,INDATUM, lan_text, n_maltreatmentSyndrome) %>% 
  filter(n_maltreatmentSyndrome == 1) %>% 
  data.table

# Sort on time
setkey(times, lpnr, INDATUM)

times <- times[!duplicated(times$lpnr)]

times <- 
times %>% 
  select(lpnr, lan_text) %>% 
  mutate(lanMaltreatment = lan_text,
         lan_text = NULL) %>% 
  data.table

setkey(times, lpnr)
setkey(par_barn2, lpnr)

par_barn2 <- times[par_barn2]


#special time dependencies
par_barn2$n_NeoSDH7 <- ifelse(par_barn2$alderDiagnosManad_n_NeoSDH7*(365.24/12)<7,par_barn2$n_NeoSDH7, NA)
par_barn2$n_NEOSDH28 <- ifelse(par_barn2$alderDiagnosManad_n_NEOSDH28*(365.24/12)<28,par_barn2$n_NEOSDH28, NA)
par_barn2$n_SDH_T_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_SDH_T_1_11 >1 & par_barn2$alderDiagnosManad_n_SDH_T_1_11 <12, par_barn2$n_SDH_T_1_11,NA)
par_barn2$n_SDH_IT_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_SDH_IT_1_11 >1 & par_barn2$alderDiagnosManad_n_SDH_IT_1_11 <12, par_barn2$n_SDH_IT_1_11,NA)
par_barn2$n_Alla_SDH_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_Alla_SDH_1_11 >1 & par_barn2$alderDiagnosManad_n_Alla_SDH_1_11 <12, par_barn2$n_Alla_SDH_1_11,NA)

#-------------------------------------------------------------------------------
#out1 <- grep("diagnosInnanUtskrivning", names(par_barn2), value=T)
#--------------------------- prepare for print ---------------------------------
DMP_02_barndiagnoser = data.frame(par_barn2)
#write.csv2(DMP_02_barndiagnoser[,c(names(DMP_02_barndiagnoser)[1:7],"barnMisshandel_d1")], file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.csv"),na="",row.names=F)
save(DMP_02_barndiagnoser, file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.Rdata"))
#EOF