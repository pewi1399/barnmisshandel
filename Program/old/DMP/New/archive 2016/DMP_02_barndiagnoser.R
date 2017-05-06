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

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)

#Source
source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveChildDiagnoses.R", encoding="utf-8")

# diagnoses
metadata <- readWorksheetFromFile("Indata/dataDictionary.xlsx", sheet = "barnmissh") 
derivVars <- subset(metadata, derived == "ja")
metadata <- subset(metadata, derived == "nej")
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

#--------------------------------- barndata ------------------------------------

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

#Tests 2015-08-25
#par_barn$INYEAR = substr(par_barn$INDATUM,1,4)
#by(par_barn$EKO,par_barn$INYEAR, function(x){ table(x=="      ")})
#table(par_barn$INYEAR)
#df2013 = subset(par_barn,par_barn$INYEAR==2013)

#------------------- kontrollutskrift till ulf 2015-06-29 ----------------------

# DT = data.table(par_barn)
# 
# DT1 = DT[,list(minimum_age = min(ALDER)),
#          by="lpnr"]

#write.csv2(DT1, file=file.path(path, "Output","DMP","DMP_02_åldrar.csv"),row.names=F)

#-------------------------------------------------------------------------------
par_barn = subset(par_barn,par_barn$ALDER<1) #viktig rad. Kanske för hård

#subset on vars and date
par_barn1 = par_barn[!is.na(par_barn$INDATUM),c("lpnr","INDATUM","DIAGNOSER","EKO",
                                                "MDC","INDATUM","UTDATUM","sjukhusnamn","lan_text","klinik","SPEC","SOURCE")]



rm(oppen_barn0);rm(oppen_barn1);rm(oppen_barnG);rm(par_barn);rm(sluten_barn0)
rm(sluten_barn1)
gc()

#create diagnoses
par_barn2 = deriveChildDiagnoses(par_barn1)

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
par_barn2$s_rakit_d1 <- ifelse(rowSums(par_barn2[,c("s_aktivRakit", "s_osteomalaci", "s_kalcium_d1", "s_george_d1"), with =FALSE], na.rm=TRUE)>0,1,0)
par_barn2$s_rakit_d2 <- ifelse(rowSums(par_barn2[,c("s_aktivRakit", "s_osteomalaci"), with =FALSE], na.rm=TRUE)>0,1,0)
par_barn2 <- data.frame(par_barn2)
#-------------------------------------------------------------------------------

#---------- extra oct 2015 add time dependency for some diagnosis vars ---------
vars = c("rakit_d1","frakturRevben_d1","frakturLårben_d1",
         "frakturUnderben_d1","övrigSkallskada_d1","frakturÖverUnderArm_d1",
         "skallFraktur_d1", "retinalBlödning_d1", "subduralIT_d1",
         "subduralTraum_d1","svt_d1", "ogi_d1", "spädbarnskolik_d1",
         "frakturRörben_d2","subduralBlödning_d2","frakturLändkotpelare","nyckelbenfraktur",
         "ickeSpecSymtomNervMuskoskelSys","blåttÖga","ytligSkadaHuvudBröst",
         "kontusionRyggBäckenBukväggLårUnderben","brännskada","ALTE","kramper",
         "feberkramp", "krampanfallUNS", "epilepsi", "hjärnskakning",
         "kräkningar", "kraniosunostos", "anoxiskHjärnskada", "skadorPåHuvudetUtanFraktur",
         "andningsuppehåll","ytligSårskada","fallskadaKlämskada","summationVariabler_d1")


#vars %in% names(par_barn2) 

kalenderVars = paste("alderDiagnosManad_",vars,sep="")

load(file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
mfr = DMP_01_mfr[,c("lpnr_BARN","BFODDAT","BUTDAT")]
names(mfr) = c("lpnr","BFODDAT","BUTDAT")

par_barn2 = merge(par_barn2,mfr,by="lpnr",all.x=T)

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

# if diagnosis exists (x==1) find date (or rather age) for first occurence.
datefinder = function(x,dte){ifelse(is.null(which.first(x == 1)),NA,dte[which.first(x == 1)])}
#dat0[,alteFirst3:=datefinder(get(x),get("date")),by="id"]
#dat0
setkey(par_barn2,lpnr,kalenderAr)
par_barn2[,(kalenderVars[1:26]):=lapply(vars[1:26], function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]
par_barn2[,(kalenderVars[27:36]):=lapply(vars[27:36], function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]

vars2 <- c("frakturRevben_d1", "frakturLårben_d1",
  "frakturUnderben_d1", "övrigSkallskada_d1", "UtrObsMissh_d1", "DiagnosMissh_d1",
  "barnmisshandelssyndromUNS", "syndromBarnMisshdel", "frakturÖverUnderArm_d1",
  "skallFraktur_d1", "retinalBlödning_d1", "subduralIT_d1", "subduralTraum_d1", 
  "svt_d1", "ogi_d1", "spädbarnskolik_d1", "frakturRörben_d2", "subduralBlödning_d2",
  "ALTE", "kramper", "feberkramp", "krampanfallUNS", "epilepsi", "hjärnskakning",
  "nyckelbenfraktur", "maltreatmentSyndrome", "assault", "undeterminedCauses",
  "adverseSocialCircumstances", "dottBarn", "maltreatmentRelatedInjury",
  "markerintracranical", "markerlongbone", "frakturHumerusskaftet", "frakturFemurskaftet")

vars2 = vars2[!(vars2 %in% vars)]

kalenderVars2 = paste("alderDiagnosManad_",vars2,sep="")

par_barn2[,(kalenderVars2):=lapply(vars2, function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]

table(par_barn2$alderDiagnosManad_ALTE<=1)
table(par_barn2$alderDiagnosManad_ogi_d1<=1)

vars3 <- c("barnMisshandel_d1", "våldtäkt", "glaskroppsblödning", "subkonjunktivalBlödning", 
           "infektion", "neutropeni", "anemi", "blöjderytemDermatit", "missbildningar", 
           "obstipation", "gulsot", "failureToThrive", "försenatKroppsligtUtvecklingsstadium", 
           "kontaktHetDryckFöda", "misshandelUtövadFörälder", "misshandelUtövadIckeSpecPerson", 
           "mordDråpförsök", "olycka", "andraSkadorOklarAvsikt", "psykiskStörningRubbningIFamilj",
"s_aktivRakit",                    
"s_osteomalaci",                   
"s_kalcium_d1",                    
"s_george_d1",                     
"s_hie",                           
"s_cpSkada",                       
"s_anoxiskHjarnskad",              
"s_krampOspec",                    
"s_krampNyfodd",                   
"s_Epdiag",                        
"s_enbartFeberKramp",              
"s_IVH",                           
"s_kramperNeo",                    
"s_kardiovaskMissb",               
"s_hjartsvikt",                    
"s_njursjukdom",                   
"s_kroniskNjursvikt",              
"s_anemi",                         
"s_anemiUnderburenJarnbrist",      
"s_anemiOspec",                    
"s_sepsis",                        
"s_uppfodningSvarighet",           
"s_bristMineralamnen",             
"s_rubbningarKalcium",             
"s_annanNaringsbristSpec",         
"s_proteinEnergibrist",            
"s_undernutritionNyfodd",          
"s_annanUteblivenNormalUtveckl",   
"s_failureToThrive",               
"s_rubbningKalciumMagnesiumNyfodd",
"s_neonatalHypokalcHypomagnesemi", 
"s_annanHypokalcemi",              
"s_gulsotForlForeBeraknad",        
"s_gulsotNyfoddOspec",             
"s_endokrinRubbningHypokalc",      
"s_bristDVitamin",                 
"s_rakit_d1",                      
"s_rakit_d2"
)



kalenderVars3 = paste("alderDiagnosManad_",vars3,sep="")

par_barn2[,(kalenderVars3):=lapply(vars3, function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]


# Koda om vars, 1,2 för att avgöra om diagnos inträffat innan utskrivning
diagnosUtskrivning <- paste("diagnosInnanUtskrivning", c(kalenderVars, kalenderVars2, kalenderVars3),sep="")
diagnosUtskrivning <- gsub("alderDiagnosManad", "", diagnosUtskrivning)
par_barn2[,(diagnosUtskrivning):=lapply(c(kalenderVars, kalenderVars2, kalenderVars3), 
                                                    function(x){ifelse(get(x)>get("kalenderUtskrivning"),1,2)}),]

table(par_barn2$alderDiagnosManad_ALTE)
table(par_barn2$alderDiagnosManad_ogi_d1)

par_barn2$kalenderAr = NULL

#-------------------------------------------------------------------------------
#out1 <- grep("diagnosInnanUtskrivning", names(par_barn2), value=T)
#--------------------------- prepare for print ---------------------------------
DMP_02_barndiagnoser = data.frame(par_barn2)
write.csv2(DMP_02_barndiagnoser[,c(names(DMP_02_barndiagnoser)[1:7],"barnMisshandel_d1")], file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.csv"),na="",row.names=F)
save(DMP_02_barndiagnoser, file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.Rdata"))
#EOF