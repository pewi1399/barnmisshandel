rm(list=ls())

library(data.table)
library(dplyr)

#Source
# source("Program/DMP/New/functions/deriveChildDiagnoses.R", encoding="utf-8")
# 
# # diagnoses
# metadata <- readWorksheetFromFile("/home/per/Polycarp/KBH/P101_barnmisshandel/Indata/dataDictionary_161009.xlsx", sheet = "barnmissh") 
# 
# metadata$variable <- paste0("n_", metadata$variable)
# # collapse all code variables
# diags <- grep("kod", names(metadata), value = TRUE)
# 
# #insert leading spaces
# metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))
# 
# # assemble search phrases
# metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))
# 
# derivVars <- subset(metadata, derived == "ja")
# metadata <- subset(metadata, derived == "nej")
# 
# # searches should start with an exact match but may end on any string 
# metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)
#-------------------------------------------------------------------------------

#--------------------------------- barndata ------------------------------------
if(TRUE){
  #read in mfr from grunddata and mfr from mfr_1
  par_barn <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_PAR_B_28574_2015.txt",
                    sep = "\t",
                    header = TRUE,
                    stringsAsFactors = FALSE
  )
  
  par_barn$Cnation  = gsub("\xe4","ä",par_barn$Cnation)
  par_barn$Cnation<- gsub("\xf6","ö",par_barn$Cnation)
  par_barn$Cnation<- gsub("\U3e35653c","å",par_barn$Cnation)
  par_barn$Cnation<- gsub("\xd6","Ö",par_barn$Cnation)
  
  par_barn$DIAGNOS = paste(" ",par_barn$DIAGNOS," ", sep="") 
  
  #new eko
  cols = c(names(par_barn)[grep("EKO",names(par_barn))])
  
  par_barn$EKO <- apply(par_barn[ , cols ] , 1 , paste , collapse = " ")
  par_barn$EKO = paste(" ",par_barn$EKO," ", sep="") 
  
  #new Special var for derivation of UtrObsMissh 2015-11-03
  cols = c(names(par_barn)[grep("EKO|MDC|DIAG|hdia",names(par_barn))])
  
  par_barn$SPEC <- apply(par_barn[ , cols ] , 1 , paste , collapse = " ")
  par_barn$SPEC = paste(" ",par_barn$SPEC," ", sep="") 
  
  #merge EKO and diagnosis
  par_barn$DIAGNOS_EKOD = paste(par_barn$DIAGNOSER,par_barn$EKO,sep="")
  
  #-------------------------------------------------------------------------------
  
  #subset on vars and date
  par_barn = par_barn[ ,c("lpnr","INDATUM","UTDATUM","DIAGNOS", "OP" 
                                                 ,"EKO","DIAGNOS_EKOD"
                                                 ,"SJUKHUS","SPEC","source")]
  
  #--------------------------- tillägg oktober 2016-------------------------------
  
  # load(file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
  # DMP_01_mfr <- DMP_01_mfr[,c("lpnr_BARN", "BDIAG", "BFODDAT")]
  # names(DMP_01_mfr) <- c("lpnr","DIAGNOSER", "INDATUM")
  # DMP_01_mfr$INDATUM <- as.character(as.Date(as.character(DMP_01_mfr$INDATUM), "%Y%m%d"))
  # 
  # par_barn1 <- bind_rows(par_barn1,DMP_01_mfr)
  # rm(DMP_01_mfr);gc()
  # 
  # load(file.path(path, "Output","DMP","DMP_06_dodBarn.Rdata"))
  # 
  # #new eko
  # cols = c(names(DMP_06_dodBarn)[grep("ORSAK",names(DMP_06_dodBarn))])
  # 
  # DMP_06_dodBarn <- as.data.frame(DMP_06_dodBarn)
  # 
  # DMP_06_dodBarn$DIAGNOSER <- apply(DMP_06_dodBarn[ , cols ] , 1 , paste , collapse = " ")
  # DMP_06_dodBarn$DIAGNOSER = paste(" ",DMP_06_dodBarn$DIAGNOSER," ", sep="") 
  # 
  # DMP_06_dodBarn <- DMP_06_dodBarn[,c("lpnr","DIAGNOSER", "DODSDATN")]
  # DMP_06_dodBarn$INDATUM <- DMP_06_dodBarn$DODSDATN
  # DMP_06_dodBarn$DODSDATN <- NULL
  # 
  # par_barn1 <- bind_rows(par_barn1, DMP_06_dodBarn)
  # rm(DMP_06_dodBarn);gc()
  
  saveRDS(par_barn, "Output/par_barn.rds")
}else{
  par_barn <- readRDS("Output/par_barn.rds")
}

#-------------------------------------------------------------------------------

# # apply search phrase on these
# applySearch <- function(variable, phrase){
#   ## variable: variable containing ICD diag
#   ## phrase: a string containing string or regex to be used for matching
#   out<- ifelse(grepl(phrase, variable), 1,0)  
#   return(out)
# }
# 
# # create diagnosis variables for student project
# setDT(par_barn)
# par_barn[,(metadata$variable):=lapply(metadata$search, applySearch, variable = par_barn2$DIAGNOSER),]
# # derive new diagnoses
# 
# par_barn <- data.frame(par_barn)
# 
# # calculate derived vars
# for( i in 1:nrow(derivVars)){
#   # row: dataframe with code definitions
#   row <- derivVars[i,]
#   variable <- as.character(row$variable)
#   
#   vars <- gsub(" ", "", unlist(strsplit(gsub("\\|","",as.character(row$search)), " ")))
#   vars <- subset(vars, vars!="" & vars != "NA")
#   
#   par_barn[, variable] <- ifelse(rowSums(par_barn[,paste0("n_",vars)])>0,1,0)
# }
#-------------------------------------------------------------------------------

# #---------- extra oct 2015 add time dependency for some diagnosis vars ---------
# vars = c(metadata$variable, derivVars$variable)
# vars <- gsub("\\.", "_", vars)
# vars <- gsub(" ", "_", vars)
# vars <- gsub("-", "_", vars)
# 
# kalenderVars = paste("alderDiagnosManad_",vars,sep="")
# 
# load(file=file.path(path, "Output","DMP","DMP_01_mfr.Rdata"))
# mfr = DMP_01_mfr[,c("lpnr_BARN","BFODDAT","BUTDAT")]
# names(mfr) = c("lpnr","BFODDAT","BUTDAT")
# 
# par_barn = merge(par_barn,mfr,by="lpnr",all.x=TRUE)
# 
# par_barn$INDATUM = as.Date(par_barn$INDATUM,format="%Y-%m-%d")
# par_barn$BFODDAT = as.Date(as.character(par_barn$BFODDAT),format="%Y%m%d")
# par_barn$BUTDAT = as.Date(as.character(par_barn$BUTDAT),format="%Y%m%d")
# 
# #fix vars
# par_barn$INDATUM.1 = NULL
# par_barn$kalenderAr = difftime(par_barn$INDATUM,par_barn$BFODDAT,units ="days")
# par_barn$kalenderAr = as.numeric(par_barn$kalenderAr/(365.24/12))
# 
# #Ålder vid utskrivning
# par_barn$kalenderUtskrivning = difftime(par_barn$BUTDAT,par_barn$BFODDAT,units ="days")
# par_barn$kalenderUtskrivning = as.numeric(par_barn$kalenderUtskrivning/(365.24/12))
# 
# setDT(par_barn)
# 
# names(par_barn2) <- gsub("\\.", "_", names(par_barn2))
# names(par_barn2) <- gsub(" ", "_", names(par_barn2))
# names(par_barn2) <- gsub("-", "_", names(par_barn2))
# 
# # if diagnosis exists (x==1) find date (or rather age) for first occurence.
# datefinder = function(x,dte){ifelse(is.null(which.first(x == 1)),NA,dte[which.first(x == 1)])}
# 
# setkey(par_barn2,lpnr,kalenderAr)
# par_barn2[,(kalenderVars):=lapply(vars, function(x){datefinder(get(x),get("kalenderAr"))}),by="lpnr"]
# 
# # Koda om vars, 1,2 för att avgöra om diagnos inträffat innan utskrivning
# diagnosUtskrivning <- paste("diagnosInnanUtskrivning", kalenderVars,sep="")
# diagnosUtskrivning <- gsub("alderDiagnosManad", "", diagnosUtskrivning)
# par_barn2[,(diagnosUtskrivning):=lapply(kalenderVars, function(x){ifelse(get(x)>get("kalenderUtskrivning"),1,2)}),]
# 
# par_barn2$kalenderAr = NULL
# 
# # first maltreatment syndrome
# table(par_barn2$alderDiagnosManad_n_maltreatmentSyndrome)
# 
# #par_barn2[,minMaltreatment:=min(alderDiagnosManad_n_maltreatmentSyndrome, na.rm =TRUE), by = lpnr]
# #par_barn2$minMaltreatment <- ifelse(par_barn2$minMaltreatment==Inf, NA, par_barn2$minMaltreatment)
# 
# 
# times <- 
# par_barn2 %>% 
#   select(lpnr,INDATUM, lan_text, n_maltreatmentSyndrome) %>% 
#   filter(n_maltreatmentSyndrome == 1) %>% 
#   data.table
# 
# # Sort on time
# setkey(times, lpnr, INDATUM)
# 
# times <- times[!duplicated(times$lpnr)]
# 
# times <- 
# times %>% 
#   select(lpnr, lan_text) %>% 
#   mutate(lanMaltreatment = lan_text,
#          lan_text = NULL) %>% 
#   data.table
# 
# setkey(times, lpnr)
# setkey(par_barn2, lpnr)
# 
# par_barn2 <- times[par_barn2]
# 
# 
# #special time dependencies
# par_barn2$n_NeoSDH7 <- ifelse(par_barn2$alderDiagnosManad_n_NeoSDH7*(365.24/12)<7,par_barn2$n_NeoSDH7, NA)
# par_barn2$n_NEOSDH28 <- ifelse(par_barn2$alderDiagnosManad_n_NEOSDH28*(365.24/12)<28,par_barn2$n_NEOSDH28, NA)
# par_barn2$n_SDH_T_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_SDH_T_1_11 >1 & par_barn2$alderDiagnosManad_n_SDH_T_1_11 <12, par_barn2$n_SDH_T_1_11,NA)
# par_barn2$n_SDH_IT_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_SDH_IT_1_11 >1 & par_barn2$alderDiagnosManad_n_SDH_IT_1_11 <12, par_barn2$n_SDH_IT_1_11,NA)
# par_barn2$n_Alla_SDH_1_11 <- ifelse(par_barn2$alderDiagnosManad_n_Alla_SDH_1_11 >1 & par_barn2$alderDiagnosManad_n_Alla_SDH_1_11 <12, par_barn2$n_Alla_SDH_1_11,NA)
# 
#-------------------------------------------------------------------------------
#out1 <- grep("diagnosInnanUtskrivning", names(par_barn2), value=T)
#--------------------------- prepare for print ---------------------------------
# DMP_02_barndiagnoser = data.frame(par_barn2)
# write.csv2(DMP_02_barndiagnoser[,c(names(DMP_02_barndiagnoser)[1:7],"barnMisshandel_d1")], file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.csv"),na="",row.names=F)
# save(DMP_02_barndiagnoser, file=file.path(path, "Output","DMP","DMP_02_barndiagnoser.Rdata"))
# EOF