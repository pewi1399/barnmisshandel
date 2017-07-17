# fel <- analysdata_bu_atc_scb %>%
#     select(LopNrBarn, 
#            n_Alla_SDH_1_11, 
#            n_subduralBlodningTrauma, 
#            n_kramperAlla, 
#            n_markerLongbone, 
#            n_markerIntracranial)
# 
# lapply(fel, sum, na.mr = TRUE)

rm(list = ls())

library(dplyr)
library(data.table)
library(testthat)
library(tidyr)
library(parallel)

source("Program/functions.R", encoding = "utf-8")


# ange de variabler som skall beräknas
test_variabler <- c("n_SDH_IT_1_11", "n_SDH_T_1_11","n_Alla_SDH_1_11" )

metadata_fel <- metadata %>% 
  filter(variable %in% test_variabler)

#make each code a diagnosis of its own
x = "n_subduralBlodningTrauma"

checkDiagnose <- function(x, metadata){
  BARN = TRUE
  
  metadata <- subset(metadata, variable == x)
  
  diagnosecodes <- unique(unlist(strsplit(metadata$search, "\\|")))
  diagnosecodes <- diagnosecodes[!grepl("^ NA$", diagnosecodes)]
  
  variable_names <- c(metadata$variable, paste0("Diagnoskod_", gsub(" ", "", diagnosecodes)))
  variable_search <- c(metadata$search, diagnosecodes)
  
 # # add parents 
 # kopplingBio <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_BIOFORAL.txt",
 #                           sep = "\t",
 #                           header = TRUE,
 #                           stringsAsFactors = FALSE
 # )
 # 
 # kopplingAdoptiv <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_ADOPTIVF.txt",
 #                               sep = "\t",
 #                               header = TRUE,
 #                               stringsAsFactors = FALSE
 # )
 # 
 # # order of names is important
 # names(kopplingAdoptiv) <- names(kopplingBio)
 # koppling <- rbind(kopplingBio, kopplingAdoptiv)
 # 
 # koppling <-
 #   koppling %>% 
 #   rename("LopNrBarn" = LopNr) %>% 
 #   filter(!duplicated(LopNrBarn))
  
  mfr_data <- readRDS("Output/1_mfr.rds")
  setDT(mfr_data)
  if(BARN){
    par_data <- readRDS("Output/2_par_barn.rds")
  }else if(FORALDER){
    par_data <- readRDS("Output/3_par_foralder.rds")
  

  
  mfr_data <- merge(mfr_data, koppling, by.x = "BLOPNR", by.y = "LopNrBarn", all.x = TRUE)
  }
  
  setDT(par_data)
  
  # calculate par_diagnoses
  par_data[,(paste0(variable_names, "_par")):=lapply(variable_search, applySearch, variable = DIAGNOS),]
  par_data <- par_data[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>0,1,0)}), by = "lopnr", .SDcols = paste0(variable_names, "_par")]
  par_data <- data.frame(par_data)
  par_data <- par_data[!duplicated(par_data$lopnr),]
  
  if(BARN){
    mfr_data[,(paste0(variable_names, "_mfr")):=lapply(variable_search, applySearch, variable = BDIAG),]
    mfr_data <- mfr_data[!duplicated(mfr_data$BLOPNR),]
    
    testdata <- merge(mfr_data, par_data, by.x = "BLOPNR", by.y = "lopnr", all.x = TRUE)
  }else if(FORALDER){
    mfr_data[,(paste0(variable_names, "_mfr")):=lapply(variable_search, applySearch, variable = MDIAG),]
  }
  
  # skapa aggregerade diagnos
  testdata[,(variable_names):=lapply(variable_names, 
                                                function(x){
                                                  
                                                  sumvars <- grep(paste0("^", x ,"_[a-z]*$"), names(testdata), value = TRUE)
                                                  print(x)
                                                  print(sumvars)
                                                  
                                                  if(length(sumvars) != 2){
                                                    stop("Incorrect number of variables to sum")
                                                  }else{
                                                    ifelse(rowSums(testdata[,sumvars, with = FALSE], na.rm = TRUE) > 0, 1, 0)
                                                  }
                                                }),
             ]
  
  # add classification 
  fall_kontroll <- read.table("Indata/Sos_20170407/SoS/Data/META_28574_2015.tab",
                              sep = "\t",
                              header = TRUE,
                              stringsAsFactors = FALSE
  )
  
  fall_kontroll <- fall_kontroll %>% 
    select(LopNr, TYPE, FODAR) %>% 
    rename(BLOPNR = LopNr) %>% 
    setDT(key = "BLOPNR") %>% 
    distinct()
  
  
  testdata <- merge(testdata, fall_kontroll, by = "BLOPNR", all.x = TRUE)

  out <- testdata[,grep("^Diagnoskod|^n_|FODAR", names(testdata)), with = FALSE]
  
  out <- out[,lapply(.SD, function(x){sum(x, na.rm = TRUE)}), by = "FODAR"]
  
  setkey(out, "FODAR")
  
  openxlsx::write.xlsx(out, paste0("Output/", x, "_med_underdiagnoser.xlsx"))
}

lapply(test_variabler,
       checkDiagnose, metadata = metadata)
