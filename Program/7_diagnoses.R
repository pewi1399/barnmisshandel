rm(list = ls())

library(dplyr)
library(data.table)
library(testthat)
library(tidyr)
library(parallel)

source("Program/functions.R", encoding = "utf-8")
BARN = TRUE
FORALDRAR = TRUE
MFR = TRUE
PAR = TRUE
MERGE = FALSE
ncores <- detectCores() - 4
  

# filter and split data dictionary
metadata_barn <- metadata %>% 
  filter(grepl("^barn$|^barn_", Group) & Barnmissh == 1 & derived == "nej")

# foraldrar
metadata_foralder <- metadata %>% 
  filter(grepl("^mor_|^mor$|^far_|^far$", Group) & Barnmissh == 1 & derived == "nej")

# derive diagnoses in steps. Apply search per source 
#--------------------------------- PAR BARN ------------------------------------
if(BARN){
#  system.time({
#  par_barn <- readRDS("Output/2_par_barn.rds")
# 
#  setDT(par_barn)
#  par_barn[,(paste0(metadata_barn$variable, "_parbarn")):=lapply(metadata_barn$search, applySearch, variable = par_barn$DIAGNOS),]
#  # derive new diagnoses
# 
#  par_barn <- par_barn[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>0,1,0)}), by = "lopnr", .SDcols = paste0(metadata_barn$variable, "_parbarn")]
#  
#  par_barn <- data.frame(par_barn)
#  saveRDS(par_barn, "Output/7_par_barn.rds")
#  rm(par_barn)
#  gc()
#  }) #1520.83
  #-------------------------------- derive diagnoses ---------------------------
  var_list <- metadata_barn$variable
  
  splitvector <- rep(1:ncores, length(var_list)/ncores)
  
  ll <- list()
  for(i in 1:ncores){
    ll[[i]] <- metadata_barn[splitvector == i,]
  }
  #----------------------------------------------------------------------------- 
  if(PAR){
    par_barn <- readRDS("Output/2_par_barn.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "par_barn", "applySearch"))
    
    system.time({
    outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_parbarn", type = "par", dataset = par_barn)
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
        Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="lopnr"), .)
    
    saveRDS(out, "Output/7_par_barn_test.rds")
  }
  
  if(MFR){
    mfr_barn <- readRDS("Output/1_mfr.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "mfr_barn", "applySearch"))
    
    system.time({
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_mfrbarn", type = "mfr", dataset = mfr_barn)
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="BLOPNR"), .)
    
    saveRDS(out, "Output/7_mfr_barn_test.rds")
    
    
  }
}

#------------------------------- PAR FORALDRAR ---------------------------------
if(FORALDRAR){
#  system.time({
#  par_foralder <- readRDS("Output/3_par_foralder.rds")
#  
#  setDT(par_foralder)
#  par_foralder[,(paste0(metadata_foralder$variable, "_parbarn")):=lapply(metadata_foralder$search, applySearch, variable = par_foralder$DIAGNOS),]
#  # derive new diagnoses
#  
#  par_foralder <- par_foralder[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>0,1,0)}), by = "lopnr", .SDcols = paste0(metadata_foralder$variable, "_parbarn")]
#  
#  par_foralder <- data.frame(par_foralder)
#  saveRDS(par_foralder,"Output/7_par_foralder.rds")
#  rm(par_foralder)
#  }) # 1165.78
  
  
  var_list <- metadata_foralder$variable
  splitvector <- rep(1:ncores, length(var_list)/ncores)
  
  ll <- list()
  for(i in 1:ncores){
    ll[[i]] <- metadata_foralder[splitvector == i,]
  }
  
  if(PAR){
    par_foralder <- readRDS("Output/3_par_foralder.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "par_foralder", "applySearch"))
    
    system.time({
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_parforalder", type = "par", dataset = par_foralder)
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="lopnr"), .)
    
    saveRDS(out, "Output/7_par_foralder_test.rds")
  }
  
  if(MFR){
    mfr_foralder <- readRDS("Output/1_mfr.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "mfr_foralder", "applySearch"))
    
    system.time({
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_mfrforalder", type = "mfr", dataset = mfr_foralder)
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("BLOPNR", "Mlopnr")), .)
    
    saveRDS(out, "Output/7_mfr_foralder_test.rds")
    
    
  }
}

#------------------------------------ MFR --------------------------------------
if(MFR){
  #if(BARN){
  #  mfr <- readRDS("Output/1_mfr.rds")
  #  
  #  setDT(mfr)
  #  
  #  system.time({
  #  mfr[,(paste0(metadata_barn$variable, "_mfr")):=lapply(metadata_barn$search, applySearch, variable = mfr$BDIAG),]
  #  # derive new diagnoses
  #  
  #  #mfr <- mfr[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "BLOPNR", .SDcols = paste0(metadata_barn$variable, "_mfr")]
  #  
  #  mfr <- data.frame(mfr)
  #  saveRDS(mfr, "Output/7_mfr_barn.rds")
  #  rm(mfr)
  #  gc()
  #  }) # 
  #}
  
#  if(FORALDRAR){
#    mfr <- readRDS("Output/1_mfr.rds")
#    
#    setDT(mfr)
#    
#    system.time({
#    mfr[,(paste0(metadata_foralder$variable, "_mfr")):=lapply(metadata_foralder$search, applySearch, variable = mfr$MDIAG),]
#    # derive new diagnoses
#    
#    #mfr <- mfr[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "BLOPNR", .SDcols = paste0(metadata_foralder$variable, "_mfr")]
#    
#    mfr <- data.frame(mfr)
#    saveRDS(mfr, "Output/7_mfr_foralder.rds")
#    rm(mfr)
#    gc()
#    })
#  }
    
}
#------------------ Merge (maybe move this to another script) ------------------

if(MERGE){
  mfr <- readRDS("Output/7_mfr_barn.rds")
  mfr <- mfr[!duplicated(mfr$BLOPNR),]
  
  
  par_barn <- readRDS("Output/7_par_barn.rds")
  
  par_barn <- dplyr::rename(par_barn, BLOPNR = lopnr)
  
  
  mfr_foralder <- readRDS("Output/7_mfr_foralder.rds")
  mfr_foralder <- mfr_foralder[!duplicated(mfr$BLOPNR), grep("^n_|BLOPNR", names(mfr_foralder))]
  
  
  par_foralder <- readRDS("Output/7_par_foralder.rds")
  
  par_foralder <- dplyr::rename(par_foralder, BLOPNR = lopnr)
  
  setDT(mfr, key = "BLOPNR")
  setDT(par_barn, key = "BLOPNR")
  setDT(mfr_foralder, key = "BLOPNR")
  setDT(par_foralder, key = "BLOPNR")
 
  mfr_cases <- nrow(mfr) 
    
  analysdata <- merge(mfr, par_barn, by = "BLOPNR", all.x = TRUE)
  analysdata <- merge(analysdata, mfr_foralder, by = "BLOPNR", all.x = TRUE)
  analysdata <- merge(analysdata, par_foralder, by = "BLOPNR", all.x = TRUE)
 
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

  analysdata <- merge(analysdata, fall_kontroll, by = "BLOPNR", all.x = TRUE)
  
   test_that({"expect same number of rows in mfr as before merge"},
             expect_equal(mfr_cases, nrow(analysdata))
             )
  
  # get rid of variables that are not derived from 
  metadataSingle <- metadata %>% 
    filter(Barnmissh == 1 & derived == "nej")
   
   
  analysdata[,(metadataSingle$variable):=lapply(metadataSingle$variable, 
                                          function(x){
                                            
                                            sumvars <- grep(paste0("^", x ,"_[a-z]*$"), names(analysdata), value = TRUE)
                                            print(x)
                                            print(sumvars)
                                            
                                            if(length(sumvars) != 2){
                                              stop("Incorrect number of variables to sum")
                                            }else{
                                            ifelse(rowSums(analysdata[,sumvars, with = FALSE], na.rm = TRUE) > 0, 1, 0)
                                            }
                                          }),
             ]
  
  # these should all be the same
  #table(analysdata$n_maltreatmentSyndrome == analysdata$n_maltreatmentSyndrome_mfr + analysdata$n_maltreatmentSyndrome_parbarn) 

  tmp <- grep("_mfr$|_parbarn$", names(analysdata), value = TRUE, invert = TRUE)
  analysdata <- analysdata[,tmp, with = FALSE]
  
  n_table <-   
    analysdata %>% 
    group_by(TYPE, FODAR) %>% 
    summarise(n = n())
  
  out <- 
  analysdata %>% 
    select(-BLOPNR, 
           -BDIAG, 
           -MDIAG, 
           -Mlopnr, 
           -SJUKHUS_S, 
           -MFLOP, 
           -BFLOP, 
           -CMFODLAND,
           -Cfnat,
           -Cmnat) %>% 
    group_by(TYPE, FODAR) %>%
    summarise_each(funs(sum)) %>% 
    data.frame %>% 
    arrange(FODAR, TYPE)
  
#  analysdata_long <-
#    analysdata %>%
#    select(-BLOPNR)%>%
#    gather(key, value, -TYPE, -FODAR)
  
  out <- merge(out, n_table, by = c("TYPE", "FODAR"))
  
  sheet1 <-
  out %>% 
    gather(variabel, antal, -TYPE, -FODAR, -n) %>% 
    mutate(per_100000 = round(100000 * antal/n, 2)) %>%
    gather(key, value, -TYPE, -FODAR, -n, -variabel) %>% 
    mutate(variabel = paste0(variabel, "_", key)) %>% 
    select(-key) %>% 
    spread(variabel, value) %>% 
    arrange(FODAR, TYPE) #%>% 
    #openxlsx::write.xlsx("Output/7_diagnoser.xlsx")
  
  lowerYear = 1997
  upperYear = 2013
  breakYear = 2007
  
  sheet2 <-
    out %>%
      filter(!is.na(FODAR) & FODAR >= lowerYear) %>% 
      mutate(period = ifelse(FODAR > breakYear, paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear))) %>% 
      select(-FODAR, -LAN) %>% 
      group_by(period, TYPE) %>% 
      summarise_each(funs(sum(., na.rm = TRUE))) %>% 
      gather(variabel, antal, -TYPE, -period, -n) %>% 
      mutate(per_100000 = round(100000 * antal/n, 2)) %>%
      gather(key, value, -TYPE, -period, -n, -variabel) %>% 
      mutate(variabel = paste0(variabel, "_", key)) %>% 
      select(-key) %>% 
      spread(variabel, value) %>% 
      arrange(period, TYPE)
  
  
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Year")
  openxlsx::addWorksheet(wb, "Period")
  
  openxlsx::writeData(wb, "Year", sheet1)
  openxlsx::writeData(wb, "Period", sheet2)
  
  
  openxlsx::saveWorkbook(wb, file = "Output/7_diagnoser.xlsx", overwrite = TRUE)

}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
 # calculate derived vars
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
