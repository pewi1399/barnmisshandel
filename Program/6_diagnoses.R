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
MALTREATMENT = TRUE
MERGE = TRUE
ncores <- detectCores() - 1
  

# filter and split data dictionary
metadata_barn <- metadata %>% 
  filter(grepl("^barn$|^barn_", Group) & Barnmissh == 1 & derived == "nej")

# foraldrar
metadata_foralder <- metadata %>% 
  filter(grepl("^mor_|^mor$|^far_|^far$", Group) & Barnmissh == 1 & derived == "nej")

# derive diagnoses in steps. Apply search per source 
#--------------------------------- PAR BARN ------------------------------------
if(BARN){
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
   #par_barn <- par_barn[1:20000,]
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "par_barn", "applySearch"))
    
   # Rprof("Output/hjsdfhjdsf")
    system.time({
    outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_parbarn", type = "par", dataset = par_barn, diagvar = "DIAGNOS")
    })
    #Rprof()
    #summaryRprof("Output/hjsdfhjdsf")
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
        Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="lopnr"), .)
    
    saveRDS(out, "Output/6_par_barn.rds")
    rm(par_barn)
    rm(outlist)
  }
  
  if(MFR){
    mfr_barn <- readRDS("Output/1_mfr.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "mfr_barn", "applySearch"))
    
    system.time({
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_mfrbarn", type = "mfr", dataset = mfr_barn, "BDIAG")
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="BLOPNR"), .)
    
    out <- out[,-grep("Mlopnr.", names(out))]
    
    mfr_grundvariabler <- mfr_barn #%>% 
      #select(BLOPNR, 
       #    BDIAG, 
        #   MDIAG, 
        #   SJUKHUS_S, 
        #   MFLOP, 
        #   BFLOP, 
        #   CMFODLAND,
        #   Cfnat,
        #   Cmnat)
    
    out <- merge(out, mfr_grundvariabler, by = "BLOPNR")
    
    
    saveRDS(out, "Output/6_mfr_barn.rds")
    rm(mfr_barn)
    rm(outlist)
    
  }
}

#------------------------------- PAR FORALDRAR ---------------------------------
if(FORALDRAR){
  
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
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_parforalder", type = "par", dataset = par_foralder, diagvar = "DIAGNOS")
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="LopNrBarn"), .)
    
    saveRDS(out, "Output/6_par_foralder.rds")
    rm(par_foralder)
  }
  
  if(MFR){
    mfr_foralder <- readRDS("Output/1_mfr.rds")
    
    cl <- makeCluster(ncores)
    
    clusterExport(cl, c("parallelDiagnoses", "mfr_foralder", "applySearch"))
    
    system.time({
      outlist <- parLapply(cl, ll, parallelDiagnoses, suffix = "_mfrforalder", type = "mfr", dataset = mfr_foralder, diagvar = "MDIAG")
    })
    
    parallel::stopCluster(cl)
    
    out <- 
      outlist %>% 
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("BLOPNR", "Mlopnr")), .)
    
    saveRDS(out, "Output/6_mfr_foralder.rds")
    rm(mfr_foralder)
  }
}

#----------------------- calculate times for misshandel ------------------------
#mfr_barn <- readRDS("Output/1_mfr.rds")
if(MALTREATMENT){
  par_barn <- readRDS("Output/2_par_barn.rds")
  
  metadata_maltreatment <- 
    metadata_barn %>% 
      filter(variable == "n_maltreatmentSyndrome") 
    
  setDT(par_barn)
  
  par_barn[,(paste0("n_maltreatmentSyndrome")):=lapply(metadata_maltreatment$search, applySearch, variable = par_barn$DIAGNOS),]
  
  out <- 
    par_barn %>%
    filter(n_maltreatmentSyndrome == 1) %>% 
    select(lopnr, INDATUM) %>%
    group_by(lopnr) %>% 
    mutate(firstMaltreatment = min(INDATUM)) %>%
    ungroup() %>% 
    filter(firstMaltreatment == INDATUM) %>% 
    select(lopnr, firstMaltreatment) %>% 
    distinct(lopnr, firstMaltreatment) %>% 
    mutate(maltreatmentYear = substr(firstMaltreatment,1,4))
  
  
  firstdate <- out %>%
    select(lopnr , maltreatmentYear) %>% 
    mutate(maltrementYear = as.numeric(maltreatmentYear))
  
  system.time({
    write.table(firstdate, "Output/6_firstMaltreatment.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  })
  
  saveRDS(out, "Output/6_maltreatmenttime.rds")
}
#-------------------------------------------------------------------------------

#------------------ Merge (maybe move this to another script) ------------------

if(MERGE){
  mfr <- readRDS("Output/6_mfr_barn.rds")
  mfr <- mfr[!duplicated(mfr$BLOPNR),]
  
  
  par_barn <- readRDS("Output/6_par_barn.rds")
  par_barn <- dplyr::rename(par_barn, BLOPNR = lopnr)
  
  
  mfr_foralder <- readRDS("Output/6_mfr_foralder.rds")
  mfr_foralder <- mfr_foralder[!duplicated(mfr_foralder$BLOPNR), grep("^n_|BLOPNR", names(mfr_foralder))]
  
  
  par_foralder <- readRDS("Output/6_par_foralder.rds")
  par_foralder <- dplyr::rename(par_foralder, LopNrForalder = lopnr)
  
  
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

  tmp <- grep("_mfrbarn$|_parbarn$|_mfrforalder$|_parforalder$", names(analysdata), value = TRUE, invert = TRUE)
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
           -Cmnat
           ) %>% 
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
      select(-FODAR) %>% 
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
  
  
  openxlsx::saveWorkbook(wb, file = "Output/6_diagnoser.xlsx", overwrite = TRUE)
  saveRDS(analysdata, "Output/6_analysdata.rds")

}
#-------------------------------------------------------------------------------