rm(list = ls())

source("Program/functions.R", encoding = "utf-8")
BARN = TRUE
FORALDRAR = FALSE
MFR = TRUE
MERGE = FALSE
 
# derive diagnoses in steps. Apply search per source 
#--------------------------------- PAR BARN ------------------------------------
if(BARN){
  par_barn <- readRDS("Output/2_par_barn.rds")
 
  setDT(par_barn)
  par_barn[,(paste0(metadata$variable, "_parbarn")):=lapply(metadata$search, applySearch, variable = par_barn$DIAGNOS),]
  # derive new diagnoses
 
  par_barn <- par_barn[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "lopnr", .SDcols = paste0(metadata$variable, "_parbarn")]
  
  par_barn <- data.frame(par_barn)
  saveRDS(par_barn, "Output/7_par_barn.rds")
  rm(par_barn)
  gc()
}

#------------------------------- PAR FORALDRAR ---------------------------------
if(FORALDRAR){
  par_foralder <- readRDS("Output/3_par_foralder.rds")
  RDS(par_foralder,"Output/7_par_foralder.rds")
}

#------------------------------------ MFR --------------------------------------
if(MFR){
  mfr <- readRDS("Output/1_mfr.rds")
  
  
  setDT(mfr)
  mfr[,(paste0(metadata$variable, "_mfr")):=lapply(metadata$search, applySearch, variable = mfr$BDIAG),]
  # derive new diagnoses
  
  mfr <- mfr[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "BLOPNR", .SDcols = paste0(metadata$variable, "_mfr")]
  
  mfr <- data.frame(mfr)
  saveRDS(mfr, "Output/7_mfr.rds")
  rm(mfr)
  gc()
}
#------------------ Merge (maybe move this to another script) ------------------

if(MERGE){
  mfr <- readRDS("Output/7_mfr.rds")
  par_barn <- readRDS("Output/7_par_barn.rds")
  
  par_barn <- dplyr::rename(par_barn, BLOPNR = lopnr)
  
  setDT(mfr, key = "BLOPNR")
  setDT(par_barn, key = "BLOPNR")
  
    
  analysdata <- merge(mfr, par_barn, by = "BLOPNR", all.x = TRUE)
  
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
  
  analysdata[,(metadata$variable):=lapply(metadata$variable, 
                                          function(x){
                                            
                                            sumvars <- grep(paste0("^", x ,"_"), names(analysdata), value = TRUE)
                                            
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
    select(-BLOPNR) %>% 
    group_by(TYPE, FODAR) %>%
    summarise_each(funs(sum)) %>% 
    data.frame %>% 
    arrange(FODAR, TYPE)
  
#  analysdata_long <-
#    analysdata %>%
#    select(-BLOPNR)%>%
#    gather(key, value, -TYPE, -FODAR)
  
  out <- merge(out, n_table, by = c("TYPE", "FODAR"))
  
  out %>% 
    gather(variabel, antal, -TYPE, -FODAR, -n) %>% 
    mutate(per_100000 = round(100000 * antal/n, 2)) %>%
    gather(key, value, -TYPE, -FODAR, -n, -variabel) %>% 
    mutate(variabel = paste0(variabel, "_", key)) %>% 
    select(-key) %>% 
    spread(variabel, value) %>% 
    arrange(FODAR, TYPE) %>% 
    openxlsx::write.xlsx("Output/7_barndiagnoser.xlsx")

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
