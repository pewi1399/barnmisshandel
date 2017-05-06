rm(list = ls())

source("Program/functions.R", encoding = "utf-8")
BARN = TRUE
FORALDRAR = FALSE
MFR = TRUE
 
# derive diagnoses in steps. Apply search per source 
#--------------------------------- PAR BARN ------------------------------------
if(BARN){
  par_barn <- readRDS("Output/2_par_barn.rds")
 
  setDT(par_barn)
  par_barn[,(metadata$variable):=lapply(metadata$search, applySearch, variable = par_barn$DIAGNOS),]
  # derive new diagnoses
 
  par_barn <- par_barn[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "lopnr", .SDcols = metadata$variable]
  
  par_barn <- data.frame(par_barn)
  saveRDS("Output/7_par_barn.rds")
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
  mfr[,(metadata$variable):=lapply(metadata$search, applySearch, variable = mfr$BDIAGS),]
  # derive new diagnoses
  
  mfr <- mfr[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "BLOPNR", .SDcols = metadata$variable]
  
  mfr <- data.frame(mfr)
  saveRDS("Output/7_mfr.rds")
}
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
