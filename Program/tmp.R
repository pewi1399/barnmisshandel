rm(list = ls())

library(data.table)
library(dplyr)

setwd("C:/Users/perwim/Desktop/TMP/MOVE/ULF_P101")
par_barn <- readRDS("Output/2_par_barn.rds")
setDT(par_barn)
#par_barn <- par_barn[1:10000,]


source("Program/functions.R", encoding = "utf-8")

# filter and split data dictionary
metadata_barn <- metadata %>% 
  filter(grepl("^barn$|^barn_", Group) & Barnmissh == 1 & derived == "nej")


var_list <- metadata_barn$variable




parallelDiagnoses <- function(metadata_tmp){
  library(data.table)
  setDT(par_barn)
  
  par_barn[,(paste0(metadata_tmp$variable, "_parbarn")):=lapply(metadata_tmp$search, applySearch, variable = par_barn$DIAGNOS),]
  
  par_barn <- par_barn[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>1,1,0)}), by = "lopnr", .SDcols = paste0(metadata_tmp$variable, "_parbarn")]
  par_barn <- data.frame(par_barn)
  
  out <- par_barn[,c("lopnr", paste0(metadata_tmp$variable, "_parbarn"))]
  return(out)
}

#-------------------------------- derive diagnoses -----------------------------
library(parallel)

ncores <- detectCores() - 1
splitvector <- rep(1:ncores, length(var_list)/ncores)

ll <- list()
for(i in 1:ncores){
  ll[[i]] <- metadata[splitvector == i,]
}

cl <- makeCluster(ncores)

clusterExport(cl, c("parallelDiagnoses", "par_barn", "applySearch"))

system.time({
outlist <- parLapply(cl, ll, parallelDiagnoses)
})

parallel::stopCluster(cl)


out <- 
  outlist %>% 
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="lopnr"), .)

saveRDS(out, "Output/7_par_barn_test.rds")



