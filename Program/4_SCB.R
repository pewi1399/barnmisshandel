rm(list=ls())
library(data.table)
library(haven)

scb_utbildning <- data.frame(read_sas("Indata/Sos_20170407/SCB/Datafiler/u_hogberg2_lev_utbildning.sas7bdat"))

scb <- scb_utbildning[,c("LopNr", "SUN2000Niva_old_2015")]

scb[,] <- lapply(scb[,], as.numeric)

scb <-
scb %>% 
  filter(!duplicated(LopNr))


saveRDS(scb, "Output/4_SCB.rds")