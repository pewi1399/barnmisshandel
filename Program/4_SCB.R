rm(list=ls())
library(data.table)
library(haven)

scb_utbildning <- read_sas("Indata/Sos_20170407/SCB/Datafiler/u_hogberg2_lev_utbildning.sas7bdat")
