rm(list=objects())
library(data.table)
library(dplyr)
library(tidyr)
#-------------------------------------------------------------------------------


#Source
system.time({source("Program/1_mfr.R", encoding="utf-8")}) # 78s
system.time({source("Program/2_par_barn.R", encoding="utf-8")}) # 55.99s 
system.time({source("Program/3_par_foralder.R", encoding="utf-8")}) # 146.85s
system.time({source("Program/4_SCB.R", encoding="utf-8")}) # 12.55s 
system.time({source("Program/5_BU.R", encoding="utf-8")}) # 0.42s 
system.time({source("Program/6_diagnoses.R", encoding="utf-8")}) # 2620s 
system.time({source("Program/7_ATC.R", encoding="utf-8")}) # 1642s?
system.time({source("Program/8_dod.R", encoding="utf-8")}) # 0.18s
system.time({source("Program/9_tidsdiagnoser.R", encoding="utf-8")}) # 378s


