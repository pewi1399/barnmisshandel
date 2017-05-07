rm(list=objects())
library(data.table)
library(dplyr)
library(tidyr)
#-------------------------------------------------------------------------------


#Source
system.time({source("Program/1_mfr.R", encoding="utf-8")})
system.time({source("Program/2_par_barn.R", encoding="utf-8")})
system.time({source("Program/3_par_foralder.R", encoding="utf-8")})
system.time({source("Program/7_diagnoses.R", encoding="utf-8")})






