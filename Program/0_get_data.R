rm(list=objects())
library(data.table)
library(dplyr)
library(tidyr)
#-------------------------------------------------------------------------------


#Source
system.time({source("Program/1_mfr.R", encoding="utf-8")}) #s
system.time({source("Program/2_par_barn.R", encoding="utf-8")}) #s
system.time({source("Program/3_par_foralder.R", encoding="utf-8")}) #s
system.time({source("Program/4_SCB.R", encoding="utf-8")}) #s
system.time({source("Program/5_BU.R", encoding="utf-8")}) #s
system.time({source("Program/6_diagnoses.R", encoding="utf-8")}) #s
system.time({source("Program/7_ATC.R", encoding="utf-8")}) #s






