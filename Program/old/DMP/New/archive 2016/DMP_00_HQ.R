# SETUP #########################################################
# NAME      : UH
#
# R-version : 3.2.2
#              
# PRODUCES  : runs DMP scipts
# 
# STATUS    : DRAFT
#
# AUTHOR    : Per Wikman
# CREATED   : 2015-10-08
# NOTES     : 
# 
rm(list=objects())

# Todays date
datet <- format(Sys.time(), "%x")
fileDate <- gsub("-", "", datet)

# Rversion
Rversion <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep=".")

rm(list=ls())


path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)
#-------------------------------------------------------------------------------


#Source
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_01_mfr.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_02_barndiagnoser.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_03_föräldradiagnoser_far.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_03_föräldradiagnoser_mor.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_04_barnmedicinering.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_05_föräldramedicinering_mor_far.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_06_dödsorsakerbarn.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_07_dödsorsakerföräldrar.R", encoding="utf-8")
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/New/DMP_08_SNQ_barn.R", encoding="utf-8")