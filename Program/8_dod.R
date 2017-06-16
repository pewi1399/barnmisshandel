rm(list=objects())

#Source
#source("Program/DMP/New/functions/deriveMFR.R", encoding="utf-8")

#read in mfr
dod <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_DORS_B_28574_2015.txt",
                  sep = "\t",
                  header = TRUE
)
#-------------------------------------------------------------------------------

dod <-
  dod %>% 
  select(LopNr, 
         AR, 
         ULORSAK, 
         DALDDAG, 
         DALDMAN, 
         ALDER, 
         DALDKL5, 
         DODSPL, 
         PNRQ,
         Skada)

names(dod) <- paste0(names(dod), "_dodbarn")

dod <- 
  dod %>% 
rename("LopNrBarn" = LopNr_dodbarn) 