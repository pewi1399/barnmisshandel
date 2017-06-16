rm(list = ls())

library(dplyr)
library(data.table)
library(testthat)
library(tidyr)
library(parallel)

source("Program/functions.R", encoding = "utf-8")
ncores <- detectCores() - 1

# foraldrar
metadata_foralder_diagnos <- metadata %>% 
  filter(grepl("^foralder", Group) & Barnmissh == 1 & derived == "nej" & kalla == "diagnoser")


metadata_foralder_EKO <- metadata %>% 
  filter(grepl("^foralder", Group) & Barnmissh == 1 & derived == "nej" & kalla == "EKO")


#------------------------------ parallell --------------------------------------
par_foralder <- setDT(readRDS("Output/3_par_foralder.rds"), key = "lopnr")

system.time({
  par_foralder[,(metadata_foralder_diagnos$variable):=lapply(metadata_foralder_diagnos$search, applySearch, variable = par_foralder$DIAGNOS) ]
})

system.time({
  par_foralder[,(metadata_foralder_EKO$variable):=lapply(metadata_foralder_EKO$search, applySearch, variable = par_foralder$EKO) ]
})

#----------------------------- relate data -------------------------------------

#-------------------------------------------------------------------------------

# add parents 
kopplingBio <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_BIOFORAL.txt",
                          sep = "\t",
                          header = TRUE,
                          stringsAsFactors = FALSE
)

kopplingAdoptiv <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_ADOPTIVF.txt",
                              sep = "\t",
                              header = TRUE,
                              stringsAsFactors = FALSE
)

# order of names is important
names(kopplingAdoptiv) <- names(kopplingBio)
koppling <- rbind(kopplingBio, kopplingAdoptiv)

# join in parent ids
#maltreatment_time <- merge(maltreatment_time, koppling, by.x = "lopnr", by.y = "LopNr")

koppling_mor <- koppling %>% select(LopNr, LopNrMor) %>% 
  rename("LopNrMorBarn" = LopNr) %>% 
  filter(!is.na(LopNrMor)) %>% 
  distinct()

koppling_far <- koppling %>% select(LopNr, LopNrFar) %>% 
  rename("LopNrFarBarn" = LopNr) %>% 
  filter(!is.na(LopNrFar)) %>% 
  distinct()

tmp <- merge(par_foralder, koppling_far, by.x = "lopnr", by.y = "LopNrFar", all.x = TRUE, allow.cartesian = TRUE) 
tmp <- merge(tmp, koppling_mor, by.x = "lopnr", by.y = "LopNrMor", all.x = TRUE, allow.cartesian = TRUE)

# read in diagnosis to determine when diagnosis occurs relative to maltreatment
maltreatment_time <- readRDS("Output/6_maltreatmenttime.rds")

maltreatment_time<-
  maltreatment_time %>% 
  rename("LopNrBarn" = lopnr)


# combine children lopnr into one variable
tmp <- 
  tmp %>% 
  mutate(
    LopNrBarn = ifelse(is.na(LopNrFarBarn), LopNrMorBarn, LopNrFarBarn),
    foralder = ifelse(is.na(LopNrFarBarn), "MOR", "FAR")
  ) %>% 
  filter(LopNrBarn %in% maltreatment_time$LopNrBarn) %>% 
  rename("LopNrForalder" = lopnr) 




tmp <- merge(tmp, maltreatment_time, by= "LopNrBarn", all.x = TRUE)

#------------------ place diagnoses with respect to diagnosis ------------------ 
tmp$timediff <- difftime(tmp$INDATUM, tmp$firstMaltreatment, units = "days")

tmp$event_relation <- ifelse(tmp$timediff < 0, "before_event",
                             ifelse(tmp$timediff > 0 & tmp$timediff < 365.24*4, "diagnosis_1_4_years_after_event",
                                    ifelse(tmp$timediff >= 365.24*4, "diagnosis_5_years_after_event", NA)))

out_times <-
  tmp %>%
  select(-LopNrForalder, 
         -timediff, 
         -EDATUM, 
         -FDATUM, 
         -atc, 
         -LopNrFarBarn,
         -LopNrMorBarn,
         -firstMaltreatment,
         -maltreatmentYear,
         -timediff,
         -ADHD
  ) %>% 
  mutate(
    LM = ifelse(min(LM)>0, min(LM), 0),
    SSRI = ifelse(min(SSRI)>0, min(SSRI), 0)
  ) %>% 
  gather(key, value, -LopNrBarn, 
         -foralder, 
         -event_relation) %>%
  filter(value > 0) %>% 
  mutate(variable = paste0(key, "_", foralder,  "_", event_relation)) %>% 
  select(LopNrBarn, variable, value) %>% 
  distinct() %>% 
  spread(key = variable, value = value, fill= 0)

saveRDS(out,"Output/8_tidsdiagnoser.rds")
