rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)

source("Program/functions.R", encoding = "utf-8")

lmed_foralder <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_LMED_F_28574_2015.txt",
                      sep = "\t",
                      header = TRUE,
                      stringsAsFactors = FALSE
)

lmed_foralder <- 
lmed_foralder %>% 
  select(lopnr, EDATUM, FDATUM, atc) %>%
  mutate(atc = paste0(" ",atc, " ")) %>% 
  setDT(key = "lopnr")

metadata_ATC <- 
  metadata %>% 
  filter(kalla == "atc")



lmed_foralder[,(metadata_ATC$variable):=lapply(metadata_ATC$search, applySearch, variable = lmed_foralder$atc) ]


lmed_foralder$LM <- ifelse(lmed_foralder$n_N05A == 1, 1, 
                           ifelse(lmed_foralder$n_N05B == 1, 2, 
                                  ifelse(lmed_foralder$n_N05C == 1, 3, 
                                         ifelse(lmed_foralder$n_N06A == 1, 4, 0)
                                         )
                                  )
                           )

lmed_foralder$SSRI <- ifelse(lmed_foralder$n_N06AB == 1, 1, 0)
lmed_foralder$ADHD <- ifelse(lmed_foralder$n_N06B == 1, 1, 0)

# ADHD is invariant of maltreatment diagnosis
#lmed_adhd <- lmed_foralder[,list(ADHD = ifelse(sum(ADHD)>0,1,0)),by = "lopnr"]

# read in diagnosis to determine when medication occurs relative to maltreatment
maltreatment_time <- readRDS("Output/6_maltreatmenttime.rds")

maltreatment_time<-
  maltreatment_time %>% 
  rename("LopNrBarn" = lopnr)

#tmp <- merge(lmed_adhd, maltreatment_time, by = "BLOPNR")


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


tmp <- merge(lmed_foralder, koppling_far, by.x = "lopnr", by.y = "LopNrFar", all.x = TRUE, allow.cartesian = TRUE) 
tmp <- merge(tmp, koppling_mor, by.x = "lopnr", by.y = "LopNrMor", all.x = TRUE, allow.cartesian = TRUE)


#table(is.na(tmp$LopNrMorBarn), is.na(tmp$LopNrFarBarn))

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


#relate diagnosis to malteratment event

tmp$timediff <- difftime(tmp$FDATUM, tmp$firstMaltreatment, units = "days")

tmp$event_relation <- ifelse(tmp$timediff < 0, "before_event",
                             ifelse(tmp$timediff > 0 & tmp$timediff < 365.24*4, "diagnosis_1_4_years_after_event",
                                    ifelse(tmp$timediff >= 365.24*4, "diagnosis_5_years_after_event", NA)))

out_timeATC <-
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


out_adhd <-
tmp %>% 
  mutate(
    LopNrBarn = ifelse(is.na(LopNrFarBarn), LopNrMorBarn, LopNrFarBarn),
    foralder = ifelse(is.na(LopNrFarBarn), "MOR", "FAR")
  ) %>% 
  select(LopNrBarn, ADHD, foralder) %>% 
  gather(key, value, -LopNrBarn, -foralder) %>% 
  mutate(variable = paste0(foralder,key)) %>% 
  select(LopNrBarn, variable, value) %>% 
  filter(value == 1) %>% 
  distinct() %>% 
  spread(key = variable, value = value, fill = 0)


out <- merge(out_timeATC, out_adhd, by = "LopNrBarn", all = TRUE)

saveRDS(out,"Output/7_ATC.rds")
