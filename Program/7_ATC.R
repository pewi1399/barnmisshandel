rm(list=ls())
library(data.table)
library(dplyr)

source("Program/functions.R", encoding = "utf-8")

lmed_foralder <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_LMED_F_28574_2015.txt",
                      sep = "\t",
                      header = TRUE,
                      stringsAsFactors = FALSE
)

lmed_foralder <- 
lmed_foralder %>% 
  select(lopnr, EDATUM, FDATUM, atc) %>% 
  setDT(key = "lopnr")

diaglist <- c("N05A", "N05B", "N05C", "N06A", "N06B","N06AB")


lmed_foralder[,(diaglist):=lapply(diaglist, applySearch, variable = lmed_foralder$atc) ]


lmed_foralder$LM <- ifelse(lmed_foralder$N05A == 1, 1, 
                           ifelse(lmed_foralder$N05B == 1, 2, 
                                  ifelse(lmed_foralder$N05C == 1, 3, 
                                         ifelse(lmed_foralder$N06A == 1, 4, 0)
                                         )
                                  )
                           )

lmed_foralder$SSRI <- ifelse(lmed_foralder$N06AB == 1, 1, 0)
lmed_foralder$ADHD <- ifelse(lmed_foralder$N06B == 1, 1, 0)

# ADHD is invariant of maltreatment diagnosis
lmed_adhd <- lmed_foralder[,list(ADHD = ifelse(sum(ADHD)>0,1,0)),by = "lopnr"]

# read in diagnosis to determine when medication occurs relative to maltreatment
maltreatment_time <- readRDS("Output/6_maltreatmenttime.rds")

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
maltreatment_time <- merge(maltreatment_time, koppling, by.x = "lopnr", by.y = "LopNr")

koppling_mor <- koppling %>% select(LopNr, LopNrMor) %>% 
  rename(LopNrDotter = "LopNr")

koppling_far <- koppling %>% select(LopNr, LopNrFar) %>% 
  rename(LopNrSon = "LopNr")

tmp <- merge(lmed_foralder, koppling_far, by.x = "lopnr", by.y = "LopNrFar", all.x = TRUE) 
tmp <- merge(tmp, koppling_mor, by.x = "lopnr", by.y = "LopNrMor", all.x = TRUE)



#maltreatment_time %>% 
#  filter(lopnr %in% c(2325417, 2464590, 2653999))

# get rid of medicated without maltreatment
lmed_foralder <-
  lmed_foralder %>% 
  filter()
  


lmed_foralder$mother <- ifelse(lmed_foralder$lopnr %in% koppling$LopNrMor, 1, 0)
lmed_foralder$father <- ifelse(lmed_foralder$lopnr %in% koppling$LopNrFar, 1, 0)



lmed_mor <-
  lmed_foralder %>% 
  filter(mother == 1) %>% 
  select(lopnr,EDATUM, LM, SSRI)

lmed_far <-
  lmed_foralder %>% 
  filter(father ==1) %>% 
  select(lopnr,EDATUM, LM, SSRI)

rm(lmed_foralder)





