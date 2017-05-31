rm(list=ls())
library(data.table)
library(dplyr)

bu_barn <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/BU_BARN_28574_2015.txt",
           sep = "\t",
           header = TRUE,
           stringsAsFactors = FALSE
)

names(bu_barn) <- c("LopNr",
                    "LVUpagar",
                    "LVUstartar",
                    "LVUsenastear",
                    "KON")



bu_insats <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/BU_INSATS_28574_2015.txt",
                      sep = "\t",
                      header = TRUE,
                      stringsAsFactors = FALSE
)

# rename to fit data dictionary
names(bu_insats) <- c("LopNr", 
                      "LVUstartdat", 
                      "AVSLDAT", 
                      "LVUvardhav", 
                      "LVUvardhavsl", 
                      "LVUinstyp", 
                      "KON", 
                      "LVUvarddagar")

#bu_plac <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/BU_PLAC_28574_2015.txt",
#                      sep = "\t",
#                      header = TRUE,
#                      stringsAsFactors = FALSE
#)

#-------------------------------------------------------------------------------
setDT(bu_insats,key = c("LopNr", "LVUstartdat"))


names(bu_insats)

# calculate number of episodes
bu_insats[,index:= 1:.N, by = "LopNr"]
bu_insats[,LVUantalinsatser := max(index), by = "LopNr"]
bu_insats[,LVUvarddagartotalt := sum(LVUvarddagar), by = "LopNr"]

# split on first and last episode
bu_insats_first <- 
  bu_insats %>% 
    filter(index == 1) %>% 
    select(LopNr, LVUstartdat, LVUvardhav, LVUvardhavsl, LVUinstyp, LVUvarddagar)

bu_insats_last <- 
  bu_insats %>% 
  filter(index == LVUantalinsatser) %>% 
  select(LopNr, LVUstartdat, LVUvardhav, LVUvardhavsl, LVUinstyp, LVUvarddagar)

# label vars with source
names(bu_insats_first) <- c("LopNr", paste0(names(bu_insats_first)[-1], "_first"))
names(bu_insats_last) <- c("LopNr", paste0(names(bu_insats_last)[-1], "_last"))

# keep time invariant vars
bu_insats <- 
  bu_insats %>% 
  select(LopNr, LVUantalinsatser, LVUvarddagartotalt) %>% 
  distinct()

# merge sources
tmp <- merge(bu_insats_first, bu_insats_last, by = "LopNr")

bu_insats <- merge(bu_insats, tmp, by = "LopNr")


saveRDS(bu_insats, "Output/5_BU.rds")

