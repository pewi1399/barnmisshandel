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


rowSums(lmed_foralder[,c("LM", "SSRI", "ADHD")])
