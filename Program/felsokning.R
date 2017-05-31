#par_foralder <- read_sas("Indata/Sos_20170407/SoS/Data/UT_28574_2015/ut_par_f_28574_2015.sas7bdat")
#par_foralder <- data.frame(par_foralder)


library(dplyr)
library(data.table)

par_barn <- readRDS("Output/7_par_barn_test.rds")
mfr_barn <-  readRDS("Output/7_mfr_barn_test.rds")

# add classification 
fall_kontroll <- read.table("Indata/Sos_20170407/SoS/Data/META_28574_2015.tab",
                            sep = "\t",
                            header = TRUE,
                            stringsAsFactors = FALSE
)

fall_kontroll <- fall_kontroll %>% 
  select(LopNr, TYPE, FODAR) %>% 
  rename(BLOPNR = LopNr) %>% 
  setDT(key = "BLOPNR") %>% 
  distinct()

mfr <- merge(mfr_barn, fall_kontroll, by = "BLOPNR", all.x = TRUE)

mfr %>% 
  select(BLOPNR, n_maltreatmentSyndrome_mfrbarn, TYPE, FODAR) %>% 
  filter(TYPE == "CONTROL" & n_maltreatmentSyndrome_mfrbarn == 1)

par <- merge(par_barn, fall_kontroll, by.x = "lopnr" , by.y = "BLOPNR", all.x = TRUE)

par %>%
  select(lopnr,n_maltreatmentSyndrome_parbarn, TYPE, FODAR) %>% 
  filter(TYPE == "CONTROL" & n_maltreatmentSyndrome_parbarn == 1)

par_raw <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_PAR_B_28574_2015.txt",
                       sep = "\t",
                       header = TRUE,
                       stringsAsFactors = FALSE
)

par_raw <- merge(par_raw, fall_kontroll, by.x = "lopnr" , by.y = "BLOPNR", all.x = TRUE)

par_raw %>% 
  #select(lopnr,n_maltreatmentSyndrome_parbarn, TYPE, FODAR) %>% 
  filter(lopnr %in% c(102439, 132256, 2643776))