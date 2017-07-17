rm(list = ls())
library(dplyr)
library(epiR)
library(openxlsx)
library(zoo)
library(data.table)
library(tidyr)

options(scipen = 999)

?epiR::epi.conf
m <- matrix( c(10,5, 100, 100), ncol = 2) 
epi.conf(m, ctype = "inc.rate", method = "exact")

# task get incidence ratios for cases and non cases for different time periods
# 1: get divisiors = infants born in each year
# 2: caluclate number of cases per misshandelstats and period
# 3: calculate confidence intervals 
# 4: compare to old values
# 5: success!!

#---------------------------- read in nollaringar ------------------------------
# data supplied by ulf in mail july 2017
nollar <- read.xlsx("Indata/filer_ulf/Sammanräknat 1987-2016_170712.xlsx", colNames = FALSE)
names(nollar) <-c("year", "sex1", "sex2", "n")

nollar$period <- ifelse(nollar$year<1997,"<1997",
                     ifelse(nollar$year <2008, "1997_2007",
                            ifelse(nollar$year <2015, "2008_2014", ">2014")))

nollar <- 
  nollar %>% 
  group_by(period) %>% 
  summarise(n = sum(n))

# data by lan 
nollar_lan <- read.xlsx("Indata/filer_ulf/Befolkning 0 år 1990-99 & 2000-2015 per län_170712.xlsx")
nollar_lan$kon[is.na(nollar_lan$kon)] <- "alla"
nollar_lan$lan <- ifelse(grepl("[0-9]",nollar_lan$`Folkmängd.efter.region,.ålder,.kön.och.år`), nollar_lan$`Folkmängd.efter.region,.ålder,.kön.och.år`, NA)
nollar_lan$lan <- na.locf(nollar_lan$lan)

nollar_lan$`1990-1999` <- NULL
nollar_lan$`2000-2013` <- NULL

nollar_lan$p1997_2007 <- rowSums(nollar_lan[,grep("199[7-9]|200[0-7]", names(nollar_lan), value = TRUE)])
nollar_lan$p2008_2014 <- rowSums(nollar_lan[,grep("200[8-9]|201[0-4]", names(nollar_lan), value = TRUE)])

nollar_lan$p2014 <- nollar_lan$`2014`
nollar_lan$p2015 <- nollar_lan$`2015`


nollar_lan <- 
nollar_lan %>% 
  select(lan, kon, p1997_2007, p2008_2014, p2015) %>% 
  filter(kon == "alla") %>% 
  mutate(lankod = gsub("[a-ö]*|[A-Ö]| ","", lan))

nollar_lan$p2008_2015 <- nollar_lan$p2008_2014 + nollar_lan$p2015

nollar_lan$lankod <- as.numeric(nollar_lan$lankod)

nollar_lan$region <- ifelse(nollar_lan$lankod %in% c(7, 10 , 12, 13), "new_southern",
              ifelse(nollar_lan$lankod %in% c(5, 6, 8), "new_southEast",
              ifelse(nollar_lan$lankod %in% c(14), "new_western",
              ifelse(nollar_lan$lankod %in% c(3, 4, 17, 18 ,19 ,20, 21), "new_uppsalaOrebro",
              ifelse(nollar_lan$lankod %in% c(1, 9), "new_stockholm",
              ifelse(nollar_lan$lankod %in% c(22, 23, 24, 25), "new_northern",NA))))))

nollar_lan <- 
nollar_lan %>% 
  select(region, p1997_2007, p2008_2014) %>% 
  gather(key, value, -region)

nollar_lan$period <- gsub("p", "", nollar_lan$key)
nollar_lan$key <- NULL  

nollar_lan <-
nollar_lan %>% 
  group_by(region, period) %>% 
  summarise(n = sum(value, na.rm = TRUE)) %>% 
  ungroup()


nollar$n1 <- nollar$n
nollar$n <- NULL

nollar_lan$n2 <- nollar_lan$n
nollar_lan$n <- NULL
#-------------------------------------------------------------------------------

#------------------------------- beräkna diagnoser per år etc ------------------
analysdata <- readRDS("Output/10_analysdata.rds")

path_vars <- c("n_maltreatmentSyndrome",
               "n_assault",
               "n_undeterminedCauses",
               "n_adverseSocial",
               "n_dottBarn",
               "n_dottBarnMaltreatmsyndrome",
               "n_markerLongbone",
               "n_markerIntracranial",
               "n_retinalBlodning",
               "n_subduralBlodningIckeTrauma",
               "n_subduralBlodningTrauma",
               "n_subduralBlodning",
               "n_sinusventrombos",
               "n_skallfraktur",
               "n_hjarnskakning",
               "n_hjarnodem",
               "n_huvudskadaEjFraktur",
               "n_intrakraniellskada",
               "n_frakturRevben",
               "n_frakturRevbernMultipla",
               "n_RevbenAlla",
               "n_nyckelbenFraktur",
               "n_frakturLarben",
               "n_frakturLarbenskaft",
               "n_frakturskaftlongbone",
               "n_farkturLarbenMultipla",
               "n_frakturHumerusskaft",
               "n_frakturUnderben",
               "n_underbensfrakturer",
               "n_frakturSluten",
               "n_frakturOverUnderarm",
               "n_aktivRakit",
               "n_rubbningarKalciumMineral",
               "n_RakitKalcium",
               "n_osteogenesisImperfecta",
               "n_ALTE",
               "n_andningsuppehall",
               "n_ALTE_plus_andningsuppehall",
               "n_spadbarnskolik",
               "n_feberkramp",
               "n_kramper",
               "n_krampanfallUNS",
               "n_kramperAlla",
               "n_epilepsi",
               "n_anoxiskHjarnskada",
               "n_maltreatmentRelatedInjury")

path_vars <- c(path_vars, "period",
               grep("new_|motherSwed|SDH", names(analysdata), value = TRUE))

path_vars <- path_vars[!duplicated(path_vars)]

incdata <- 
analysdata %>% 
  filter(TYPE == "CASE") %>% 
  filter(period == "1997_2007" | period == "2008_2014") %>% 
  select(-TYPE)

incdata <- incdata[,path_vars]


# calculate total cases
incdata <- 
incdata %>% 
  group_by(period, n_maltreatmentSyndrome) %>% 
  summarise_all(.funs =  "sum", na.rm = TRUE) %>% 
  mutate(maltreatment = ifelse(n_maltreatmentSyndrome == 1, "Yes", "No")) %>% 
  ungroup() %>% 
  select(-n_maltreatmentSyndrome) %>% 
  gather(key, value, -period, -maltreatment)

#-------------------------------------------------------------------------------

#--------------------------------- merge data ----------------------------------
tmp <- merge(incdata, nollar, by = "period", all.x = TRUE) 
tmp1 <- merge(tmp, nollar_lan, by.x = c("period", "key"), by.y = c("period", "region"), all.x = TRUE)

incdata <- tmp1 %>% 
  mutate(n = ifelse(is.na(n2), n1, n2)) %>% 
  select(-n1, -n2)

incidences <- epi.conf(as.matrix(incdata[,c("value", "n")]),  ctype = "inc.rate", method = "exact") * 100000

out <- cbind(incdata, incidences)

out$CI <- paste0("(",round(out$lower, 2), " - ", round(out$upper,2), ")")

setDT(out, key = c("key", "period", "maltreatment"))

write.xlsx(out, "Output/incindenstal_per100000.xlsx")
#-------------------------------------------------------------------------------