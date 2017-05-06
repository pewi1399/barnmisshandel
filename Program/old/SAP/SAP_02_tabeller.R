library(data.table)
library(epiR)
library(XLConnect)
library(testthat)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(haven)

rm(list=ls())

UTAN_oppen<- FALSE
if(UTAN_oppen){
  test <- readRDS("Output/DMP/AnalysdataUtanOppen.rds")
}else{
  test <- readRDS("Output/DMP/Analysdata.rds")  
}
#test <- test[1:1000,]
#load(file = "output/dmp/dmp_09_per100000.rdata") # out
load(file = "Output/DMP/DMP_09_nollar.rdata") # noll0
#load(file = "Output/DMP/DMP_100_diagnosinnan.Rdata") # out1
namnare <- read.csv2("Indata/namnareTab2.csv")
#-------------------------------------------------------------------------------
#setkey(out, year)
#setkey(out1, year)

# in out1 we find diagnoses that where to early, calculate how many these are per year
vars <- grep("^diagnosInnan", names(test), value = TRUE) 

test$year <- as.numeric(substr(as.character(test$BFODDAT), 1, 4))
# den här raden behöver korrigeras för att endast ta med individer med rätt filter
# måste den??

#only correct ones are ok
test$n_maltreatmentSyndrome <- ifelse(test$n_maltreatmentSyndrome ==1 & test$diagnosInnanUtskrivning_n_maltreatmentSyndrome != 2, 1, 0)
test$n_maltreatmentSyndrome <- ifelse(is.na(test$n_maltreatmentSyndrome),0,
                                       ifelse(test$n_maltreatmentSyndrome==1,1,0))

saveRDS(test, "Output/SAP/SAP_02_tab1.rds")
setDT(test)

early_diags <- test[,lapply(.SD, function(x) sum(x == 2, na.rm = TRUE)), 
                    by = c("year", "n_maltreatmentSyndrome"), .SDcols = vars]

setkeyv(early_diags, c("year", "n_maltreatmentSyndrome"))

#-------------------------------------------------------------------------------
tmp <- test[,grepl("^n_|^year$", names(test)), with=FALSE]


# tab1fun <- function(filter){
  filter = "n_maltreatmentSyndrome"
  useFilter <- gsub("^n_", "", filter)
  tmp[,(useFilter):=get(filter),]
  
  
  tmp <- tmp[,lapply(.SD, function(x) sum(x)), by = c("year", useFilter)]
  
  setkeyv(tmp, c("year", useFilter))
  
  out <- tmp
  # tmp[,c("n_maltreatmentSyndrome", "year"), with = FALSE]
  # out[,c("n_maltreatmentSyndrome", "year"), with = FALSE]
  #-------------------------------------------------------------------------------
  # TASK: correct the number of diagnoses by subtracting early diagnoses
  
  # merge datasets
  dat0 <- out[early_diags]
  
  # reduce the number of rows since we dont need this many
  lowerYear = 1997
  upperYear = 2013
  breakYear = 2007


  
tab1Fun <- function(lowerYear,upperYear, breakYear){
  dat0 <- 
    dat0 %>% 
    filter(!is.na(year) & year >= lowerYear) %>%   
    mutate(period = ifelse(year > breakYear, paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear)))
  
  periodVars <- grep("^diagnosInnan|^n_", names(dat0), value = TRUE)
  
  # aggregate by period
  dat1 <- dat0[,lapply(.SD, sum), by = c("period", useFilter), .SDcols = periodVars]
  
  # recast
  dat2 <- 
    dat1 %>% 
    gather(key, value, -period, -get(useFilter)) %>%
    mutate(group = ifelse(grepl("^n_", key), "diagnoser", "korrigering"),
           variable = gsub("diagnosInnanUtskrivning_", "", key),
           key = NULL
           # key = paste0(period, "_", variable),
           # period = NULL,
    ) %>%
    unite(tidyKey, period, variable, get(useFilter)) %>% 
    spread(group, value) %>% 
    mutate(variabel = gsub("\\d{4}_\\d{4}_", "", tidyKey),
           period = gsub("_n_.*$", "", tidyKey),
           tidyKey = NULL,
           diagKorr = diagnoser - korrigering) %>% 
    data.table
  
  # calculate and merge on number of infants born each year
  nollAringar<-
    noll0 %>% 
    filter(year >= lowerYear & year <= upperYear) %>% 
    mutate(period = ifelse(year >breakYear , paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear))) %>% 
    data.table
  
  nollAringar <- nollAringar[,list(n = sum(n)),by = period]
  
  setkey(nollAringar, period)
  setkey(dat2, period)
  
  dat3 <-dat2[nollAringar]
  
  
  # calculate doubles
  dat3sum <-
  dat3 %>% 
    mutate(var = gsub("_.$","", variabel))
  
  sumVars <- c("diagnoser", "korrigering", "diagKorr", "n")
  
  dat3sum <-
    dat3sum[,lapply(.SD, function(x){sum(x, na.rm = TRUE)}),
          .SDcols = sumVars, by = c("var", "period")] %>% 
    mutate(variabel = var,
           var = NULL,
           n = n/2)
  
  
  dat3 <- plyr::rbind.fill(dat3,dat3sum)
  setDT(dat3)
  
  # calculate confidence intervals and incidence estimates
  tab1 <-
    dat3[,c("diagKorr", "n"), with = FALSE] %>%
    as.matrix %>%
    epi.conf(ctype = "inc.rate", method = "exact") %>%
    cbind(dat3,.) %>% 
    data.frame %>% 
    mutate(est = format(round(est*100000,3), nsmall = 3),
           lower = format(round(lower*100000,3), nsmall = 3),
           upper = format(round(upper*100000,3), nsmall = 3)
    ) %>% 
    data.table #%>% 
  #  select(-diagnoser, -korrigering)
  
  
  # calculate relative risk
  mlt <- melt(tab1, id = c("variabel", "period"))
  tab1 <- dcast(mlt, variabel~variable+period, value.var = "value") %>%  data.frame
  
  tab1[,grep("variabel", names(tab1), invert = TRUE, value = TRUE)] = lapply(tab1[,grep("variabel", names(tab1), invert = TRUE, value = TRUE)], as.numeric)
  
  tab1$RR <- (tab1[,paste0("diagKorr_",breakYear+1,"_", upperYear)]/tab1[,paste0("n_",breakYear+1,"_", upperYear)])/
    (tab1[,paste0("diagKorr_", lowerYear, "_", breakYear)]/tab1[,paste0("n_",lowerYear,"_", breakYear)])

 return(tab1)
}
#--------------------------- tabell 2 ------------------------------------------
if(UTAN_oppen){
  test <- readRDS("Output/DMP/AnalysdataUtanOppen.rds")
}else{
  test <- readRDS("Output/DMP/Analysdata.rds")  
}
#test <- test[1:1000,]

test$new_motherSwedish <- ifelse(test$MFODLAND_REGION == "Sverige", 1, 0)
test$new_motherEuropean <- ifelse(test$MFODLAND_REGION == "Sverige" | 
                                    test$MFODLAND_REGION == "Skandinavien" | 
                                    test$MFODLAND_REGION == "Nordeuropa" | 
                                    test$MFODLAND_REGION == "Östeuropa" |
                                    test$MFODLAND_REGION == "Sydeuropa", 1, 0)

test$new_motherOutsideEurope <- ifelse(test$new_motherEuropean!=1,1,0)

# test$new_motherOutsideEurope <- ifelse(test$MFODLAND_REGION == "Asien" |
#                                          test$MFODLAND_REGION == "Latinamerika" |
#                                          test$MFODLAND_REGION == "Nordafrika" |
#                                          test$MFODLAND_REGION == "Subsahariska afrika" |
#                                          test$MFODLAND_REGION == "Sydasien" |
#                                          test$MFODLAND_REGION == "Västasien", 1, 0)

test$new_motherForeign <- ifelse(test$MFODLAND_REGION != "Sverige", 1, 0)

test$new_preterm <- ifelse(test$GRDBS/7<37, 1, 0)
test$new_small <- test$MSGA
test$new_twins <- ifelse(gsub("^.", "", test$BORDNRF2)==2,1,0)


test$new_southern <- ifelse(test$regionMaltreatment == "Södra sjukvårdsregionen", 1, 0)
test$new_southEast <- ifelse(test$regionMaltreatment == "Sydöstra sjukvårdsregionen", 1, 0)
test$new_western <- ifelse(test$regionMaltreatment == "Västra sjukvårdsregionen", 1, 0)
test$new_uppsalaOrebro <- ifelse(test$regionMaltreatment == "Uppsala-Örebro", 1, 0)
test$new_stockholm <- ifelse(test$regionMaltreatment == "Stockholms sjukvårdsregion", 1, 0)
test$new_northern <- ifelse(test$regionMaltreatment == "Norra Sjukvårdsregionen", 1, 0)

#test$new_maltreatment <- ifelse(test$n_maltreatmentSyndrome ==1 & test$diagnosInnanUtskrivning_n_maltreatmentSyndrome != 2, 1, 0)
test$new_maltreatment <- ifelse(test$n_maltreatmentSyndrome == 1 & test$diagnosInnanUtskrivning_n_maltreatmentSyndrome != 2, 1, 0)
test$new_maltreatment <- ifelse(is.na(test$new_maltreatment),0,
                                      ifelse(test$new_maltreatment==1,1,0))

test$new_maltreatment_filter <- ifelse(is.na(test$new_maltreatment),0,
                                      ifelse(test$new_maltreatment==1,1,0))

#table(is.na(test$alderDiagnosManad_n_maltreatmentSyndrome))
test$new_maltreatment_filter <- ifelse(is.na(test$alderDiagnosManad_n_maltreatmentSyndrome),"no maltreatment diagnosis",
                                        ifelse(test$new_maltreatment_filter ==1 & test$alderDiagnosManad_n_maltreatmentSyndrome <4,"diagnosed before 4 months", 
                                               ifelse(test$new_maltreatment_filter ==1 & test$alderDiagnosManad_n_maltreatmentSyndrome >=4,"diagnosed 4+ months", "no maltreatment diagnosis")
                                        )
)

keepVars <- grep("^new_|^BFODDAT", names(test), value = TRUE)

test1 <- test[,keepVars]

test2 <- 
test1 %>% 
  filter(new_maltreatment ==1)


# tmp <- test[,grepl("^n_|^year$", names(test)), with=FALSE]
# 
# 
# # tab1fun <- function(filter){
# filter = "n_maltreatmentSyndrome"
# useFilter <- gsub("^n_", "", filter)
# tmp[,(useFilter):=get(filter),]
# 
# 
# tmp <- tmp[,lapply(.SD, function(x) sum(x)), by = c("year", useFilter)]
# 
# setkeyv(tmp, c("year", useFilter))
# 
# out <- tmp


summary(test2)

test2$year <- as.numeric(substr(test2$BFODDAT,1,4))

#----------------------------- create table ------------------------------------
# reduce the number of rows since we dont need this many

tab2Fun <- function(lowerYear,upperYear, breakYear){
  
  test2$NM_filter <- test2$new_maltreatment_filter
  test2$new_maltreatment_filter <- NULL
  
  periodVars <- grep("^new", names(test2), value = TRUE)
  
  setDT(test2)
  #browser()
  # aggregate by period
  test3 <- test2[,lapply(.SD, sum, na.rm=TRUE), by = c("year","NM_filter"), .SDcols = periodVars]
  
  test3 <- 
    test3 %>% 
    filter(!is.na(year) & year >= lowerYear) %>%   
    mutate(period = ifelse(year > breakYear, paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear))) %>% 
    data.table
  
  test3 <- test3[,lapply(.SD, sum, na.rm=TRUE), by = c("period","NM_filter"), .SDcols = periodVars]
  
  # recast
  test4 <- 
    test3 %>% 
    gather(key, value, -period, -NM_filter) %>% 
    data.table
  
  # calculate and merge on number of infants born each year
  nollAringar<-
    noll0 %>% 
    filter(year >= lowerYear & year <= upperYear) %>% 
    mutate(period = ifelse(year >breakYear , paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear))) %>% 
    data.table
  
  nollAringar <- nollAringar[,list(n = sum(n)),by = period]
  
  setkey(nollAringar, period)
  setkey(test4, period, NM_filter)
  
  test4 <- test4[nollAringar]
  
  # calculate doubles
  sumVars <- c("value", "n")
  
  test4sum <-
    test4[,lapply(.SD, function(x){sum(x, na.rm = TRUE)}),
            .SDcols = sumVars, by = c("key", "period")] %>% 
    mutate(n = n/3,
           NM_filter = " tot")
  
  
  test4 <- plyr::rbind.fill(test4,test4sum)
  
  period1 <- paste0(lowerYear, "_", breakYear)
  period2 <- paste0(breakYear+1, "_", upperYear)
  
  #some are supposed to be hard coded see mail from 29th of march 2016
  test4[test4$key == "new_small" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_small", paste0("X", period1)]
  test4[test4$key == "new_small" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_small", paste0("X", period2)]
  
  test4[test4$key == "new_preterm" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_preterm", paste0("X", period1)]
  test4[test4$key == "new_preterm" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_preterm", paste0("X", period2)]
  
  test4[test4$key == "new_motherForeign" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_motherForeign", paste0("X", period1)]
  test4[test4$key == "new_motherForeign" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_motherForeign", paste0("X", period2)]
  
  test4[test4$key == "new_motherSwedish" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_motherSwedish", paste0("X", period1)]
  test4[test4$key == "new_motherSwedish" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_motherSwedish", paste0("X", period2)]
  
  test4[test4$key == "new_twins" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_twins", paste0("X", period1)]
  test4[test4$key == "new_twins" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_twins", paste0("X", period2)]
  
  test4[test4$key == "new_northern" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_northern", paste0("X", period1)]
  test4[test4$key == "new_northern" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_northern", paste0("X", period2)]
  
  test4[test4$key == "new_uppsalaOrebro" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_uppsalaOrebro", paste0("X", period1)]
  test4[test4$key == "new_uppsalaOrebro" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_uppsalaOrebro", paste0("X", period2)]
  
  test4[test4$key == "new_stockholm" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_stockholm", paste0("X", period1)]
  test4[test4$key == "new_stockholm" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_stockholm", paste0("X", period2)]
  
  test4[test4$key == "new_southEast" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_southEast", paste0("X", period1)]
  test4[test4$key == "new_southEast" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_southEast", paste0("X", period2)]
  
  test4[test4$key == "new_southern" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_southern", paste0("X", period1)]
  test4[test4$key == "new_southern" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_southern", paste0("X", period2)]
  
  test4[test4$key == "new_western" & test4$period == period1 ,"n"] <- namnare[namnare$key == "new_western", paste0("X", period1)]
  test4[test4$key == "new_western" & test4$period == period2 ,"n"] <- namnare[namnare$key == "new_western", paste0("X", period2)]
  
  setDT(test4)
  
  # format table and calculate ci etc
  tab2 <-
    test4[,c("value", "n"), with = FALSE] %>%
    as.matrix %>%
    epi.conf(ctype = "inc.rate", method = "exact") %>%
    cbind(test4,.) %>% 
    data.frame %>% 
    mutate(est = format(round(est*100000,3), nsmall = 3),
           lower = format(round(lower*100000,3), nsmall = 3),
           upper = format(round(upper*100000,3), nsmall = 3)
    ) %>% 
    data.table # %>% 
  #  select(-diagnoser, -korrigering)
  
  setnames(tab2, "value", "val")
  
  
  # calculate relative risk
  mlt <- melt(tab2, id = c("period","key", "NM_filter"))
  tab2 <- dcast(mlt, key+NM_filter~variable+period, value.var = "value") %>%  data.frame
  
  tab2[,grep("key|NM_filter", names(tab2), invert = TRUE, value = TRUE)] = lapply(tab2[,grep("key|NM_filter", names(tab2), invert = TRUE, value = TRUE)], as.numeric)
  #tab2$RR <- (tab2$val_2008_2013/tab2$n_2008_2013)/(tab2$val_1997_2007/tab2$n_1997_2007)
  
  tab2$RR <- (tab2[,paste0("val_",breakYear+1,"_", upperYear)]/tab2[,paste0("n_",breakYear+1,"_", upperYear)])/
    (tab2[,paste0("val_", lowerYear, "_", breakYear)]/tab2[,paste0("n_",lowerYear,"_", breakYear)])
  
  
return(tab2)
}
#-------------------------------- arbetstabeller -------------------------------
metadata <- readWorksheetFromFile("Indata/dataDictionary.xlsx", sheet = "barnmissh") 

metadata <-
metadata %>% 
  filter(variable == "assault" |
        variable == "adverseSocial" |
        variable == "undeterminedCauses")

codeVars <- grep("^kod", names(metadata), value = TRUE)

metadata <- metadata[,c("variable", codeVars)]

metadata <- 
metadata %>% 
  gather(key, value, -variable) %>% 
  filter(!is.na(value)) %>% 
  select(-key) %>% 
  data.table

setkey(metadata, variable)


#test one 


subDiagTabler <- function(diagnosis, time){
  #diagnosis <- "adverseSocial"
  #time <- "'2008_2013'"
  
  codes <- 
  metadata %>%
    filter(variable == diagnosis) %>% 
    select(value)
  
  
  diags <- 
    test %>% 
    filter_(paste0("n_",diagnosis,"==1")) %>%
    mutate(year = as.numeric(substr(BFODDAT,1,4))) %>% 
    filter(year >= 1997) %>% 
    mutate(period = ifelse(year >2007, "2008_2013", "1997_2007")) %>%
    filter_(paste0("period ==", time)) %>% 
    select(AllDiagnosesPar, period, year) 
  
  
  codes$n <- 0
  #rm(tmp)
  for(i in 1:nrow(codes)){
    tmp <- 0
    for(j in 1:nrow(diags)){
       tmp <- tmp + ifelse(grepl(codes$value[i], diags$AllDiagnosesPar[j]),1,0)
    }
    codes$n[i] <- tmp  
  }

  
  setnames(codes, old = "n", new = paste0("n_",gsub("'","",time)))
  codes <- codes[!duplicated(codes)]
  
return(codes)
}

assaultTab <- merge(
  subDiagTabler(diagnosis = "assault", time = "'2008_2013'"),
  subDiagTabler(diagnosis = "assault", time = "'1997_2007'"),
  by = "value"
)


adverseSocialTab <- merge(
  subDiagTabler(diagnosis = "adverseSocial", time = "'2008_2013'"),
  subDiagTabler(diagnosis = "adverseSocial", time = "'1997_2007'"),
  by = "value"
)


undeterminedCausesTab <- merge(
  subDiagTabler(diagnosis = "undeterminedCauses", time = "'2008_2013'"),
  subDiagTabler(diagnosis = "undeterminedCauses", time = "'1997_2007'"),
  by = "value"
)

#--------------------------------- export --------------------------------------
wb <- loadWorkbook("Output/SAP/SAP_01_tabeller.xlsx", create = T)

createSheet(wb, c("Table1_original",
                  "Table2_original",
                  "Table1_1987_2013",
                  "Table1_1991_2013", 
                  "Table1_1997_2013",
                  "Table2_1997_2013"))


tab1 <- tab1Fun(1997, 2013, 2007)
tab2 <-tab2Fun(1997, 2013, 2007)

tab1_1987 <- tab1Fun(1987, 2013, 2006)
tab1_1991 <- tab1Fun(1991, 2013, 2006)
tab1_1997 <- tab1Fun(1997, 2013, 2006)
tab2_1997 <- tab2Fun(1997, 2013, 2006)

writeWorksheet(wb, tab1, "Table1_original")
writeWorksheet(wb, tab2, "Table2_original")

writeWorksheet(wb, tab1_1987, "Table1_1987_2013")
writeWorksheet(wb, tab1_1991, "Table1_1991_2013")
writeWorksheet(wb, tab1_1997, "Table1_1997_2013")
writeWorksheet(wb, tab2_1997, "Table2_1997_2013")

if(UTAN_oppen){
  saveWorkbook(wb,"Output/SAP/SAP_01_tabellerUtanOppen.xlsx")
}else{
  saveWorkbook(wb,"Output/SAP/SAP_01_tabeller.xlsx") 
}

# 
# names(test)
outfileTab2 <- test2[grep("lpnr|year|alderDiagnosManad_|AllDiagnosesPar|^n_|^new", names(test2), value = TRUE)]

write.csv2(outfileTab2, "Output/SAP/SAP_02_tab2.csv", row.names = FALSE, na = "")
saveRDS(outfileTab2, "Output/SAP/SAP_02_tab2.rds")
# write_dta(outfile, "Output/SAP/arbetsfil.dta")
# 
# 
# saveRDS(test[,c("BFODDAT","AllDiagnosesPar")], "testFileDiags.rds")

