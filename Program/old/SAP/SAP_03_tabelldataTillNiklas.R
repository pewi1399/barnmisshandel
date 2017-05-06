library(dplyr)
library(data.table)
library(magrittr)
rm(list = ls())
part1 <- readRDS("Output/SAP/SAP_02_tab1.rds")
#part2 <- readRDS("Output/SAP/SAP_02_tab2.rds")

#-------------------------- format first part of data --------------------------

table(part1$n_maltreatmentSyndrome, part1$year)

# reduce the number of rows since we dont need this many
lowerYear = 1997
upperYear = 2013
breakYear = 2007

part1 <- 
  part1 %>% 
  filter(!is.na(year) & year >= lowerYear) %>%   
  mutate(period = ifelse(year > breakYear, paste0(breakYear+1,"_",upperYear), paste0(lowerYear,"_",breakYear)))

#tes if numbert of diags add up?
table(part1$n_maltreatmentSyndrome, part1$period) # should look like 66, 129 (june 2016)

periodVars <- grep("^diagnosInnan|^n_", names(part1), value = TRUE)

#get before vars
beforeVars <- grep("^diagnosInnan", names(part1), value = TRUE)

#set NA to zero
part1[,beforeVars] <- lapply(part1[,beforeVars], function(x) ifelse(is.na(x),0,x))

#test correct sum
#sum(ifelse(part1$diagnosInnanUtskrivning_n_RakitKalcium==2, 0, part1$n_RakitKalcium))

# run above funtcion on all vars
for(i in beforeVars){ #i <- beforeVars[1]
  ii <- gsub("diagnosInnanUtskrivning_", "", i)
  part1[,ii] <-ifelse(part1[,i]==2, 0, part1[,ii])
}

#test
#table(part1$n_kramper, part1$period) should mirror input in table 1


part1$new_motherSwedish <- ifelse(part1$MFODLAND_REGION == "Sverige", 1, 0)
part1$new_motherEuropean <- ifelse(part1$MFODLAND_REGION == "Sverige" | 
                                    part1$MFODLAND_REGION == "Skandinavien" | 
                                    part1$MFODLAND_REGION == "Nordeuropa" | 
                                    part1$MFODLAND_REGION == "Östeuropa" |
                                    part1$MFODLAND_REGION == "Sydeuropa", 1, 0)

part1$new_motherOutsideEurope <- ifelse(part1$new_motherEuropean!=1,1,0)

# part1$new_motherOutsideEurope <- ifelse(part1$MFODLAND_REGION == "Asien" |
#                                          part1$MFODLAND_REGION == "Latinamerika" |
#                                          part1$MFODLAND_REGION == "Nordafrika" |
#                                          part1$MFODLAND_REGION == "Subsahariska afrika" |
#                                          part1$MFODLAND_REGION == "Sydasien" |
#                                          part1$MFODLAND_REGION == "Västasien", 1, 0)

part1$new_motherForeign <- ifelse(part1$MFODLAND_REGION != "Sverige", 1, 0)

part1$new_preterm <- ifelse(part1$GRDBS/7<37, 1, 0)
part1$new_small <- part1$MSGA
part1$new_twins <- ifelse(gsub("^.", "", part1$BORDNRF2)==2,1,0)

#part1$new_maltreatment <- ifelse(part1$n_maltreatmentSyndrome ==1 & part1$diagnosInnanUtskrivning_n_maltreatmentSyndrome != 2, 1, 0)
part1$new_maltreatment <- ifelse(part1$n_maltreatmentSyndrome == 1 & part1$diagnosInnanUtskrivning_n_maltreatmentSyndrome != 2, 1, 0)
part1$new_maltreatment <- ifelse(is.na(part1$new_maltreatment),0,
                                ifelse(part1$new_maltreatment==1,1,0))

part1$new_southern <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Södra sjukvårdsregionen", 1, 0)
part1$new_southEast <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Sydöstra sjukvårdsregionen", 1, 0)
part1$new_western <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Västra sjukvårdsregionen", 1, 0)
part1$new_uppsalaOrebro <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Uppsala-Örebro", 1, 0)
part1$new_stockholm <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Stockholms sjukvårdsregion", 1, 0)
part1$new_northern <- ifelse(part1$new_maltreatment == 1 & part1$regionMaltreatment == "Norra Sjukvårdsregionen", 1, 0)


# vars to extract
extract <- c("lpnr","year", "period" ,grep("^new_|^n_", names(part1), value = TRUE))

# niclas data
out <- part1[,extract]

# assume if no mark not twins
out$new_twins[is.na(out$new_twins)] <- 0


out$lpnr <- ""


write.csv2(out, "Output/SAP/pathData20160606.csv", na = "", row.names = FALSE)


